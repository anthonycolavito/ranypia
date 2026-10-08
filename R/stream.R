# Benefit streams: a retired worker's benefit in each month (or each year)
# from entitlement to a final age, built on retired_worker().

#' Retired-worker benefit streams
#'
#' The monthly benefit a retired worker is paid from the claim month through
#' `to_age`, every `step` months. Each row is exactly what
#' [retired_worker()] gives with that `benefit_age`: the PIA carried forward
#' by COLAs, the claiming factor, SSA's rounding, the special minimum, and
#' recomputation for any earnings after entitlement (earnings in and after a
#' benefit month's year are ignored for that month). There is no earnings
#' test, mortality or discounting; `to_age` can differ by worker, so a death
#' age from a microsimulation ends each stream.
#'
#' The default `step = 12` samples the anniversary of the claim month. COLAs
#' take effect with December's benefit, so twelve times that month is not
#' quite a calendar year's total; use `step = 1` and sum by `benefit_year`
#' for exact annual amounts (see the examples).
#'
#' @inheritParams retired_worker
#' @param claim_age Age at entitlement, in months.
#' @param to_age Age in the last benefit month, in months (default 100 years,
#'   1200). One value or one per worker; streams stop at the last step at
#'   or before it.
#' @param step Months between rows: 12 (default) for one row a year, 1 for
#'   every month.
#' @param chunk_size Rows computed per call to [retired_worker()]; lower it
#'   if memory is tight. It does not change results.
#' @return A tibble with one row per worker and benefit month: `id` when
#'   known (otherwise `worker`, the input row), `claim_age`, `benefit_age`
#'   (months), `benefit_year`, `benefit_month`, then `aime`, `pia`, `mfb`,
#'   `factor`, `benefit` (whole dollars), `method` and `insured` as in
#'   [retired_worker()].
#' @export
#' @examples
#' earnings <- setNames(current_law()$awi[(1986:2030) - 1936], 1986:2030)
#'
#' # one row a year from claiming at 67 to 100
#' s <- benefit_stream(earnings, 1964, 6, claim_age = 67 * 12)
#' head(s)
#'
#' # every month, summed to calendar-year totals
#' m <- benefit_stream(earnings, 1964, 6, claim_age = 67 * 12, step = 1)
#' annual <- tapply(m$benefit, m$benefit_year, sum)
#' head(annual)
benefit_stream <- function(earnings, birth_year, birth_month, claim_age, to_age = 1200,
                           step = 12, first_year = NULL, birth_day = 15,
                           noncovered_pension = 0, qc_history = NULL, people = NULL,
                           policy = current_law(), chunk_size = 250000) {
  e <- as_earnings(earnings, first_year)
  fp <- fill_from_people(people, names(match.call())[-1],
                         c("birth_year", "birth_month", "birth_day", "claim_age", "to_age",
                           "noncovered_pension"),
                         environment(), e$ids)
  ids <- if (is.null(fp$ids)) e$ids else fp$ids
  m <- expand_rows(take_rows(e$m, fp$rows), birth_year, birth_month, birth_day, claim_age,
                   to_age, noncovered_pension)
  n <- nrow(m)
  qh <- fit_qc_rows(qc_history_matrix(qc_history, e), fp$rows, n)
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  claim <- rows_of(claim_age, "claim_age", n)
  last <- rows_of(to_age, "to_age", n)
  ncp <- rows_of(noncovered_pension, "noncovered_pension", n)
  check_stream_args(step, chunk_size)
  check("to_age", last < claim, "before claim_age")

  # one row per worker and benefit month, workers in input order
  count <- (last - claim) %/% step + 1
  w <- rep(seq_len(n), times = count)
  ben_age <- claim[w] + sequence(count, from = 0, by = step)

  pieces <- lapply(chunks(length(w), chunk_size), function(r) {
    wr <- w[r]
    retired_worker(
      m[wr, , drop = FALSE], by[wr], bm[wr], claim[wr], first_year = e$first,
      birth_day = bd[wr], benefit_age = ben_age[r], noncovered_pension = ncp[wr],
      qc_history = if (!is.null(qh)) qh[wr, , drop = FALSE], policy = policy
    )
  })
  out <- do.call(rbind, pieces)

  kb <- adjusted_birth(by[w], bm[w], bd[w])
  ben <- from_month_index(month_index(kb$year, kb$month) + ben_age)
  front <- stream_front(ids, w, claim_age = claim[w], benefit_age = ben_age,
                        benefit_year = ben$year, benefit_month = ben$month)
  tibble::as_tibble(c(front, out[STREAM_COLS]))
}

STREAM_COLS <- c("aime", "pia", "mfb", "factor", "benefit", "method", "insured")

check_stream_args <- function(step, chunk_size) {
  if (length(step) != 1 || !is.numeric(step) || is.na(step) || step < 1 || step %% 1 != 0) {
    abort_field("step", "must be one whole number of months, 1 or more")
  }
  if (length(chunk_size) != 1 || !is.numeric(chunk_size) || is.na(chunk_size) ||
      chunk_size < 1) {
    abort_field("chunk_size", "must be one number, 1 or more")
  }
}

# The leading columns of a stream: `id` when known, otherwise `worker` (the
# input row), then the columns given.
stream_front <- function(ids, w, ...) {
  front <- tibble::tibble(...)
  if (is.null(ids)) tibble::add_column(front, worker = w, .before = 1) else
    tibble::add_column(front, id = ids[w], .before = 1)
}

# Row chunks of 1..total, each at most chunk_size long.
chunks <- function(total, chunk_size) {
  starts <- seq(1, total, by = chunk_size)
  lapply(starts, function(s) seq(s, min(s + chunk_size - 1, total)))
}

#' Disabled-worker benefit streams
#'
#' A disabled worker's monthly benefit from entitlement through `to_age`,
#' every `step` months. Each row is exactly what [disabled_worker()] gives
#' for that benefit month. At normal retirement age the disability benefit
#' converts to a retirement benefit on the same PIA, unreduced; `type` says
#' which it is (`"disabled"` before NRA, `"retired"` from NRA on), and the
#' amount carries on unchanged apart from COLAs.
#'
#' Entitlement defaults, as in [disabled_worker()], to the end of the
#' five-month waiting period, which starts with the first full month of
#' disability. There is no recovery, earnings test, mortality or
#' discounting; end a stream early with a per-worker `to_age` (a simulated
#' age at death, say).
#'
#' @inheritParams disabled_worker
#' @param entitlement Optional `list(year, month)` of entitlement, one per
#'   worker.
#' @param to_age Age in the last benefit month, in months (default 100 years,
#'   1200). One value or one per worker.
#' @param step Months between rows: 12 (default) for one row a year from the
#'   entitlement month, 1 for every month.
#' @param chunk_size Rows computed per call to [disabled_worker()]; it does not
#'   change results.
#' @return A tibble with one row per worker and benefit month: `id` when
#'   known (otherwise `worker`, the input row), `entitlement_age`,
#'   `benefit_age` (months), `benefit_year`, `benefit_month`, `type`, then
#'   `aime`, `pia`, `mfb`, `factor`, `benefit` (whole dollars), `method` and
#'   `insured` as in [disabled_worker()].
#' @export
#' @examples
#' e <- setNames(current_law()$awi[(1990:2020) - 1936], 1990:2020)
#' s <- disabled_stream(e, 1970, 3, onset_year = 2021, onset_month = 6)
#' head(s)
#' s[s$type == "retired", ][1, ]  # the conversion at NRA
disabled_stream <- function(earnings, birth_year, birth_month, onset_year, onset_month,
                            to_age = 1200, step = 12, first_year = NULL, birth_day = 15,
                            onset_day = 15, entitlement = NULL, childcare = NULL,
                            qc_history = NULL, people = NULL, policy = current_law(),
                            chunk_size = 250000) {
  e <- as_earnings(earnings, first_year)
  fp <- fill_from_people(people, names(match.call())[-1],
                         c("birth_year", "birth_month", "birth_day", "onset_year",
                           "onset_month", "onset_day", "to_age"),
                         environment(), e$ids)
  ids <- if (is.null(fp$ids)) e$ids else fp$ids
  if (!is.null(fp$rows) && is.matrix(childcare) && nrow(childcare) == nrow(e$m)) {
    childcare <- take_rows(childcare, fp$rows)
  }
  m <- expand_rows(take_rows(e$m, fp$rows), birth_year, birth_month, birth_day, onset_year,
                   onset_month, onset_day, to_age, entitlement[[1]])
  n <- nrow(m)
  qh <- fit_qc_rows(qc_history_matrix(qc_history, e), fp$rows, n)
  if (is.matrix(childcare) && nrow(childcare) == 1 && n > 1) {
    childcare <- childcare[rep(1, n), , drop = FALSE]
  }
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  oy <- rows_of(onset_year, "onset_year", n)
  om <- rows_of(onset_month, "onset_month", n)
  od <- rows_of(onset_day, "onset_day", n)
  last <- rows_of(to_age, "to_age", n)
  check_stream_args(step, chunk_size)

  if (is.null(entitlement)) {
    ent_idx <- month_index(oy, om) + ifelse(od == 1, 0, 1) + 5
  } else {
    ent_idx <- month_index(rows_of(entitlement[[1]], "entitlement_year", n),
                           rows_of(entitlement[[2]], "entitlement_month", n))
  }
  kb <- adjusted_birth(by, bm, bd)
  k <- month_index(kb$year, kb$month)
  ent_age <- ent_idx - k
  nra <- normal_retirement_age(by, bm, bd, policy)
  check("entitlement", ent_age >= nra, "at or after normal retirement age")
  check("to_age", last < ent_age, "before the entitlement age")

  # one row per worker and benefit month, workers in input order
  count <- (last - ent_age) %/% step + 1
  w <- rep(seq_len(n), times = count)
  ben_idx <- ent_idx[w] + sequence(count, from = 0, by = step)
  ent <- from_month_index(ent_idx)
  ben <- from_month_index(ben_idx)

  pieces <- lapply(chunks(length(w), chunk_size), function(r) {
    wr <- w[r]
    cc <- if (is.matrix(childcare)) childcare[wr, , drop = FALSE] else childcare
    disabled_worker(m[wr, , drop = FALSE], by[wr], bm[wr], oy[wr], om[wr],
                    first_year = e$first, birth_day = bd[wr], onset_day = od[wr],
                    entitlement = list(ent$year[wr], ent$month[wr]),
                    benefit = list(ben$year[r], ben$month[r]), childcare = cc,
                    qc_history = if (!is.null(qh)) qh[wr, , drop = FALSE], policy = policy)
  })
  out <- do.call(rbind, pieces)

  ben_age <- ben_idx - k[w]
  front <- stream_front(ids, w, entitlement_age = ent_age[w], benefit_age = ben_age,
                        benefit_year = ben$year, benefit_month = ben$month,
                        type = ifelse(ben_age < nra[w], "disabled", "retired"))
  tibble::as_tibble(c(front, out[STREAM_COLS]))
}
