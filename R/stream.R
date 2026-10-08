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
                           noncovered_pension = 0, people = NULL, policy = current_law(),
                           chunk_size = 250000) {
  e <- as_earnings(earnings, first_year)
  fp <- fill_from_people(people, names(match.call())[-1],
                         c("birth_year", "birth_month", "birth_day", "claim_age", "to_age",
                           "noncovered_pension"),
                         environment(), e$ids)
  ids <- if (is.null(fp$ids)) e$ids else fp$ids
  m <- expand_rows(take_rows(e$m, fp$rows), birth_year, birth_month, birth_day, claim_age,
                   to_age, noncovered_pension)
  n <- nrow(m)
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  claim <- rows_of(claim_age, "claim_age", n)
  last <- rows_of(to_age, "to_age", n)
  ncp <- rows_of(noncovered_pension, "noncovered_pension", n)
  if (length(step) != 1 || !is.numeric(step) || is.na(step) || step < 1 || step %% 1 != 0) {
    abort_field("step", "must be one whole number of months, 1 or more")
  }
  if (length(chunk_size) != 1 || !is.numeric(chunk_size) || is.na(chunk_size) ||
      chunk_size < 1) {
    abort_field("chunk_size", "must be one number, 1 or more")
  }
  check("to_age", last < claim, "before claim_age")

  # one row per worker and benefit month, workers in input order
  count <- (last - claim) %/% step + 1
  w <- rep(seq_len(n), times = count)
  ben_age <- claim[w] + sequence(count, from = 0, by = step)

  pieces <- list()
  starts <- seq(1, length(w), by = chunk_size)
  for (s in starts) {
    r <- seq(s, min(s + chunk_size - 1, length(w)))
    wr <- w[r]
    pieces[[length(pieces) + 1]] <- retired_worker(
      m[wr, , drop = FALSE], by[wr], bm[wr], claim[wr], first_year = e$first,
      birth_day = bd[wr], benefit_age = ben_age[r], noncovered_pension = ncp[wr],
      policy = policy
    )
  }
  out <- do.call(rbind, pieces)

  kb <- adjusted_birth(by[w], bm[w], bd[w])
  ben <- from_month_index(month_index(kb$year, kb$month) + ben_age)
  front <- tibble::tibble(claim_age = claim[w], benefit_age = ben_age,
                          benefit_year = ben$year, benefit_month = ben$month)
  front <- if (is.null(ids)) tibble::add_column(front, worker = w, .before = 1) else
    tibble::add_column(front, id = ids[w], .before = 1)
  cols <- c("aime", "pia", "mfb", "factor", "benefit", "method", "insured")
  tibble::as_tibble(c(front, out[cols]))
}
