# One-call benefit computations that chain the formula functions (ported
# from pyanypia's convenience.py).

# Fills the calling function's missing arguments from columns of `people`,
# lining people up with the earnings rows when both carry ids.
fill_from_people <- function(people, supplied, names, env, ids) {
  if (is.null(people)) return(NULL)
  if (!is.data.frame(people)) abort_field("people", "must be a data frame")
  if (!is.null(ids) && "id" %in% names(people)) {
    rows <- match(ids, people$id)
    if (anyNA(rows)) {
      abort_field("people", sprintf("has no row for earnings id %s", ids[is.na(rows)][1]))
    }
    people <- people[rows, , drop = FALSE]
  }
  for (nm in intersect(names, names(people))) {
    if (!nm %in% supplied) assign(nm, people[[nm]], envir = env)
  }
  if ("id" %in% names(people)) people$id else NULL
}

worker_tibble <- function(ids, ...) {
  cols <- list(...)
  out <- tibble::as_tibble(cols)
  if (!is.null(ids)) out <- tibble::add_column(out, id = ids, .before = 1)
  out
}

special_minimum_at <- function(m, first, last_year, ben_y, ben_m, policy) {
  yoc <- years_of_coverage(m, last_year, first_year = first, policy = policy)
  special_minimum_pia(yoc, ben_y, ben_m, policy)
}

apply_wep_if_enabled <- function(pia_elig, aime_v, elig, m, first, last_year, ben_y, pension,
                                 policy) {
  pen <- recycle_to(as_num("noncovered_pension", pension), "noncovered_pension",
                    length(pia_elig))
  if (!isTRUE(policy$wep_enabled) || !any(pen > 0)) return(pia_elig)
  yoc <- years_of_coverage(m, last_year, first_year = first, kind = "wep", policy = policy)
  wep_pia(aime_v, elig, yoc, pen, ben_y, policy)
}

#' Retired worker's benefit
#'
#' The whole chain for a retired worker, for the month at `benefit_age`
#' (default: the entitlement month, `claim_age`). Ages are months from the
#' adjusted birth month ([adjusted_birth()]): a worker born 15 March 1964 is
#' 744 months old in March 2026. Earnings in and after the benefit year are
#' ignored. The higher of the wage-indexed and special-minimum PIAs is used;
#' delayed credits never apply to a special minimum, and start only once the
#' worker is fully insured.
#'
#' A worker who is not fully insured is entitled to nothing; `insured`
#' flags that, and the amounts are what AnyPIA reports regardless.
#'
#' @param earnings A matrix (rows = workers), a named vector, or a long data
#'   frame with `id`, `year`, `earnings`.
#' @inheritParams eligibility_year
#' @param claim_age Age at entitlement, in months.
#' @param benefit_age Age in the benefit month (default `claim_age`).
#' @param first_year First year of a matrix without year column names.
#' @param noncovered_pension Monthly noncovered pension; matters only when
#'   `policy$wep_enabled`.
#' @param people Optional data frame supplying any of the per-worker
#'   arguments as columns (explicit arguments win). With an `id` column and
#'   long earnings, rows are matched by id.
#' @param policy A [current_law()] policy.
#' @return A tibble with one row per worker: `elig_year`, `aime`, `pia_elig`
#'   (wage-indexed PIA at eligibility), `pia` and `mfb` (in the benefit
#'   month), `nra`, `factor`, `benefit` (whole dollars), `method`
#'   (`"wage_indexed"` or `"special_minimum"`) and `insured`; plus `id` when
#'   known.
#' @export
#' @examples
#' earnings <- setNames(rep(60000, 40), 1990:2029)
#' retired_worker(earnings, 1964, 6, c(62 * 12 + 1, 67 * 12, 70 * 12))
retired_worker <- function(earnings, birth_year, birth_month, claim_age, first_year = NULL,
                           birth_day = 15, benefit_age = NULL, noncovered_pension = 0,
                           people = NULL, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  ids <- fill_from_people(people, names(match.call())[-1],
                          c("birth_year", "birth_month", "birth_day", "claim_age",
                            "benefit_age", "noncovered_pension"),
                          environment(), e$ids)
  if (is.null(ids)) ids <- e$ids
  m <- e$m
  first <- e$first
  n <- nrow(m)
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  claim <- rows_of(claim_age, "claim_age", n)
  ben_age <- if (is.null(benefit_age)) claim else rows_of(benefit_age, "benefit_age", n)
  kb <- adjusted_birth(by, bm, bd)
  k <- month_index(kb$year, kb$month)
  ben <- from_month_index(k + ben_age)
  last_year <- ben$year - 1
  elig <- kb$year + 62

  comp <- computation_years(by, bm, elig, bd)
  aime_v <- aime(m, elig, comp, first_year = first, last_year = last_year, policy = policy)
  pia_elig <- pia(aime_v, elig, policy)
  pia_elig <- apply_wep_if_enabled(pia_elig, aime_v, elig, m, first, last_year, ben$year,
                                   noncovered_pension, policy)
  mfb_elig <- family_max(pia_elig, elig, policy)
  wage_pia <- apply_colas(pia_elig, elig, ben$year, ben$month, policy)
  wage_mfb <- apply_colas(mfb_elig, elig, ben$year, ben$month, policy)
  sm <- special_minimum_at(m, first, last_year, ben$year, ben$month, policy)

  nra <- normal_retirement_age(by, bm, bd, policy)
  ent <- k + claim
  qc <- qcs(m, first, policy)
  no_freeze <- rep(NO_FREEZE, n)
  insured <- fully_insured_at(m, qc, first, kb$year, ent %/% 3, no_freeze)
  # Delayed credits run from the first quarter, from the one NRA falls in,
  # in which the worker is fully insured (PiaCal::fullInsDateCal); never,
  # if that is not before the benefit month.
  ben_idx <- k + ben_age
  fra_q <- (k + nra) %/% 3
  credits_from <- rep(.Machine$integer.max * 4, n)
  for (j in seq(0, max(c(ben_idx %/% 3 - fra_q, 0)))) {
    q <- fra_q + j
    start <- 3 * q
    searching <- credits_from > ben_idx & (j == 0 | start < ben_idx)
    hit <- searching & fully_insured_at(m, qc, first, kb$year, q, no_freeze)
    credits_from <- ifelse(hit, start, credits_from)
  }
  factor <- benefit_factor(by, bm, claim, bd, ben_age, policy, credits_from)
  cy <- cola_year(ben$year, ben$month)
  # The higher PIA wins (ties to the wage-indexed method). Delayed credits
  # never apply to a special minimum, so a worker past NRA gets the larger of
  # the credited wage-indexed benefit and the uncredited special minimum.
  sm_wins <- sm$pia > wage_pia
  pia_v <- ifelse(sm_wins, sm$pia, wage_pia)
  mfb_v <- ifelse(sm_wins, sm$mfb, wage_mfb)
  support <- sm_wins & claim > nra
  unrounded <- ifelse(support, pmax(round_benefit(factor * wage_pia, cy), sm$pia),
                      round_benefit(factor * pia_v, cy))
  worker_tibble(ids, elig_year = elig, aime = aime_v, pia_elig = pia_elig, pia = pia_v,
                mfb = mfb_v, nra = nra, factor = factor, benefit = floor_dollar(unrounded),
                method = ifelse(sm_wins, "special_minimum", "wage_indexed"),
                insured = insured)
}
