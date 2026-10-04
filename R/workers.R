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

#' Disabled worker's benefit
#'
#' A disabled worker's benefit, with no prior retirement benefit.
#' `entitlement` and `benefit` are `list(year, month)`; entitlement defaults
#' to the end of the five-month waiting period, which starts with the first
#' full month of disability, and the benefit month to entitlement. Earnings
#' after the onset year fall in the disability freeze and are ignored.
#'
#' Three computations compete, as in AnyPIA, and the highest PIA wins: the
#' ordinary one; the child-care dropout one (when `childcare` is given); and
#' the "non-freeze" one, which takes the waiting period's first year as the
#' eligibility year and counts every year's earnings through the year before
#' the benefit month, if the worker is insured on that basis too. `insured`
#' is disability insured status ([disability_insured()]).
#'
#' @inheritParams retired_worker
#' @param onset_year,onset_month,onset_day Date of disability onset.
#' @param entitlement,benefit Optional `list(year, month)`.
#' @param childcare Optional logical matrix shaped like the earnings: years
#'   with a child under 3 in care.
#' @return A tibble like [retired_worker()]'s.
#' @export
disabled_worker <- function(earnings, birth_year, birth_month, onset_year, onset_month,
                            first_year = NULL, birth_day = 15, onset_day = 15,
                            entitlement = NULL, benefit = NULL, childcare = NULL,
                            people = NULL, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  ids <- fill_from_people(people, names(match.call())[-1],
                          c("birth_year", "birth_month", "birth_day", "onset_year",
                            "onset_month", "onset_day"), environment(), e$ids)
  if (is.null(ids)) ids <- e$ids
  m <- e$m
  first <- e$first
  n <- nrow(m)
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  oy <- rows_of(onset_year, "onset_year", n)
  om <- rows_of(onset_month, "onset_month", n)
  od <- rows_of(onset_day, "onset_day", n)
  if (is.null(entitlement)) {
    waiting <- month_index(oy, om) + ifelse(od == 1, 0, 1)
    ent_idx <- waiting + 5
  } else {
    ent_idx <- month_index(rows_of(entitlement[[1]], "entitlement_year", n),
                           rows_of(entitlement[[2]], "entitlement_month", n))
    waiting <- ent_idx - 5
  }
  ben_idx <- if (is.null(benefit)) ent_idx else
    month_index(rows_of(benefit[[1]], "benefit_year", n),
                rows_of(benefit[[2]], "benefit_month", n))
  check("entitlement", ent_idx < month_index(1980, 7), "before July 1980")
  check("benefit", ben_idx < ent_idx, "before entitlement")
  ben <- from_month_index(ben_idx)
  kb <- adjusted_birth(by, bm, bd)
  through <- ben$year - 1

  wage_indexed <- function(elig, last) {
    comp <- computation_years(by, bm, elig, bd, disabled = TRUE)
    a <- aime(m, elig, comp, first_year = first, last_year = last, policy = policy)
    list(elig = elig, comp = comp, aime = a, pia = pia(a, elig, policy))
  }
  # the ordinary computation: earnings after onset fall in the freeze
  elig <- pmin(kb$year + 62, oy)
  jan1 <- om == 1 & od == 1
  last_year <- pmin(through, ifelse(jan1, oy - 1, oy))
  ordinary <- wage_indexed(elig, last_year)
  methods <- list(ordinary)
  if (!is.null(childcare)) {
    elapsed <- elapsed_years(by, bm, elig, bd)
    cc_aime <- childcare_aime(m, elig, ordinary$comp, elapsed - ordinary$comp, childcare,
                              first_year = first, last_year = last_year,
                              through_year = through, policy = policy)
    methods[[length(methods) + 1]] <- list(elig = elig, aime = cc_aime,
                                           pia = pia(cc_aime, elig, policy))
  }
  # the non-freeze computation applies only if insured without the freeze
  elig_nf <- pmin(kb$year + 62, waiting %/% 12)
  nf <- wage_indexed(elig_nf, through)
  qc <- qcs(m, first, policy)
  age21 <- age21_quarter(kb$year, kb$month)
  wait_q <- waiting %/% 3
  ent_q <- ent_idx %/% 3
  regular <- twenty_of_forty(qc, first, wait_q, age21, FALSE)
  young <- twenty_of_forty(qc, first, wait_q, age21, TRUE)
  nf_ok <- (regular$ok | (regular$start < age21 & young$ok)) &
    fully_insured_at(m, qc, first, kb$year, ent_q, waiting %/% 12)
  methods[[length(methods) + 1]] <- list(elig = elig_nf, aime = ifelse(nf_ok, nf$aime, 0),
                                         pia = ifelse(nf_ok, nf$pia, 0))
  insured <- disability_insured_at(m, qc, first, kb$year, kb$month, month_index(oy, om) %/% 3,
                                   wait_q, ent_q, oy)

  pias <- lapply(methods, function(x) apply_colas(x$pia, x$elig, ben$year, ben$month, policy))
  yoc <- years_of_coverage(m, through, first_year = first, policy = policy)
  sm_pia <- special_minimum_pia(yoc, ben$year, ben$month, policy)$pia
  # the highest PIA wins, ties to the earlier method; the special minimum
  # ranks after the ordinary computation and before the others
  high <- pias[[1]]
  winner <- rep(1, n)
  sm_wins <- sm_pia > high
  high <- ifelse(sm_wins, sm_pia, high)
  for (j in seq_along(methods)[-1]) {
    better <- pias[[j]] > high
    high <- ifelse(better, pias[[j]], high)
    winner <- ifelse(better, j, winner)
    sm_wins <- sm_wins & !better
  }
  # each method's DI maximum, never below the highest PIA; a special-minimum
  # winner takes the one of the method with the highest AIME
  mfbs <- lapply(methods, function(x) {
    pmax(apply_colas(di_family_max(x$pia, x$aime, x$elig, policy), x$elig, ben$year,
                     ben$month, policy), high)
  })
  top_aime <- methods[[1]]$aime
  top <- rep(1, n)
  for (j in seq_along(methods)[-1]) {
    higher <- methods[[j]]$aime > top_aime
    top_aime <- ifelse(higher, methods[[j]]$aime, top_aime)
    top <- ifelse(higher, j, top)
  }
  pick <- function(vals, idx) do.call(cbind, vals)[cbind(seq_len(n), idx)]
  mfb_v <- ifelse(sm_wins, pick(mfbs, top), pick(mfbs, winner))
  nra <- normal_retirement_age(by, bm, bd, policy)
  unrounded <- round_benefit(1 * high, cola_year(ben$year, ben$month))
  worker_tibble(ids, elig_year = pick(lapply(methods, `[[`, "elig"), winner),
                aime = pick(lapply(methods, `[[`, "aime"), winner),
                pia_elig = pick(lapply(methods, `[[`, "pia"), winner), pia = high, mfb = mfb_v,
                nra = nra, factor = rep(1, n), benefit = floor_dollar(unrounded),
                method = ifelse(sm_wins, "special_minimum", "wage_indexed"), insured = insured)
}

#' Deceased worker's PIA and family maximum
#'
#' A deceased worker's PIA and family maximum in a survivor benefit month
#' (`benefit` is `list(year, month)`), for
#' `family_benefits(..., survivor = TRUE)`. Earnings through the year of
#' death count. `factor` is 1 and `benefit` 0: the worker is paid nothing.
#' `insured` is not evaluated (always `TRUE`).
#'
#' @inheritParams retired_worker
#' @param death_year Year of death.
#' @param benefit `list(year, month)` of the survivor benefit month.
#' @return A tibble like [retired_worker()]'s.
#' @export
deceased_worker <- function(earnings, birth_year, birth_month, death_year, benefit,
                            first_year = NULL, birth_day = 15, people = NULL,
                            policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  ids <- fill_from_people(people, names(match.call())[-1],
                          c("birth_year", "birth_month", "birth_day", "death_year"),
                          environment(), e$ids)
  if (is.null(ids)) ids <- e$ids
  m <- e$m
  first <- e$first
  n <- nrow(m)
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  dy <- rows_of(death_year, "death_year", n)
  ben_y <- rows_of(benefit[[1]], "benefit_year", n)
  ben_m <- rows_of(benefit[[2]], "benefit_month", n)
  ky <- adjusted_birth(by, bm, bd)$year
  elig <- pmin(ky + 62, dy)
  comp <- computation_years(by, bm, elig, bd, death_year = dy)
  aime_v <- aime(m, elig, comp, first_year = first, last_year = dy, policy = policy)
  pia_elig <- pia(aime_v, elig, policy)
  mfb_elig <- family_max(pia_elig, elig, policy)
  wage_pia <- apply_colas(pia_elig, elig, ben_y, ben_m, policy)
  wage_mfb <- apply_colas(mfb_elig, elig, ben_y, ben_m, policy)
  sm <- special_minimum_at(m, first, dy, ben_y, ben_m, policy)
  sm_wins <- sm$pia > wage_pia
  worker_tibble(ids, elig_year = elig, aime = aime_v, pia_elig = pia_elig,
                pia = ifelse(sm_wins, sm$pia, wage_pia),
                mfb = ifelse(sm_wins, sm$mfb, wage_mfb),
                nra = normal_retirement_age(by, bm, bd, policy), factor = rep(1, n),
                benefit = rep(0, n),
                method = ifelse(sm_wins, "special_minimum", "wage_indexed"),
                insured = rep(TRUE, n))
}

#' Re-indexed widow(er)'s guarantee
#'
#' For a worker who dies before 62, a widow(er)'s benefit can instead be
#' based on the worker's earnings indexed to the year the widow(er) turns 60
#' (for a disabled widow(er), the later of onset and turning 50), but no
#' later than the year the worker would have turned 62. Returns that PIA in
#' the benefit month, or 0 where the guarantee does not apply; pass it as
#' `auxiliary(guarantee_pia = )`.
#'
#' @inheritParams deceased_worker
#' @param death_month,death_day Month and day of death.
#' @param widow_birth_year,widow_birth_month,widow_birth_day The widow(er)'s
#'   date of birth.
#' @param disabled_onset_year,entitlement_year For a disabled widow(er): the
#'   year of onset and of entitlement.
#' @return Guarantee PIAs.
#' @export
widow_guarantee_pia <- function(earnings, birth_year, birth_month, death_year, death_month,
                                widow_birth_year, widow_birth_month, benefit,
                                widow_birth_day = 15, disabled_onset_year = NULL,
                                entitlement_year = NULL, first_year = NULL, birth_day = 15,
                                death_day = 15, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  m <- e$m
  first <- e$first
  n <- nrow(m)
  by <- rows_of(birth_year, "birth_year", n)
  bm <- rows_of(birth_month, "birth_month", n)
  bd <- rows_of(birth_day, "birth_day", n)
  dy <- rows_of(death_year, "death_year", n)
  dm <- rows_of(death_month, "death_month", n)
  dd <- rows_of(death_day, "death_day", n)
  ben_y <- rows_of(benefit[[1]], "benefit_year", n)
  ben_m <- rows_of(benefit[[2]], "benefit_month", n)
  kb <- adjusted_birth(by, bm, bd)
  wky <- recycle_to(adjusted_birth(widow_birth_year, widow_birth_month,
                                   widow_birth_day)$year, "widow_birth_year", n)
  if (is.null(disabled_onset_year)) {
    widow_elig <- wky + 60
    test_year <- widow_elig
  } else {
    if (is.null(entitlement_year)) {
      abort_field("entitlement_year", "required for a disabled widow(er)")
    }
    widow_elig <- pmax(rows_of(disabled_onset_year, "disabled_onset_year", n), wky + 50)
    test_year <- rows_of(entitlement_year, "entitlement_year", n)
  }
  # death before the day before the 62nd birthday (a birth on the 1st makes
  # that the last day of the previous month)
  kday <- ifelse(bd == 1, 31, bd - 1)
  before62 <- (dy * 10000 + dm * 100 + dd) < ((kb$year + 62) * 10000 + kb$month * 100 + kday)
  elig_worker <- pmin(kb$year + 62, dy)
  applies <- before62 & elig_worker > 1978 & (test_year > 1984 | dy >= 1985)
  elig <- pmin(pmax(elig_worker, widow_elig), kb$year + 62)
  comp <- computation_years(by, bm, elig_worker, bd, death_year = dy)
  aime_v <- aime(m, elig, comp, first_year = first, last_year = dy, policy = policy)
  pia_v <- apply_colas(pia(aime_v, elig, policy), elig, ben_y, ben_m, policy)
  ifelse(applies, pia_v, 0)
}
