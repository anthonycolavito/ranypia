# Retirement ages and the reduction or credit for claiming early or late
# (ported from pyanypia's claiming.py). Ages are whole months from the
# adjusted birth month.

#' Eligibility year
#'
#' The year of attaining 62, or of disability onset or death if earlier.
#'
#' @param birth_year,birth_month,birth_day Date of birth.
#' @param onset_year,death_year Optional year of disability onset or death.
#' @return Eligibility years.
#' @export
#' @examples
#' eligibility_year(1960, 6)
eligibility_year <- function(birth_year, birth_month, birth_day = 15, onset_year = NULL,
                             death_year = NULL) {
  e <- adjusted_birth(birth_year, birth_month, birth_day)$year + 62
  if (!is.null(death_year)) e <- pmin(e, as_whole("death_year", death_year))
  if (!is.null(onset_year)) e <- pmin(e, as_whole("onset_year", onset_year))
  e
}

#' Normal retirement age
#'
#' @inheritParams eligibility_year
#' @param policy A [current_law()] policy.
#' @return The normal (full) retirement age in months.
#' @export
#' @examples
#' normal_retirement_age(1960, 6) / 12
normal_retirement_age <- function(birth_year, birth_month, birth_day = 15,
                                  policy = current_law()) {
  at(policy$nra, adjusted_birth(birth_year, birth_month, birth_day)$year + 62, "nra")
}

#' Earliest retirement claim age
#'
#' In months from the adjusted birth month: 62 and 1 month, except 62 for
#' those born on the 2nd, who are 62 for the whole of their birthday month.
#' Someone born on the 1st is also 62 all month, but their adjusted birth
#' month is the month before, so they too count 62 and 1 month from it.
#'
#' @param birth_day Day of birth.
#' @return Months.
#' @export
earliest_claim_age <- function(birth_day = 15) {
  ifelse(as_whole("birth_day", birth_day) == 2, 744, 745)
}

#' Reduction and credit factors
#'
#' `early_reduction_factor()` is the retired-worker reduction (5/9 of 1% a
#' month for 36 months, 5/12 after); `spouse_reduction_factor()` the spouse
#' reduction (25/36 of 1%, then 5/12); `delayed_credit_factor()` the delayed
#' retirement credit for an eligibility year.
#'
#' @param months Months of reduction or credit.
#' @param elig_year Year of attaining 62.
#' @param policy A [current_law()] policy.
#' @return Factors.
#' @export
#' @examples
#' early_reduction_factor(60)
early_reduction_factor <- function(months, policy = current_law()) {
  m <- as_whole("months", months)
  n1 <- policy$ar_months_first
  first <- 1 - m * policy$ar_rate_first
  later <- 1 - n1 * policy$ar_rate_first - (m - n1) * policy$ar_rate_later
  ifelse(m <= n1, first, later)
}

#' @rdname early_reduction_factor
#' @export
spouse_reduction_factor <- function(months, policy = current_law()) {
  m <- as_whole("months", months)
  n1 <- policy$ar_months_first
  first <- 1 - m * policy$spouse_ar_rate_first
  later <- 1 - n1 * policy$spouse_ar_rate_first - (m - n1) * policy$ar_rate_later
  ifelse(m < 0, 0, ifelse(m <= n1, first, later))
}

#' @rdname early_reduction_factor
#' @export
delayed_credit_factor <- function(months, elig_year, policy = current_law()) {
  m <- as_whole("months", months)
  1 + m * at(policy$drc_rate_by_elig, as_whole("elig_year", elig_year), "drc_rate")
}

#' Months of reduction or delayed credit
#'
#' Delayed credits earned in the year of entitlement are credited the
#' following January, so a benefit paid in the entitlement year counts only
#' the credits through the prior December, unless the worker has reached 70.
#'
#' @inheritParams eligibility_year
#' @param claim_age Age at entitlement, in months.
#' @param benefit_age Age in the benefit month (default `claim_age`).
#' @param policy A [current_law()] policy.
#' @param credits_from Internal: the month index from which credits may
#'   start (the first quarter fully insured), when later than NRA.
#' @return A list with `reduction` and `credit` months.
#' @export
adjustment_months <- function(birth_year, birth_month, claim_age, birth_day = 15,
                              benefit_age = NULL, policy = current_law(),
                              credits_from = NULL) {
  kb <- adjusted_birth(birth_year, birth_month, birth_day)
  claim <- as_whole("claim_age", claim_age)
  ben_age <- if (is.null(benefit_age)) claim else as_whole("benefit_age", benefit_age)
  bd <- as_whole("birth_day", birth_day)
  n <- common_length(kb$year, claim, ben_age, bd)
  ky <- recycle_to(kb$year, "birth_year", n)
  km <- recycle_to(kb$month, "birth_month", n)
  claim <- recycle_to(claim, "claim_age", n)
  ben_age <- recycle_to(ben_age, "benefit_age", n)
  bd <- recycle_to(bd, "birth_day", n)
  check("claim_age", claim < earliest_claim_age(bd), "before the earliest retirement age")
  check("benefit_age", ben_age < claim, "before claim_age")
  nra <- at(policy$nra, ky + 62, "nra")
  k <- month_index(ky, km)
  ent <- k + claim
  ben <- k + ben_age
  fra <- k + nra
  start <- if (is.null(credits_from)) fra else pmax(fra, credits_from)
  age70 <- k + 70 * 12
  jan_ent <- 12 * (ent %/% 12)
  credited_to <- ifelse(age70 <= ent, age70,
                        ifelse(age70 <= ben | ben %/% 12 > ent %/% 12, ent,
                               pmax(fra, jan_ent)))
  late <- claim >= nra
  list(reduction = ifelse(late, 0, nra - claim),
       credit = ifelse(late, pmax(credited_to - start, 0), 0))
}

#' Benefit factor for a retired worker
#'
#' The multiplier on the PIA for a worker claiming at `claim_age`, for the
#' benefit paid at `benefit_age`.
#'
#' @inheritParams adjustment_months
#' @return Factors.
#' @export
#' @examples
#' benefit_factor(1960, 6, c(62 * 12 + 1, 67 * 12, 70 * 12))
benefit_factor <- function(birth_year, birth_month, claim_age, birth_day = 15,
                           benefit_age = NULL, policy = current_law(),
                           credits_from = NULL) {
  adj <- adjustment_months(birth_year, birth_month, claim_age, birth_day, benefit_age,
                           policy, credits_from)
  elig <- rep_len(adjusted_birth(birth_year, birth_month, birth_day)$year + 62,
                  length(adj$reduction))
  ifelse(adj$reduction > 0, early_reduction_factor(adj$reduction, policy),
         delayed_credit_factor(adj$credit, elig, policy))
}

#' Monthly benefit payable
#'
#' factor x PIA, rounded down to the dime, then paid in whole dollars.
#'
#' @param pia PIA in the benefit month.
#' @param factor Reduction or credit factor.
#' @param benefit_year,benefit_month The benefit month.
#' @return Dollars.
#' @export
#' @examples
#' monthly_benefit(1234.5, 0.7, 2030, 6)
monthly_benefit <- function(pia, factor, benefit_year, benefit_month = 12) {
  cy <- cola_year(as_whole("benefit_year", benefit_year), as_whole("benefit_month", benefit_month))
  floor_dollar(round_benefit(as_num("factor", factor) * as_num("pia", pia), cy))
}
