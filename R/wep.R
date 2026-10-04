# Windfall elimination provision and government pension offset (ported
# from pyanypia's wep.py). The Social Security Fairness Act repealed both
# for benefits payable after December 2023; they are here for historical
# and counterfactual work. retired_worker() applies the WEP only when
# policy$wep_enabled is set. AnyPIA has no GPO, so gpo_offset() follows the
# statute (42 USC 402(k)(5)).

WINDFALL_YEARS <- 30

# PercPia::setWindfallPerc: the first formula percentage under the WEP.
windfall_first_pct <- function(elig, benefit_year, yoc, p0) {
  rv <- ifelse(elig > 1989, p0 - 0.5, p0 - 0.10 * (elig - 1985))
  annual <- ifelse(benefit_year >= 1989, 0.05, 0.10)
  floor_pct <- p0 - annual * (WINDFALL_YEARS - yoc)
  rv <- ifelse(floor_pct > rv, floor_pct, rv)
  rv <- ifelse(rv < 0, 0, rv)
  ifelse(elig < 1985 | yoc >= WINDFALL_YEARS, p0, rv)
}

#' Windfall elimination provision
#'
#' The PIA at eligibility under the WEP: the higher of the PIA with a
#' reduced first percentage and the regular PIA less half the monthly
#' noncovered pension. Count `years_of_coverage` with
#' `years_of_coverage(kind = "wep")`.
#'
#' @param aime AIME.
#' @param elig_year Eligibility year.
#' @param years_of_coverage Years of substantial earnings.
#' @param pension Monthly noncovered pension.
#' @param benefit_year Year of the benefit.
#' @param policy A [current_law()] policy.
#' @return PIAs at eligibility.
#' @export
wep_pia <- function(aime, elig_year, years_of_coverage, pension, benefit_year,
                    policy = current_law()) {
  a <- as_num("aime", aime)
  e <- as_whole("elig_year", elig_year)
  yoc <- as_whole("years_of_coverage", years_of_coverage)
  pen <- as_num("pension", pension)
  by <- as_whole("benefit_year", benefit_year)
  n <- common_length(a, e, yoc, pen, by)
  a <- recycle_to(a, "aime", n)
  e <- recycle_to(e, "elig_year", n)
  yoc <- recycle_to(yoc, "years_of_coverage", n)
  pen <- recycle_to(pen, "pension", n)
  by <- recycle_to(by, "benefit_year", n)
  first_pct <- windfall_first_pct(e, by, yoc, policy$pia_pct[1])
  pct <- cbind(first_pct, matrix(rep(policy$pia_pct[-1], each = n), nrow = n))
  regular <- pia(a, e, policy)
  reduced <- pia(a, e, policy, pct = pct)
  floor_pia <- regular - round_benefit(0.5 * pen, e - 1)
  wep <- ifelse(reduced > floor_pia, reduced, floor_pia)
  ifelse(e > 1985 & pen > 0 & yoc < WINDFALL_YEARS, wep, regular)
}

#' Government pension offset
#'
#' A spouse's or widow(er)'s benefit reduced by two-thirds of the monthly
#' noncovered government pension (`policy$gpo_fraction`), not below zero, in
#' whole dollars.
#'
#' @param benefit Benefit before the offset.
#' @param pension Monthly noncovered government pension.
#' @param policy A [current_law()] policy.
#' @return Benefits.
#' @export
gpo_offset <- function(benefit, pension, policy = current_law()) {
  floor_dollar(pmax(as_num("benefit", benefit) - policy$gpo_fraction * as_num("pension", pension), 0))
}
