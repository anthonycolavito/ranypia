#' Special minimum PIA
#'
#' The PIA and family maximum under the special minimum in a benefit month
#' from 1979: a table by years of coverage over 10, carried forward by COLAs
#' from $11.50 a year in January 1979. Zero for 10 or fewer years of
#' coverage; 30 or more get the top amount. Count years with
#' [years_of_coverage()].
#'
#' @param years_of_coverage Years of coverage.
#' @param benefit_year,benefit_month The benefit month.
#' @param policy A [current_law()] policy.
#' @return A list with `pia` and `mfb`.
#' @export
#' @examples
#' special_minimum_pia(30, 2026)
special_minimum_pia <- function(years_of_coverage, benefit_year, benefit_month = 12,
                                policy = current_law()) {
  yoc <- as_whole("years_of_coverage", years_of_coverage)
  by <- as_whole("benefit_year", benefit_year)
  bm <- as_whole("benefit_month", benefit_month)
  n <- common_length(yoc, by, bm)
  yoc <- recycle_to(yoc, "years_of_coverage", n)
  by <- recycle_to(by, "benefit_year", n)
  bm <- recycle_to(bm, "benefit_month", n)
  excess <- pmin(pmax(yoc - 10, 0), 20)
  before_increase <- bm < ifelse(by >= 1983, 12, 6)
  # August-November 2001 pay the December 2000 amounts with the 1999 correction
  aug2001 <- before_increase & by == 2001 & bm >= 8
  year <- ifelse(before_increase, by - 1, by)
  row <- pmax(excess, 1)
  col <- pmin(pmax(yi(year), 1), N_YEARS)
  pia_v <- ifelse(aug2001, policy$spec_min_pia_aug2001[row], policy$spec_min_pia[cbind(row, col)])
  mfb_v <- ifelse(aug2001, policy$spec_min_mfb_aug2001[row], policy$spec_min_mfb[cbind(row, col)])
  has <- excess > 0
  list(pia = ifelse(has, pia_v, 0), mfb = ifelse(has, mfb_v, 0))
}
