# Month arithmetic. SSA counts ages from the day before birth.

#' The adjusted birth month
#'
#' The year and month of the day before birth, from which SSA counts ages.
#' Only a birth on the 1st moves it, into the previous month: such a person
#' attains each age a month early.
#'
#' @param birth_year,birth_month,birth_day Date of birth (day defaults to 15).
#' @return A list with `year` and `month`.
#' @export
#' @examples
#' adjusted_birth(1960, 3, 1)
adjusted_birth <- function(birth_year, birth_month, birth_day = 15) {
  by <- as_whole("birth_year", birth_year)
  bm <- as_whole("birth_month", birth_month)
  bd <- as_whole("birth_day", birth_day)
  n <- common_length(by, bm, bd)
  by <- recycle_to(by, "birth_year", n)
  bm <- recycle_to(bm, "birth_month", n)
  bd <- recycle_to(bd, "birth_day", n)
  check("birth_month", bm < 1 | bm > 12, "must be 1-12")
  check("birth_day", bd < 1 | bd > 31, "must be 1-31")
  first <- bd == 1
  m <- ifelse(first, bm - 1, bm)
  y <- ifelse(first & m == 0, by - 1, by)
  list(year = y, month = ifelse(m == 0, 12, m))
}

month_index <- function(year, month) 12 * year + month - 1
from_month_index <- function(i) list(year = i %/% 12, month = i %% 12 + 1)

#' The year of the benefit increase in effect
#'
#' Increases take effect in December from 1983, and in June 1974-1982.
#'
#' @param year,month A benefit month.
#' @return The year of the latest benefit increase in effect.
#' @export
#' @examples
#' cola_year(2026, c(11, 12))
cola_year <- function(year, month) {
  beninc <- ifelse(year >= 1983, 12, 6)
  ifelse(month < beninc, year - 1, year)
}
