# SSA rounding rules, vectorized. Transliterated from BenefitAmount.cpp (via
# pyanypia's rounding.py). Constants and operation order are copied so
# results match the official calculator bit for bit. Do not "clean up" the
# arithmetic.

AMEND73_YEAR <- 1973 # margin for rounding up changed 0.005 -> 0.0001
AMEND82_YEAR <- 1982 # dime rounding changed from up to down

# C's fmod for positive x and y: exact, unlike R's %%.
fmod <- function(x, y) {
  r <- x - trunc(x / y) * y
  ifelse(r < 0, r + y, r)
}

#' Round a benefit to the dime as SSA does
#'
#' Down to a multiple of $0.10 from 1982; up before (with a half-cent
#' margin through 1972).
#'
#' @param amount Amounts to round.
#' @param year The benefit-increase year, or the year before a wage-indexed
#'   formula's eligibility year.
#' @return Rounded amounts.
#' @export
round_benefit <- function(amount, year) {
  dims <- dim(amount)
  n <- max(length(amount), length(year))
  amount <- rep_len(as.numeric(amount), n)
  year <- rep_len(as.numeric(year), n)
  down <- floor(10 * amount + 0.0005) / 10
  old <- year < AMEND82_YEAR
  out <- down
  if (any(old)) {
    a <- amount[old]
    q <- vif(year[old] >= AMEND73_YEAR, 0.009, 0.499)
    x100 <- fmod(a * 100, 10)
    out[old] <- vif(x100 < q, a - x100 / 100, a + (0.10 - x100 / 100))
  }
  if (!is.null(dims) && prod(dims) == n) dim(out) <- dims
  out
}

round_wage <- function(value) floor(value * 100 + 0.5) / 100

apply_cola_once <- function(amount, percent, year) {
  round_benefit(amount * (1 + percent / 100), year)
}

floor_dollar <- function(amount) floor(amount)
