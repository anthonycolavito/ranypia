# The PIA and family-maximum formulas, and benefit increases (ported from
# pyanypia's formula.py).

AWI_BASE_YEAR <- 1977 # the 1979 bend points are defined against the 1977 AWI

index_bend <- function(base, elig, policy) {
  temp <- at(policy$awi, elig - 2, "awi") / policy$awi[yi(AWI_BASE_YEAR)]
  out <- vapply(base, function(b) floor(b * temp + 0.5), numeric(length(elig)))
  matrix(out, nrow = length(elig))
}

#' Bend points
#'
#' The PIA formula bend points (`bend_points()`) and family-maximum bend
#' points (`family_max_bend_points()`) for eligibility years: one row per
#' year, one column per bend point.
#'
#' @param elig_year Eligibility years.
#' @param policy A [current_law()] policy.
#' @return A matrix.
#' @export
#' @examples
#' bend_points(2026)
bend_points <- function(elig_year, policy = current_law()) {
  index_bend(policy$pia_bend_base, as_whole("elig_year", elig_year), policy)
}

#' @rdname bend_points
#' @export
family_max_bend_points <- function(elig_year, policy = current_law()) {
  index_bend(policy$mfb_bend_base, as_whole("elig_year", elig_year), policy)
}

# sum(pct[i] * portion[i]) accumulated left to right, as the C++ does.
# `pct` is a vector, or a matrix with one row per element of x.
bracket_sum <- function(x, bp, pct) {
  k <- ncol(bp)
  parts <- list(pmin(x, bp[, 1]))
  if (k > 1) {
    for (i in seq_len(k - 1)) {
      parts[[i + 1]] <- pmax(0, pmin(x - bp[, i], bp[, i + 1] - bp[, i]))
    }
  }
  parts[[k + 1]] <- pmax(x - bp[, k], 0)
  total <- numeric(length(x))
  for (i in seq_along(parts)) {
    p <- if (is.matrix(pct)) pct[, i] else pct[i]
    total <- total + p * parts[[i]]
  }
  total
}

#' Primary insurance amount
#'
#' The PIA at eligibility, before COLAs.
#'
#' @param aime Average indexed monthly earnings.
#' @param elig_year Eligibility year.
#' @param policy A [current_law()] policy.
#' @param pct Optional formula percentages replacing the policy's: a vector,
#'   or a matrix with one row per worker (the WEP uses this).
#' @return PIAs.
#' @export
#' @examples
#' pia(c(1000, 5000, 12000), 2026)
pia <- function(aime, elig_year, policy = current_law(), pct = NULL) {
  a <- as_num("aime", aime)
  check("aime", a < 0, "negative")
  e <- as_whole("elig_year", elig_year)
  n <- common_length(a, e)
  a <- recycle_to(a, "aime", n)
  e <- recycle_to(e, "elig_year", n)
  percents <- if (is.null(pct)) policy$pia_pct else pct
  round_benefit(bracket_sum(a, bend_points(e, policy), percents), e - 1)
}

#' Maximum family benefit
#'
#' The family maximum at eligibility, before COLAs, for retirement and
#' survivor cases. Disability uses [di_family_max()].
#'
#' @param pia_elig PIA at eligibility.
#' @inheritParams pia
#' @return Family maximums.
#' @export
family_max <- function(pia_elig, elig_year, policy = current_law()) {
  p <- as_num("pia_elig", pia_elig)
  e <- as_whole("elig_year", elig_year)
  n <- common_length(p, e)
  p <- recycle_to(p, "pia_elig", n)
  e <- recycle_to(e, "elig_year", n)
  round_benefit(bracket_sum(p, family_max_bend_points(e, policy), policy$mfb_pct), e - 1)
}

#' Apply cost-of-living adjustments
#'
#' Carries a PIA or family maximum from eligibility to a benefit month,
#' applying each benefit increase from the eligibility year on. The
#' December 1999 increase is 0.1 point higher for benefits from July 2001.
#'
#' @param amount Amounts at eligibility.
#' @param elig_year Eligibility year.
#' @param benefit_year,benefit_month The benefit month.
#' @param policy A [current_law()] policy.
#' @return Amounts in the benefit month.
#' @export
#' @examples
#' apply_colas(2000, 2022, 2026, 1)
apply_colas <- function(amount, elig_year, benefit_year, benefit_month = 12,
                        policy = current_law()) {
  a <- as_num("amount", amount)
  e <- as_whole("elig_year", elig_year)
  by <- as_whole("benefit_year", benefit_year)
  bm <- as_whole("benefit_month", benefit_month)
  n <- common_length(a, e, by, bm)
  a <- recycle_to(a, "amount", n)
  e <- recycle_to(e, "elig_year", n)
  by <- recycle_to(by, "benefit_year", n)
  bm <- recycle_to(bm, "benefit_month", n)
  if (n == 0) return(a)
  cy <- cola_year(by, bm)
  from_jul2001 <- by > 2001 | (by == 2001 & bm >= 7)
  if (max(cy) < min(e)) return(a)
  for (y in seq(min(e), max(cy))) {
    active <- e <= y & y <= cy
    if (!any(active)) next
    pct <- at(policy$cola, y, "cola") + ifelse(y == 1999 & from_jul2001, 0.1, 0)
    idx <- which(active)
    a[idx] <- apply_cola_once(a[idx], if (length(pct) > 1) pct[idx] else pct, y)
  }
  a
}
