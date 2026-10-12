# Stylized earnings histories: the Office of the Chief Actuary's scaled
# hypothetical workers (very low, low, medium, high), the maximum earner,
# steady earners, and custom levels in AWI shares or constant dollars.

# The actuaries' method (Actuarial Note 2026.3, "Scaled Factors for
# Hypothetical Earnings Examples Under the 2026 Trustees Report
# Assumptions", https://www.ssa.gov/OACT/NOTES/ran3/an2026-3.pdf):
#
# 1. Preliminary adjusted scaled factors by age (the note's Table 4), from
#    CWHS earnings of insured workers, with ages 62-64 held flat in nominal
#    terms after 61.
# 2. A preliminary worker born in 1960 earns those factors times the AWI at
#    each age, 1981-2024. Wage-indexed to 2024, the year before entitlement
#    at 65, the 35 highest years average $57,103 (Table 4).
# 3. Each worker's target career average is a share of the 2024 AWI: 25%,
#    45%, 100% and 160% (Table 5). The ratio of target to $57,103 is the
#    adjustment factor.
# 4. Each worker's scaled factors are the preliminary factors times its
#    adjustment factor (Table 6).
#
# Table 4 prints the preliminary factors to three decimals but the 1960
# worker's earnings to the cent; these are those earnings, 1981-2024:
T4_EARNINGS <- c(
  3109.99, 4050.73, 5435.27, 6877.44, 8076.40, 9136.71, 10526.90, 11817.80, 12996.33,
  14255.20, 15396.61, 16777.56, 17446.59, 18403.90, 19587.93, 20948.02, 22550.26,
  24073.82, 25743.33, 27464.36, 28405.99, 28916.52, 29836.30, 31449.44, 32764.51,
  34408.64, 36073.00, 36950.69, 36390.15, 37235.32, 38338.54, 39368.55, 39623.45,
  40703.03, 41692.08, 41389.36, 41881.80, 42342.29, 42651.30, 42145.51, 43456.80,
  44189.32, 44556.62, 45103.16
)
# ... on the AWI for 1981-2024 (identical to current_law()$awi):
T4_AWI <- c(
  13773.10, 14531.34, 15239.24, 16135.07, 16822.51, 17321.82, 18426.51, 19334.04,
  20099.55, 21027.98, 21811.60, 22935.42, 23132.67, 23753.53, 24705.66, 25913.90,
  27426.00, 28861.44, 30469.84, 32154.82, 32921.92, 33252.09, 34064.95, 35648.55,
  36952.94, 38651.41, 40405.48, 41334.97, 40711.61, 41673.83, 42979.61, 44321.67,
  44888.16, 46481.52, 48098.63, 48642.15, 50321.89, 52145.80, 54099.99, 55628.60,
  60575.07, 63795.13, 66621.80, 69846.57
)
# Their ratio recovers the unrounded preliminary factors (age 22: 0.278758,
# as the note's text gives it).
SCALED_AGES <- 21:64
SCALED_SHARES <- c(very_low = 0.25, low = 0.45, medium = 1, high = 1.6)
STYLIZED_TYPES <- c(names(SCALED_SHARES), "max")
PRELIM_FACTORS <- T4_EARNINGS / T4_AWI
AWI_2024 <- T4_AWI[length(T4_AWI)]

# Step 2: the 1960 worker's career average, indexed to 2024, in whole
# dollars as the note uses it.
oact_career_average <- function() {
  indexed <- floor(T4_EARNINGS * AWI_2024 / T4_AWI * 100 + 0.5) / 100
  floor(mean(sort(indexed, decreasing = TRUE)[1:35]) + 0.5)
}

# Steps 3-4: the scaled factor at `age` for a worker whose career average
# is `share` times the AWI.
scaled_factor <- function(age, share) {
  PRELIM_FACTORS[age - SCALED_AGES[1] + 1] * share * AWI_2024 / oact_career_average()
}
medium_factor <- function(age) scaled_factor(age, 1)

#' The actuaries' scaled factors
#'
#' Earnings at each age from 21 to 64 as a share of that year's average wage
#' index (AWI), for the Office of the Chief Actuary's four scaled
#' hypothetical workers, computed by the method of Actuarial Note 2026.3
#' (2026 Trustees Report):
#'
#' 1. The note's preliminary adjusted scaled factors (its Table 4) are
#'    applied to the AWI for a worker born in 1960.
#' 2. That worker's career-average earnings, the 35 highest years
#'    wage-indexed to 2024 (the year before entitlement at 65), are $57,103.
#' 3. Each worker's adjustment factor is its target career average (25%, 45%,
#'    100% or 160% of the 2024 AWI) over $57,103.
#' 4. Its scaled factors are the preliminary factors times its adjustment
#'    factor.
#'
#' The note prints the factors to three decimals (its Table 6) but computes
#' with unrounded ones. These are unrounded: the preliminary factors are
#' recovered from the 1960 worker's earnings, which Table 4 prints to the
#' cent. They round to Table 6 exactly.
#'
#' @param digits Round to this many decimals (`3` gives Table 6), or `NULL`
#'   (default) for the unrounded factors.
#' @return A tibble with `age`, `preliminary`, `very_low`, `low`, `medium`
#'   and `high`.
#' @seealso [stylized_earnings()]
#' @export
#' @examples
#' scaled_factors(digits = 3)
scaled_factors <- function(digits = NULL) {
  f <- function(share) scaled_factor(SCALED_AGES, share)
  out <- tibble::tibble(age = SCALED_AGES, preliminary = PRELIM_FACTORS,
                        very_low = f(0.25), low = f(0.45), medium = f(1), high = f(1.6))
  if (!is.null(digits)) out[-1] <- round(out[-1], digits)
  out
}

#' Stylized earnings histories
#'
#' Builds the earnings history of a hypothetical worker, in the long `id`,
#' `year`, `earnings` form every function takes as `earnings`: the
#' Office of the Chief Actuary's scaled workers and maximum earner, or a
#' worker at any level, either as a share of the average wage index (AWI)
#' or in constant dollars.
#'
#' **Scaled workers** (`shape = "scaled"`, the default) follow the
#' actuaries' method (Actuarial Note 2026.3): earnings at age `a` are the
#' scaled factor for that age times the AWI for the year the worker turns
#' `a`, from age 21 to 64. The factors (see [scaled_factors()]) trace the
#' typical age profile of insured workers' earnings, scaled so the career
#' average (the 35 highest years, wage-indexed to the year the worker
#' turns 64) is a set share of that year's AWI:
#'
#' * `type = "very_low"`, `"low"`, `"medium"`, `"high"`: 25%, 45%, 100% and
#'   160% of the AWI.
#' * `level = 1.1`: any share, here 110%, by the same method: the
#'   preliminary factors times 1.1 times the 2024 AWI over $57,103.
#'   `level = 1` is the medium worker, and `level = 0.25` the very low one.
#' * `real_level = 60000`: a real career average of $60,000 in
#'   `price_year` dollars. The medium profile's earnings are converted to
#'   `price_year` dollars with the GDP price index, and the whole profile is
#'   scaled so the average of the 35 highest real years is $60,000.
#'
#' **Steady workers** (`shape = "steady"`) earn the same amount every year
#' from `start_age` to `end_age`: `level` (or a type's share) times each
#' year's AWI, or with `real_level`, a constant amount in `price_year`
#' dollars, rising with the GDP price index.
#'
#' **The maximum earner** (`type = "max"`) earns the taxable maximum each
#' year from `start_age` to `end_age`; `shape` doesn't apply.
#'
#' Earnings are rounded to cents and are not capped (the benefit functions
#' cap them). They follow `policy`'s AWI, taxable maximum and prices, so a
#' policy with faster wage growth gives higher earnings. The actuaries'
#' workers are born in January (the note's are born on 2 January), so to
#' match their benefit examples pass `birth_month = 1, birth_day = 2` to the
#' benefit functions.
#'
#' @param birth_year Year of birth.
#' @param type `"very_low"`, `"low"`, `"medium"`, `"high"` or `"max"`.
#'   Ignored when `level` or `real_level` is given.
#' @param level Career-average earnings (scaled) or annual earnings
#'   (steady), as a share of the AWI: `1.1` is 110%.
#' @param real_level Career-average (scaled: the 35 highest years in
#'   `price_year` dollars) or annual (steady) earnings in `price_year`
#'   dollars, deflated by the GDP price index.
#' @param price_year The dollars `real_level` is in.
#' @param shape `"scaled"` (the actuaries' age profile) or `"steady"` (the
#'   same share of the AWI, or the same real amount, every year).
#' @param start_age,end_age First and last age with earnings (in years).
#'   Scaled workers can only have earnings at 21-64, the ages the factors
#'   cover; narrowing the range drops years without rescaling the rest.
#' @param id Worker ids (default `1, 2, ...`).
#' @param policy A [current_law()] policy.
#' @return A tibble with `id`, `year`, `age` (in years) and `earnings`, one
#'   row per year with earnings.
#'
#'   Every argument but `policy` is vectorized: each element of the
#'   recycled arguments is one worker.
#' @seealso [scaled_factors()]
#' @export
#' @examples
#' # The actuaries' medium worker born in 1964
#' stylized_earnings(1964)
#'
#' # The four scaled workers and the maximum earner, born in 1960
#' types <- c("very_low", "low", "medium", "high", "max")
#' e <- stylized_earnings(1960, type = types)
#' retired_worker(e, people = data.frame(id = 1:5, birth_year = 1960, birth_month = 1,
#'                                       birth_day = 2, claim_age = 65 * 12))
#'
#' # A scaled worker whose career average is 110% of the AWI, and a steady
#' # worker at 110% of the AWI every year
#' stylized_earnings(1970, level = 1.1, shape = c("scaled", "steady"))
stylized_earnings <- function(birth_year, type = "medium", level = NULL, real_level = NULL,
                              price_year = 2026, shape = "scaled", start_age = 21,
                              end_age = 64, id = NULL, policy = current_law()) {
  if (!is.null(level) && !is.null(real_level)) {
    abort_field("level", "give level or real_level, not both")
  }
  custom <- !is.null(level) || !is.null(real_level)
  if (custom && !missing(type)) {
    abort_field("type", "give a type, or a level or real_level, not both")
  }
  n <- common_length(birth_year, type, level, real_level, price_year, shape, start_age,
                     end_age, id)
  by <- rows_of(birth_year, "birth_year", n)
  start <- rows_of(start_age, "start_age", n)
  end <- rows_of(end_age, "end_age", n)
  shape <- recycle_to(as.character(shape), "shape", n)
  check("shape", !shape %in% c("scaled", "steady"), "must be \"scaled\" or \"steady\"")
  if (custom) {
    type <- rep("custom", n)
  } else {
    type <- recycle_to(as.character(type), "type", n)
    check("type", !type %in% STYLIZED_TYPES,
          paste("must be one of", paste0("\"", STYLIZED_TYPES, "\"", collapse = ", ")))
  }
  ids <- if (is.null(id)) seq_len(n) else recycle_to(id, "id", n)
  check("start_age", start < 0, "must not be negative")
  check("end_age", end < start, "before start_age")
  scaled <- shape == "scaled" & type != "max"
  check("start_age", scaled & start < min(SCALED_AGES),
        sprintf("below %d, where the scaled factors start", min(SCALED_AGES)))
  check("end_age", scaled & end > max(SCALED_AGES),
        sprintf("above %d, where the scaled factors end", max(SCALED_AGES)))

  # One row per worker-year.
  len <- end - start + 1
  w <- rep(seq_len(n), len)
  age <- sequence(len, from = start)
  year <- by[w] + age
  awi <- at(policy$awi, year, "awi")

  if (!is.null(real_level)) {
    real <- as_num("real_level", recycle_to(real_level, "real_level", n))
    check("real_level", real < 0, "negative")
    py <- rows_of(price_year, "price_year", n)
    base <- prices_at(policy, py)
    # steady: the real amount in each year's dollars
    earnings <- real[w] * prices_at(policy, year) / base[w]
    # scaled: the medium profile in each year's dollars, in price_year
    # dollars, and the multiple of it whose 35 highest real years average
    # real_level
    if (any(scaled)) {
      k <- rep(NA_real_, n)
      s <- which(scaled)
      ages <- matrix(SCALED_AGES, nrow = length(s), ncol = length(SCALED_AGES), byrow = TRUE)
      yrs <- by[s] + ages
      prof <- medium_factor(ages) * at(policy$awi, yrs, "awi") *
        base[s] / prices_at(policy, yrs)
      top35 <- apply(prof, 1, function(r) mean(sort(r, decreasing = TRUE)[1:35]))
      k[s] <- real[s] / top35
      earnings <- vif(scaled[w], k[w] * medium_factor(pmin(pmax(age, 21), 64)) * awi,
                      earnings)
    }
  } else {
    share <- if (custom) {
      as_num("level", recycle_to(level, "level", n))
    } else {
      unname(SCALED_SHARES[type])
    }
    check("level", !is.na(share) & share < 0, "negative")
    earnings <- vif(scaled[w], share[w] * medium_factor(pmin(pmax(age, 21), 64)) * awi,
                    share[w] * awi)
    is_max <- type[w] == "max"
    if (any(is_max)) earnings[is_max] <- at(policy$taxmax, year[is_max], "taxmax")
  }

  tibble::tibble(id = ids[w], year = year, age = age,
                 earnings = floor(earnings * 100 + 0.5) / 100)
}

prices_at <- function(policy, years) {
  if (all(is.na(policy$gdp_price))) {
    abort_field("real_level", paste(
      "needs a price index, and this policy has none: set gdp_price_hist and",
      "gdp_price_growth with policy_replace()"
    ))
  }
  at(policy$gdp_price, years, "gdp_price")
}
