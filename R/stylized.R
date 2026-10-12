# Stylized earnings histories: the Office of the Chief Actuary's scaled
# hypothetical workers (very low, low, medium, high), the maximum earner,
# steady earners, and custom levels in AWI shares or constant dollars.

# The scaled factors, unrounded. Actuarial Note 2026.3 ("Scaled Factors for
# Hypothetical Earnings Examples Under the 2026 Trustees Report
# Assumptions", https://www.ssa.gov/OACT/NOTES/ran3/an2026-3.pdf) prints the
# factors to three decimals (Table 6), but its worked examples (Table 7) are
# computed from unrounded factors. The high earner born in 1973 has earnings
# at ages 21-64 (1994-2037) of:
T7_EARNINGS <- c(
  10496.92, 13478.14, 18088.27, 22878.36, 27117.60, 31453.75, 35950.89, 39382.67,
  42078.42, 45194.94, 49247.67, 52902.62, 57050.08, 61267.22, 64138.07, 64407.23,
  67059.24, 70160.97, 73285.28, 75034.57, 78489.43, 81858.89, 83378.88, 86882.85,
  90485.74, 94255.25, 97195.53, 105975.39, 111598.74, 116496.84, 121933.87,
  125205.41, 129991.25, 134165.78, 138319.71, 141626.75, 144796.05, 147655.08,
  149646.23, 149881.74, 147484.97, 147849.82, 148051.40, 148167.37
)
# ... on an AWI of (identical to current_law()$awi for 1994-2037):
T7_AWI <- c(
  23753.53, 24705.66, 25913.90, 27426.00, 28861.44, 30469.84, 32154.82, 32921.92,
  33252.09, 34064.95, 35648.55, 36952.94, 38651.41, 40405.48, 41334.97, 40711.61,
  41673.83, 42979.61, 44321.67, 44888.16, 46481.52, 48098.63, 48642.15, 50321.89,
  52145.80, 54099.99, 55628.60, 60575.07, 63795.13, 66621.80, 69846.57, 72025.07,
  75246.70, 78286.92, 81537.43, 85047.82, 88895.99, 92915.31, 96989.47, 101085.63,
  105045.59, 109064.86, 113112.48, 117242.36
)
# Their ratio is the high factor to about seven digits. All four levels are
# one set of preliminary factors times a constant (the note's "adjustment
# factors"), so each level is the high factor times its share over 1.6.
# These reproduce every factor in Table 6 when rounded to three decimals,
# and the medium factors' 35 highest average 1.000.
SCALED_AGES <- 21:64
SCALED_SHARES <- c(very_low = 0.25, low = 0.45, medium = 1, high = 1.6)
STYLIZED_TYPES <- c(names(SCALED_SHARES), "max")
medium_factor <- function(age) (T7_EARNINGS / T7_AWI / 1.6)[age - SCALED_AGES[1] + 1]

#' The actuaries' scaled factors
#'
#' Earnings at each age from 21 to 64 as a share of that year's average wage
#' index (AWI), for the Office of the Chief Actuary's four scaled
#' hypothetical workers, from Actuarial Note 2026.3 (2026 Trustees Report).
#' The note prints them to three decimals (its Table 6) but computes its
#' examples from unrounded factors; these are the unrounded ones, recovered
#' from the note's worked example (Table 7), and they round to Table 6
#' exactly.
#'
#' The four sets differ only by a constant: each worker's career-average
#' earnings (the 35 highest years, wage-indexed to the year they turn 64)
#' are 25%, 45%, 100% and 160% of the AWI for that year.
#'
#' @param digits Round to this many decimals (`3` gives Table 6), or `NULL`
#'   (default) for the unrounded factors.
#' @return A tibble with `age`, `very_low`, `low`, `medium` and `high`.
#' @seealso [stylized_earnings()]
#' @export
#' @examples
#' scaled_factors(digits = 3)
scaled_factors <- function(digits = NULL) {
  m <- medium_factor(SCALED_AGES)
  out <- tibble::tibble(age = SCALED_AGES, very_low = 0.25 * m, low = 0.45 * m,
                        medium = m, high = 1.6 * m)
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
#' * `level = 1.1`: any share, here 110%. `level = 1` is the medium worker,
#'   and `level = 0.25` the very low one.
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
