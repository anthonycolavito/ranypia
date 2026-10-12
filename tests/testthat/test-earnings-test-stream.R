# benefit_stream() with the earnings test: withholding month by month from
# each year's excess, and the reduction recomputed at NRA.

awi_e <- function(years) setNames(current_law()$awi[years - 1936], years)

test_that("the earnings test is off by default and changes nothing", {
  e <- c(awi_e(1986:2025), setNames(rep(50000, 5), 2026:2030))
  a <- benefit_stream(e, 1964, 6, claim_age = 745)
  b <- benefit_stream(e, 1964, 6, claim_age = 745, earnings_test = TRUE)
  expect_false("withheld" %in% names(a))
  expect_equal(b$benefit[b$benefit_age < 804], a$benefit[a$benefit_age < 804])
})

test_that("each year's excess is withheld and the reduction recomputed at NRA", {
  # born June 1964 (NRA 67, June 2031), claims July 2026, earns $50,000
  # in 2026-2030
  e <- c(awi_e(1986:2025), setNames(rep(50000, 5), 2026:2030))
  m <- benefit_stream(e, 1964, 6, claim_age = 745, to_age = 68 * 12, step = 1,
                      earnings_test = TRUE)
  p <- current_law()
  by_year <- tapply(m$withheld, m$benefit_year, sum)
  # the claim year is prorated by the six months entitled
  expect_equal(by_year[["2026"]], (50000 * 6 / 12 - 24480) / 2)
  for (y in 2027:2030) {
    expect_equal(by_year[[as.character(y)]], (50000 - p$ret_lower[yi(y)]) / 2)
  }
  expect_equal(by_year[["2031"]], 0)
  expect_equal(m$paid, m$benefit - m$withheld)
  # whole months from January, the last one in part
  y27 <- m[m$benefit_year == 2027, ]
  full <- which(y27$withheld == y27$benefit)
  expect_equal(full, seq_along(full))
  expect_true(all(y27$withheld[-seq_len(length(full) + 1)] == 0))
  # months with a deduction come out of the 59 months of reduction
  months <- sum(m$withheld > 0 & m$benefit_age < 804)
  expect_equal(unique(m$factor[m$benefit_age >= 804]), early_reduction_factor(59 - months))
  expect_equal(unique(m$factor[m$benefit_age < 804]), early_reduction_factor(59))
})

test_that("grace_year = 'none' charges nothing in the claim year", {
  e <- c(awi_e(1986:2025), setNames(rep(50000, 2), 2026:2027))
  m <- benefit_stream(e, 1964, 6, claim_age = 745, to_age = 63 * 12 + 11, step = 1,
                      earnings_test = TRUE, grace_year = "none")
  expect_equal(sum(m$withheld[m$benefit_year == 2026]), 0)
  expect_gt(sum(m$withheld[m$benefit_year == 2027]), 0)
})

test_that("before 2000: tested to 70, 1/3 from 1990, credits for months after 65", {
  # born June 1925 (NRA 65 in June 1990, 70 in June 1995), claims July 1987
  # and earns $30,000 a year through 1994
  e <- setNames(c(rep(20000, 35), rep(30000, 8)), 1952:1994)
  m <- benefit_stream(e, 1925, 6, claim_age = 745, to_age = 72 * 12, step = 1,
                      earnings_test = TRUE)
  by_year <- tapply(m$withheld, m$benefit_year, sum)
  expect_equal(by_year[["1990"]], (30000 - 9360) / 3)
  expect_equal(by_year[["1995"]], 0)
  pre <- sum(m$withheld > 0 & m$benefit_age < 780)
  post <- m$withheld > 0 & m$benefit_age >= 780
  expect_equal(pre, 35)   # every month to 65 withheld: no reduction left
  rate <- current_law()$drc_rate_by_elig[yi(1987)]
  # credits from the January after each year's withheld months
  credited_1993 <- sum(post & m$benefit_year < 1993)
  expect_equal(unique(m$factor[m$benefit_year == 1993]), 1 + credited_1993 * rate)
  expect_equal(unique(m$factor[m$benefit_age >= 840]), 1 + sum(post) * rate)
})

test_that("a repealed test withholds nothing", {
  e <- c(awi_e(1986:2025), setNames(rep(80000, 5), 2026:2030))
  p <- policy_replace(current_law(), ret_enabled = FALSE)
  m <- benefit_stream(e, 1964, 6, claim_age = 745, earnings_test = TRUE, policy = p)
  expect_true(all(m$withheld == 0))
})
