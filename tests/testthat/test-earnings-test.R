# The retirement earnings test measured on annual earnings (sec. 203(b),
# (f)). Expected values are worked by hand from the Act and SSA's published
# exempt amounts.

test_that("exempt amounts are SSA's published figures", {
  p <- current_law()
  expect_equal(p$ret_lower[yi(c(1978, 1981, 1999, 2010, 2016, 2025, 2026))],
               c(3240, 4080, 9600, 14160, 15720, 23400, 24480))
  expect_equal(p$ret_higher[yi(c(1978, 1996, 2001, 2002, 2026))],
               c(4000, 12500, 25000, 30000, 65160))
})

test_that("sec. 203(f)(8) indexing reproduces the published amounts", {
  # drop the published years the formula covers and project them instead
  p <- policy_replace(current_law(),
                      ret_lower_hist = RET_LOWER_HIST[as.character(1975:1994)],
                      ret_higher_hist = RET_HIGHER_HIST[as.character(1975:2002)])
  expect_equal(p$ret_lower[yi(1995:2026)], unname(RET_LOWER_HIST[as.character(1995:2026)]))
  expect_equal(p$ret_higher[yi(2003:2026)], unname(RET_HIGHER_HIST[as.character(2003:2026)]))
})

test_that("exempt amounts stay put after a year without a COLA", {
  p <- current_law()
  proj <- p$cola_proj
  proj[as.character(2030)] <- 0
  q <- policy_replace(p, cola_proj = proj)
  expect_equal(q$ret_lower[yi(2031)], q$ret_lower[yi(2030)])
  expect_equal(q$ret_higher[yi(2031)], q$ret_higher[yi(2030)])
  expect_gt(q$ret_lower[yi(2032)], q$ret_lower[yi(2031)])
})

test_that("under NRA all year: $1 for every $2 above the lower amount", {
  # born June 1960 (NRA 67 in June 2027), entitled since 62 and 1 month
  r <- earnings_test(30000, 2026, 1960, 6, entitled_from = 745)
  expect_equal(r$exempt, 24480)
  expect_equal(r$tested_months, 12)
  expect_equal(r$excess, (30000 - 24480) / 2)
  expect_equal(earnings_test(20000, 2026, 1960, 6, entitled_from = 745)$excess, 0)
})

test_that("the year of NRA: earnings before the NRA month, $1 for $3", {
  # born November 1960: NRA 67 in November 2027, so January-October tested;
  # $84,000 a year is $70,000 before the NRA month
  r <- earnings_test(84000, 2027, 1960, 11, entitled_from = 745)
  expect_equal(r$tested_months, 10)
  expect_equal(r$tested_earnings, 70000)
  expect_equal(r$exempt, current_law()$ret_higher[yi(2027)])
  expect_equal(r$rate, 1 / 3)
  expect_equal(r$excess, floor((70000 - r$exempt) / 3))
  # no test from NRA on
  expect_equal(earnings_test(1e6, 2028, 1960, 11, entitled_from = 745)$excess, 0)
})

test_that("the first year of entitlement is prorated, or either limit", {
  # born June 1964, entitled at 62 and 1 month: July-December 2026
  e <- 60000
  pr <- earnings_test(e, 2026, 1964, 6, entitled_from = 745)
  expect_true(pr$first_year)
  expect_equal(pr$tested_months, 6)
  expect_equal(pr$excess, (e * 6 / 12 - 24480) / 2)
  an <- earnings_test(e, 2026, 1964, 6, entitled_from = 745, grace_year = "annual")
  expect_equal(an$excess, (e - 24480) / 2)
  expect_equal(earnings_test(e, 2026, 1964, 6, entitled_from = 745,
                             grace_year = "none")$excess, 0)
  # not the first year: the whole year counts
  expect_equal(earnings_test(e, 2027, 1964, 6, entitled_from = 745)$excess,
               (e - current_law()$ret_lower[yi(2027)]) / 2)
})

test_that("before entitlement nothing is charged", {
  expect_equal(earnings_test(1e5, 2025, 1964, 6, entitled_from = 745)$excess, 0)
})

test_that("rules before 2000: test to 72 (to 1982) or 70, 1/3 at 65-69 from 1990", {
  # born June 1920: 65 in 1985, 70 in June 1990
  r85 <- earnings_test(20000, 1985, 1920, 6, entitled_from = 745)
  expect_equal(r85$exempt, 7320)
  expect_equal(r85$rate, 1 / 2)
  expect_equal(r85$excess, (20000 - 7320) / 2)
  # at 63 in 1983: the lower amount
  expect_equal(earnings_test(20000, 1983, 1920, 6, entitled_from = 745)$excess,
               (20000 - 4920) / 2)
  # 1990, the year of attaining 70: January-May tested, at 1/3
  r90 <- earnings_test(36000, 1990, 1920, 6, entitled_from = 745)
  expect_equal(r90$tested_months, 5)
  expect_equal(r90$excess, floor((36000 * 5 / 12 - 9360) / 3))
  expect_equal(earnings_test(36000, 1991, 1920, 6, entitled_from = 745)$excess, 0)
  # born June 1912: 71 in 1983 is past the test; at 70 in 1982 still tested
  expect_equal(earnings_test(20000, 1982, 1912, 6, entitled_from = 780)$excess,
               (20000 - 6000) / 2)
  expect_equal(earnings_test(20000, 1983, 1912, 6, entitled_from = 780)$excess, 0)
})

test_that("repealing the test charges nothing", {
  p <- policy_replace(current_law(), ret_enabled = FALSE)
  expect_equal(earnings_test(1e5, 2026, 1960, 6, entitled_from = 745, policy = p)$excess, 0)
})

test_that("bad inputs are refused", {
  expect_error(earnings_test(1, 1977, 1915, 6), "1978")
  expect_error(earnings_test(-1, 2026, 1960, 6), "negative")
})
