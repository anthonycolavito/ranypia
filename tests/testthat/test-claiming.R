fx <- fixture("claiming")

test_that("adjusted birth and COLA year", {
  kb <- adjusted_birth(c(1960, 1960, 1960), c(1, 3, 3), c(1, 1, 2))
  expect_identical(kb$year, c(1959, 1960, 1960))
  expect_identical(kb$month, c(12, 2, 3))
  expect_identical(cola_year(c(2026, 2026, 1980, 1980), c(11, 12, 5, 6)),
                   c(2025, 2026, 1979, 1980))
})

test_that("eligibility year", {
  expect_identical(eligibility_year(1960, 1, 1), 2021)
  expect_identical(eligibility_year(1960, 6, onset_year = 2005), 2005)
})

test_that("NRA and factors match pyanypia", {
  expect_identical(normal_retirement_age(fx$nra_birth_years, 6), fx$nra)
  expect_identical(early_reduction_factor(fx$months), fx$early)
  expect_identical(spouse_reduction_factor(fx$months), fx$spouse)
  for (i in seq_along(fx$drc_elig)) {
    expect_identical(delayed_credit_factor(fx$months, fx$drc_elig[i]), fx$drc[i, ])
  }
  expect_identical(earliest_claim_age(c(1, 2, 3, 15)), c(745, 744, 745, 745))
})

test_that("reduction and credit months, and factors, match pyanypia", {
  g <- fx$grid
  adj <- adjustment_months(g[, 1], g[, 2], g[, 4], g[, 3], g[, 5])
  expect_identical(adj$reduction, g[, 6])
  expect_identical(adj$credit, g[, 7])
  expect_identical(benefit_factor(g[, 1], g[, 2], g[, 4], g[, 3], g[, 5]), g[, 8])
})

test_that("claims before the earliest age are refused", {
  expect_error(benefit_factor(1960, 6, 744), "claim_age: before the earliest")
})

test_that("monthly benefit rounds then floors", {
  expect_identical(monthly_benefit(1234.5, 0.7, 2030, 6), 864)
})
