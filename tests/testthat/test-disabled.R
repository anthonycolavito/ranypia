run_fixture <- function(name, with_childcare) {
  fx <- fixture(name)
  i <- fx$inputs
  got <- disabled_worker(i$earnings, i$birth_year, i$birth_month, i$onset_year,
                         i$onset_month, first_year = i$first_year, onset_day = i$onset_day,
                         entitlement = list(i$ent_year, i$ent_month),
                         benefit = list(i$ben_year, i$ben_month),
                         childcare = if (with_childcare) i$childcare)
  expect_outputs(got, fx$outputs)
  got
}

test_that("disabled_worker matches pyanypia", {
  got <- run_fixture("disabled", FALSE)
  expect_true(any(!got$insured))
})

test_that("child-care dropout years match pyanypia", {
  run_fixture("disabled_childcare", TRUE)
})

test_that("a non-freeze winner takes the highest-AIME method's maximum", {
  run_fixture("disabled_nonfreeze", TRUE)
})

test_that("di_family_max matches pyanypia", {
  fx <- fixture("formula")
  expect_identical(di_family_max(fx$pia_in, fx$aime, fx$elig), fx$di_family_max)
})

test_that("default entitlement follows the waiting period", {
  b <- disabled_worker(setNames(rep(50000, 21), 2000:2020), 1975, 3, 2020, 6)
  expect_identical(b$elig_year, 2020)
})

test_that("disabled_worker matches pyanypia past NRA, out to age 100", {
  fx <- fixture("disabled_lifetime")
  i <- fx$inputs
  age <- (i$ben_year * 12 + i$ben_month) - (i$birth_year * 12 + i$birth_month)
  expect_true(mean(age >= 67 * 12) > 0.5)
  got <- run_fixture("disabled_lifetime", TRUE)
  expect_equal(nrow(got), 300)
})
