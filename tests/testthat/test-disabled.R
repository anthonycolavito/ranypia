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

test_that("disabled_worker matches pyanypia past NRA, apart from the converted maximum", {
  fx <- fixture("disabled_lifetime")
  i <- fx$inputs
  want <- fx$outputs
  got <- disabled_worker(i$earnings, i$birth_year, i$birth_month, i$onset_year,
                         i$onset_month, first_year = i$first_year, onset_day = i$onset_day,
                         entitlement = list(i$ent_year, i$ent_month),
                         benefit = list(i$ben_year, i$ben_month), childcare = i$childcare)
  for (col in c("elig_year", "aime", "pia_elig", "pia", "nra", "factor", "benefit",
                "method", "insured")) {
    expect_identical(got[[col]], want[[col]], label = col)
  }
  # before NRA the disability maximum, exactly as pyanypia; from NRA the
  # regular maximum on the same PIA (sec. 203(a)(6), POMS RS 00615.742),
  # where pyanypia and AnyPIA keep the disability maximum
  age <- (i$ben_year * 12 + i$ben_month) - (i$birth_year * 12 + i$birth_month)
  conv <- age >= got$nra
  expect_true(sum(conv) > 100 && sum(!conv) > 20)
  expect_identical(got$mfb[!conv], want$mfb[!conv])
  sm <- got$method == "special_minimum"
  regular <- apply_colas(family_max(got$pia_elig, got$elig_year), got$elig_year,
                         i$ben_year, i$ben_month)
  expect_identical(got$mfb[conv & !sm], regular[conv & !sm])
  expect_true(all(got$mfb[conv & !sm] >= want$mfb[conv & !sm]))
})

test_that("a disabled worker's survivors: the PIA with the freeze, the regular maximum", {
  # disabled at 45 in 2015 with no earnings after; dies in 2028
  e <- setNames(current_law()$awi[(1992:2014) - 1936], 1992:2014)
  plain <- deceased_worker(e, 1970, 3, 2028, list(2029, 6))
  frz <- deceased_worker(e, 1970, 3, 2028, list(2029, 6), onset_year = 2015, onset_month = 6)
  di <- disabled_worker(e, 1970, 3, 2015, 6, benefit = list(2029, 6))
  # without the freeze, 13 zero years and indexing to 2026 drag the PIA down
  expect_lt(plain$pia, di$pia)
  expect_identical(frz$pia, di$pia)
  expect_identical(frz$elig_year, di$elig_year)
  expect_identical(frz$method, "disability_freeze")
  expect_identical(frz$mfb, apply_colas(family_max(di$pia_elig, di$elig_year),
                                        di$elig_year, 2029, 6))
  # NA onset leaves a worker as before; vectors mix
  both <- deceased_worker(rbind(e, e), 1970, 3, 2028, list(2029, 6),
                          onset_year = c(NA, 2015), onset_month = c(NA, 6))
  expect_identical(both$pia, c(plain$pia, frz$pia))
  # a worker not disability insured at onset gets no freeze
  thin <- setNames(c(rep(0, 20), 30000, 30000, 30000), 1992:2014)
  expect_false(disabled_worker(thin, 1970, 3, 2015, 6)$insured)
  expect_identical(deceased_worker(thin, 1970, 3, 2028, list(2029, 6), onset_year = 2015,
                                   onset_month = 6)$method,
                   deceased_worker(thin, 1970, 3, 2028, list(2029, 6))$method)
})
