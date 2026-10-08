# Statutory rules pyanypia does not model, so there is no fixture oracle:
# each case is worked by hand from the POMS section or regulation cited.

# ---- widow(er)'s limit (POMS RS 00615.320) ----

test_that("RS 00615.320 example: the limit is the larger of the reduced RIB and 82.5%", {
  # PIA $374.90, reduced RIB $350; widow 66, at her full retirement age.
  # 82.5% of $374.90 = $309.29, down to $309.20; the limit is $350.
  widow <- auxiliary("widow", 1954, 6, claim_age = 66 * 12,
                     worker_factor = 350 / 374.90)
  fam <- family_benefits(374.90, 1000, list(widow), 2020, 6, survivor = TRUE)
  expect_identical(fam$after_max, 374.90)
  expect_identical(fam$benefit, 350)
  # with a deeper reduction, 82.5% of the PIA is the larger
  widow2 <- auxiliary("widow", 1954, 6, claim_age = 66 * 12, worker_factor = 0.75)
  expect_identical(family_benefits(374.90, 1000, list(widow2), 2020, 6,
                                   survivor = TRUE)$benefit, 309)
})

test_that("the limit only binds when the widow(er)'s own reduced benefit is higher", {
  # widow claims at 60: 28.5% reduction already takes her below the limit
  widow <- auxiliary("widow", 1964, 6, claim_age = 60 * 12, worker_factor = 0.75)
  limited <- family_benefits(2000, 4000, list(widow), 2024, 6, survivor = TRUE)
  plain <- family_benefits(2000, 4000, list(auxiliary("widow", 1964, 6, claim_age = 60 * 12)),
                           2024, 6, survivor = TRUE)
  expect_identical(limited$benefit, plain$benefit)
  expect_identical(plain$benefit, floor(2000 - ceiling(2000 * 0.285 * 10) / 10))
})

test_that("a worker who claimed at 62 limits a widow at NRA to 82.5% of the PIA", {
  # worker born June 1964, average wage every year 22-66, claims at 62 and
  # 1 month; his widow (same birth date) claims at her NRA in June 2034
  e <- setNames(current_law()$awi[(1986:2030) - 1936], 1986:2030)
  w <- retired_worker(e, 1964, 6, 62 * 12 + 1, benefit_age = 70 * 12)
  widow <- auxiliary("widow", 1964, 6, claim_age = 67 * 12, worker_factor = w$factor)
  fam <- family_benefits(w$pia, w$mfb, list(widow), 2034, 6, survivor = TRUE)
  expect_identical(w$benefit, 2266)
  expect_identical(fam$after_max, w$pia)  # unlimited, she would get the full PIA
  expect_identical(fam$benefit, floor(floor(0.825 * w$pia * 10 + 0.0005) / 10))
  expect_identical(fam$benefit, 2656)
})

test_that("a disabled widow(er) is limited too", {
  dw <- auxiliary("disabled_widow", 1980, 6, claim_age = 55 * 12, worker_factor = 0.5)
  fam <- family_benefits(2000, 4000, list(dw), 2035, 6, survivor = TRUE)
  # 71.5% of $2,000 = $1,430 is below the limit max($1,650, $1,000): no change
  expect_identical(fam$benefit, 1430)
})

# ---- the deceased worker's delayed credits (RS 00615.706, 20 CFR 404.313(e)) ----

test_that("a worker's benefit with delayed credits becomes the widow(er)'s base", {
  widow <- auxiliary("widow", 1960, 3, claim_age = 67 * 12, worker_factor = 1.24)
  fam <- family_benefits(2000, 4000, list(widow), 2030, 6, survivor = TRUE)
  expect_identical(fam$full, 2480)
  expect_identical(fam$benefit, 2480)
  # claiming early, the widow's reduction applies to that base
  # (born 1963: a widow(er)'s full retirement age of 67, RS 00615.301)
  early <- auxiliary("widow", 1963, 3, claim_age = 62 * 12, worker_factor = 1.24)
  f2 <- family_benefits(2000, 4000, list(early), 2030, 6, survivor = TRUE)
  red <- 1 - (67 * 12 - 62 * 12) / (67 * 12 - 720) * 0.285
  expect_identical(f2$benefit, floor(floor(red * 2480 * 10 + 0.0005) / 10))
  # no credits and no early claim (factor 1, or NA for never entitled): no change
  for (wf in c(1, NA)) {
    f3 <- family_benefits(2000, 4000, list(auxiliary("widow", 1960, 3, claim_age = 67 * 12,
                                                     worker_factor = wf)),
                          2030, 6, survivor = TRUE)
    expect_identical(f3$benefit, 2000)
  }
})

# ---- dual entitlement (RS 00615.020) ----

test_that("method C: a spouse gets the excess over the own PIA, reduced for age", {
  # spouse benefit $1,000 full; own PIA $400; excess $600, reduced for
  # 36 months early (25/36 of 1% a month = 25%) to $450
  sp <- auxiliary("spouse", 1965, 6, claim_age = 64 * 12, own_pia = 400, own_factor = 0.8)
  fam <- family_benefits(2000, 5000, list(sp), 2029, 6)
  expect_identical(fam$full, 1000)
  expect_identical(fam$reduction_factor, 0.75)
  expect_identical(fam$benefit, 450)
  # an own PIA above half the worker's leaves nothing on this record
  sp2 <- auxiliary("spouse", 1965, 6, claim_age = 67 * 12, own_pia = 1200)
  expect_identical(family_benefits(2000, 5000, list(sp2), 2032, 6)$benefit, 0)
})

test_that("method B: a widow(er) gets the reduced widow benefit less the reduced own", {
  # RS 00615.020's example: widow $1,000 reduced to $900, own RIB $400
  # reduced to $380, excess $520. Here the widow claims at NRA ($1,000).
  wd <- auxiliary("widow", 1965, 6, claim_age = 67 * 12, own_pia = 400, own_factor = 0.95)
  fam <- family_benefits(1000, 2000, list(wd), 2032, 6, survivor = TRUE)
  expect_identical(fam$benefit, 1000 - 380)
  # the limit applies before the own benefit is taken off
  wl <- auxiliary("widow", 1965, 6, claim_age = 67 * 12, worker_factor = 0.75,
                  own_pia = 400, own_factor = 0.95)
  expect_identical(family_benefits(1000, 2000, list(wl), 2032, 6, survivor = TRUE)$benefit,
                   825 - 380)
})

# ---- family maximum with a dually entitled member (RS 00615.768) ----

test_that("RS 00615.768 example 1: a spouse reduced to zero frees room for the children", {
  # PIA $400, maximum $650; spouse B (own PIA $100, unreduced at 66) and two
  # children all start at $83.30; B's own PIA takes B to $0, and the
  # children share the $250 left: $125 each
  aux <- list(auxiliary("spouse", 1950, 1, claim_age = 66 * 12, own_pia = 100),
              auxiliary("child", 2010, 1), auxiliary("child", 2012, 1))
  fam <- family_benefits(400, 650, aux, 2016, 6)
  expect_identical(fam$after_max, c(83.3, 125, 125))
  expect_identical(fam$benefit, c(0, 125, 125))
  # before October 1999 the old rule applies: the children stay at $83.30
  old <- family_benefits(400, 650, aux, 1999, 9)
  expect_identical(old$after_max, c(83.3, 83.3, 83.3))
})

test_that("RS 00615.768 example 2: a child with a partial payable", {
  # PIA $600, maximum $900, two children at $150; HC2's own PIA is $120, so
  # HC2 gets $30 and HC1 $270, never above HC1's full $300
  aux <- list(auxiliary("child", 2012, 1, own_pia = 120), auxiliary("child", 2010, 1))
  fam <- family_benefits(600, 900, aux, 2016, 6)
  expect_identical(fam$benefit, c(30, 270))
})

test_that("when every member is dually entitled the shares are not redistributed", {
  aux <- list(auxiliary("child", 2012, 1, own_pia = 120), auxiliary("child", 2010, 1,
                                                                       own_pia = 100))
  fam <- family_benefits(600, 900, aux, 2016, 6)
  expect_identical(fam$after_max, c(150, 150))
  expect_identical(fam$benefit, c(30, 50))
})

test_that("no own benefit (NA or 0) leaves the result unchanged", {
  base <- family_benefits(2000, 3500, list(auxiliary("spouse", 1965, 6, claim_age = 67 * 12),
                                           auxiliary("child", 2015, 1)), 2032, 6)
  for (op in c(NA, 0)) {
    got <- family_benefits(2000, 3500,
                           list(auxiliary("spouse", 1965, 6, claim_age = 67 * 12, own_pia = op),
                                auxiliary("child", 2015, 1)), 2032, 6)
    expect_identical(got, base)
  }
})

# ---- termination ages (20 CFR 404.352) ----

test_that("a child stops in the month they attain 18, and the others share the room", {
  # born 15 March 2014: attains 18 in March 2032; paid through February
  kids <- list(auxiliary("child", 2014, 3, end_age = 18 * 12),
               auxiliary("child", 2016, 3, end_age = 18 * 12),
               auxiliary("child", 2018, 3, end_age = 18 * 12))
  feb <- family_benefits(2000, 3500, kids, 2032, 2)
  mar <- family_benefits(2000, 3500, kids, 2032, 3)
  expect_true(feb$benefit[1] > 0)
  expect_identical(mar$benefit[1], 0)
  expect_true(all(mar$benefit[2:3] > feb$benefit[2:3]))
  # born on the 1st: attains 18 the month before the birthday month
  first <- auxiliary("child", 2014, 3, birth_day = 1, end_age = 18 * 12)
  expect_identical(family_benefits(2000, 3500, list(first), 2032, 2)$benefit, 0)
  # without end_age nothing changes, as before
  expect_true(family_benefits(2000, 3500, list(auxiliary("child", 2014, 3)), 2033, 3)$benefit > 0)
})

test_that("bad inputs name the problem", {
  expect_error(auxiliary("spouse", worker_factor = 0.8), "worker_factor")
  expect_error(family_benefits(2000, 3500, list(auxiliary("widow", worker_factor = -1)),
                               2030, survivor = TRUE), "worker_factor")
  expect_error(family_benefits(2000, 3500, list(auxiliary("spouse", 1965, 6, claim_age = 800,
                                                          own_pia = -5)), 2032), "own_pia")
})
