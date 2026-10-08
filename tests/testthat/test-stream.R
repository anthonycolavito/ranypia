# benefit_stream(): every row must equal retired_worker() at that benefit age.

awi_earnings <- function(years = 1986:2030, scale = 1) {
  setNames(current_law()$awi[years - 1936] * scale, years)
}

test_that("a yearly stream equals retired_worker() at each benefit age", {
  e <- awi_earnings()
  s <- benefit_stream(e, 1964, 6, claim_age = 67 * 12, to_age = 100 * 12)
  ages <- seq(67 * 12, 100 * 12, by = 12)
  want <- retired_worker(e, 1964, 6, 67 * 12, benefit_age = ages)
  expect_equal(nrow(s), length(ages))
  expect_identical(s$benefit_age, ages)
  for (col in c("aime", "pia", "mfb", "factor", "benefit", "method", "insured")) {
    expect_identical(s[[col]], want[[col]], label = col)
  }
  expect_identical(s$benefit[1], retired_worker(e, 1964, 6, 67 * 12)$benefit)
  expect_equal(s$worker, rep(1, nrow(s)))
})

test_that("benefit months are dated from the adjusted birth month", {
  # born 15 June 1964: 67 in June 2031; born 1 June: 67 in May 2031
  s <- benefit_stream(awi_earnings(), 1964, 6, 67 * 12, to_age = 68 * 12)
  expect_equal(s$benefit_year, c(2031, 2032))
  expect_equal(s$benefit_month, c(6, 6))
  s1 <- benefit_stream(awi_earnings(), 1964, 6, 67 * 12, to_age = 67 * 12, birth_day = 1)
  expect_equal(c(s1$benefit_year, s1$benefit_month), c(2031, 5))
})

test_that("monthly steps, uneven end ages and chunking", {
  e <- awi_earnings()
  m <- benefit_stream(e, 1964, 6, 62 * 12 + 1, to_age = 64 * 12, step = 1)
  expect_equal(nrow(m), 64 * 12 - (62 * 12 + 1) + 1)
  expect_identical(m$benefit, retired_worker(e, 1964, 6, 62 * 12 + 1,
                                             benefit_age = m$benefit_age)$benefit)
  # the stream stops at the last step at or before to_age
  odd <- benefit_stream(e, 1964, 6, 67 * 12, to_age = 69 * 12 + 5)
  expect_equal(odd$benefit_age, c(804, 816, 828))
  # chunk size never changes results
  expect_identical(benefit_stream(e, 1964, 6, 67 * 12, to_age = 80 * 12, chunk_size = 3),
                   benefit_stream(e, 1964, 6, 67 * 12, to_age = 80 * 12))
})

test_that("several workers, per-worker end ages, and panel input with people", {
  years <- 1986:2030
  panel <- data.frame(id = rep(c(10, 20), each = length(years)),
                      year = rep(years, 2),
                      earnings = c(awi_earnings(years, 0.5), awi_earnings(years, 2)))
  people <- data.frame(id = c(20, 10, 20), birth_year = 1964, birth_month = 6,
                       claim_age = c(70, 67, 62) * 12 + c(0, 0, 1),
                       to_age = c(85, 90, 75) * 12)
  s <- benefit_stream(panel, people = people)
  # earnings order (10, then 20), people order within id 20
  expect_equal(unique(s$id), c(10, 20))
  expect_equal(nrow(s), (90 - 67 + 1) + (85 - 70 + 1) + (74 - 62 + 1))
  one <- s[s$id == 20 & s$claim_age == 70 * 12, ]
  want <- retired_worker(awi_earnings(years, 2), 1964, 6, 70 * 12,
                         benefit_age = seq(70 * 12, 85 * 12, by = 12))
  expect_identical(one$benefit, want$benefit)
})

test_that("work after claiming is recomputed into later benefits", {
  e <- c(awi_earnings(1986:2030), awi_earnings(2031:2033, 3))
  s <- benefit_stream(e, 1964, 6, 62 * 12 + 1, to_age = 72 * 12)
  expect_true(s$aime[nrow(s)] > s$aime[1])
  expect_identical(s$benefit, retired_worker(e, 1964, 6, 62 * 12 + 1,
                                             benefit_age = s$benefit_age)$benefit)
})

test_that("reforms pass through", {
  e <- awi_earnings(1986:2040)
  nra69 <- policy_with_series(current_law(), "nra", setNames(rep(828, 74), 2032:2105))
  s <- benefit_stream(e, 1975, 6, 67 * 12, to_age = 70 * 12, policy = nra69)
  expect_identical(s$benefit, retired_worker(e, 1975, 6, 67 * 12, policy = nra69,
                                             benefit_age = s$benefit_age)$benefit)
  expect_true(all(s$factor < 1))
})

test_that("bad arguments name the problem", {
  e <- awi_earnings()
  expect_error(benefit_stream(e, 1964, 6, 67 * 12, to_age = 66 * 12), "to_age")
  expect_error(benefit_stream(e, 1964, 6, 67 * 12, step = 0), "step")
  expect_error(benefit_stream(e, 1964, 6, 67 * 12, step = 1.5), "step")
  expect_error(benefit_stream(e, 1964, 6, 62 * 12), "claim_age")
})
