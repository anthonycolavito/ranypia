# lifetime_benefits() with the earnings test: a worker's excess reaches the
# family on their record; an auxiliary's own excess reaches their own benefit.

awi <- function(years, scale = 1) current_law()$awi[years - 1936] * scale

family_case <- function(late_earnings = 40000) {
  yrs <- 1986:2025
  panel <- rbind(data.frame(id = 1, year = yrs, earnings = awi(yrs, 1.5)),
                 data.frame(id = 1, year = 2026:2030, earnings = late_earnings))
  people <- data.frame(id = 1:2, birth_year = 1964, birth_month = 6,
                       death_age = c(80, 90) * 12, claim_age = 745, spouse_id = c(2, 1))
  kids <- data.frame(id = 9, parent_id = 1, birth_year = 2012, birth_month = 3)
  list(panel = panel, people = people, kids = kids)
}

test_that("off by default; on, the amounts due before the test are unchanged before NRA", {
  f <- family_case()
  a <- lifetime_benefits(f$panel, f$people, f$kids)
  b <- lifetime_benefits(f$panel, f$people, f$kids, earnings_test = TRUE)
  expect_false("withheld" %in% names(a))
  pre <- a$benefit_year < 2031
  expect_equal(b$total[pre], a$total[pre])
  expect_equal(b$paid, b$total - b$withheld)
})

test_that("a retired worker's excess is withheld from the whole family on the record", {
  f <- family_case()
  b <- lifetime_benefits(f$panel, f$people, f$kids, earnings_test = TRUE)
  y <- b[b$benefit_year == 2027, ]
  excess <- (40000 - current_law()$ret_lower[yi(2027)]) / 2
  expect_equal(sum(y$withheld), excess, tolerance = 0.05)
  jan <- y[y$benefit_month == 1, ]
  expect_equal(jan$withheld, jan$total)          # January: everyone, in full
  feb <- y[y$benefit_month == 2, ]
  # February in part, shared in proportion to each benefit
  expect_equal(feb$withheld / feb$total, rep(feb$withheld[1] / feb$total[1], 3),
               tolerance = 1e-4)
  expect_true(all(y$withheld[y$benefit_month > 2] == 0))
})

test_that("months with a deduction come out of the worker's and the spouse's reduction", {
  f <- family_case()
  a <- lifetime_benefits(f$panel, f$people, f$kids)
  b <- lifetime_benefits(f$panel, f$people, f$kids, earnings_test = TRUE)
  post <- b$benefit_year == 2032
  expect_true(all(b$own_benefit[post & b$id == 1] > a$own_benefit[post & a$id == 1]))
  expect_true(all(b$aux_benefit[post & b$id == 2] > a$aux_benefit[post & a$id == 2]))
})

test_that("an auxiliary's own earnings reduce only their own benefit", {
  yrs <- 1986:2025
  panel <- rbind(data.frame(id = 1, year = yrs, earnings = awi(yrs, 1.5)),
                 data.frame(id = 2, year = 2026:2030, earnings = 40000))
  people <- data.frame(id = 1:2, birth_year = 1964, birth_month = 6,
                       death_age = c(80, 90) * 12, claim_age = 745, spouse_id = c(2, 1))
  b <- lifetime_benefits(panel, people, earnings_test = TRUE)
  expect_true(all(b$withheld[b$id == 1] == 0))
  y <- b[b$id == 2 & b$benefit_year == 2027, ]
  expect_equal(sum(y$withheld), (40000 - current_law()$ret_lower[yi(2027)]) / 2,
               tolerance = 0.05)
})

test_that("a widow(er)'s own earnings are tested to the survivor NRA", {
  yrs <- 1986:2025
  panel <- rbind(data.frame(id = 1, year = yrs, earnings = awi(yrs, 1.5)),
                 data.frame(id = 2, year = 2024:2031, earnings = 50000))
  # the worker dies at 60 (2024); the widow, born June 1966, claims at 60
  people <- data.frame(id = 1:2, birth_year = c(1964, 1966), birth_month = 6,
                       death_age = c(60, 90) * 12, claim_age = NA,
                       survivor_claim_age = c(NA, 720), spouse_id = c(2, 1))
  b <- lifetime_benefits(panel, people, earnings_test = TRUE)
  w <- b[b$id == 2, ]
  expect_gt(sum(w$withheld[w$benefit_year == 2027]), 0)
  # survivor NRA for 1966 is 67 (June 2033): nothing withheld from then on
  expect_true(all(w$withheld[w$age >= 804] == 0))
})

test_that("months picks rows after the test is run on every month", {
  f <- family_case()
  b <- lifetime_benefits(f$panel, f$people, f$kids, earnings_test = TRUE)
  b6 <- lifetime_benefits(f$panel, f$people, f$kids, earnings_test = TRUE, months = 6)
  expect_equal(b6, b[b$benefit_month == 6, ], ignore_attr = TRUE)
})

test_that("disability benefits are not tested", {
  e <- data.frame(id = 1, year = 1990:2030, earnings = 50000)
  people <- data.frame(id = 1, birth_year = 1970, birth_month = 3, death_age = 70 * 12,
                       onset_year = 2020, onset_month = 6)
  b <- lifetime_benefits(e, people, earnings_test = TRUE)
  expect_true(all(b$withheld == 0))
})
