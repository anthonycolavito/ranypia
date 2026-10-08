# disabled_stream(): every row must equal disabled_worker() in that month.

di_earnings <- function(years = 1990:2020, scale = 1) {
  setNames(current_law()$awi[years - 1936] * scale, years)
}

test_that("a yearly stream equals disabled_worker() in each benefit month", {
  e <- di_earnings()
  s <- disabled_stream(e, 1970, 3, onset_year = 2021, onset_month = 6)
  # onset 15 June 2021: waiting period July-November, entitled December 2021
  expect_equal(c(s$benefit_year[1], s$benefit_month[1]), c(2021, 12))
  expect_equal(s$entitlement_age[1], (2021 - 1970) * 12 + 9)
  expect_equal(max(s$benefit_age), s$entitlement_age[1] + 12 * 48)  # last step before 100
  want <- disabled_worker(e, 1970, 3, 2021, 6, benefit = list(s$benefit_year, s$benefit_month))
  for (col in c("aime", "pia", "mfb", "factor", "benefit", "method", "insured")) {
    expect_identical(s[[col]], want[[col]], label = col)
  }
})

test_that("the benefit converts to a retirement benefit at NRA with nothing but COLAs", {
  m <- disabled_stream(di_earnings(), 1970, 3, 2021, 6, step = 1, to_age = 75 * 12)
  nra <- normal_retirement_age(1970, 3)
  expect_identical(m$type, ifelse(m$benefit_age < nra, "disabled", "retired"))
  expect_true(all(m$factor == 1))
  # the PIA changes only with a COLA, in December, across the conversion too
  changed <- which(diff(m$pia) != 0) + 1
  expect_true(all(m$benefit_month[changed] == 12))
  at_nra <- which(m$benefit_age == nra)
  expect_identical(m$pia[at_nra], m$pia[at_nra - 1])
})

test_that("a given entitlement month, per-worker end ages, and chunking", {
  e <- rbind(di_earnings(), di_earnings(scale = 2))
  s <- disabled_stream(e, c(1970, 1975), 3, c(2021, 2019), 6,
                       entitlement = list(c(2022, 2020), c(1, 7)),
                       to_age = c(70, 60) * 12)
  first <- s[!duplicated(s$worker), ]
  expect_equal(first$benefit_year, c(2022, 2020))
  expect_equal(first$benefit_month, c(1, 7))
  expect_true(all(s$benefit_age[s$worker == 2] <= 60 * 12))
  expect_identical(disabled_stream(e, c(1970, 1975), 3, c(2021, 2019), 6, chunk_size = 5),
                   disabled_stream(e, c(1970, 1975), 3, c(2021, 2019), 6))
})

test_that("panel input with people, including a child-care matrix", {
  years <- 1990:2020
  panel <- data.frame(id = rep(c(1, 2), each = length(years)), year = rep(years, 2),
                      earnings = c(di_earnings(years), di_earnings(years, 1.5)))
  people <- data.frame(id = c(2, 1), birth_year = c(1975, 1970), birth_month = 3,
                       onset_year = c(2019, 2021), onset_month = 6, to_age = 80 * 12)
  cc <- matrix(FALSE, 2, length(years), dimnames = list(NULL, years))
  cc[2, as.character(2003:2005)] <- TRUE
  s <- disabled_stream(panel, people = people, childcare = cc)
  expect_equal(unique(s$id), c(1, 2))
  two <- s[s$id == 2, ]
  want <- disabled_worker(di_earnings(years, 1.5), 1975, 3, 2019, 6,
                          benefit = list(two$benefit_year, two$benefit_month),
                          childcare = cc[rep(2, nrow(two)), , drop = FALSE])
  expect_identical(two$benefit, want$benefit)
})

test_that("bad arguments name the problem", {
  e <- di_earnings()
  expect_error(disabled_stream(e, 1970, 3, 2021, 6, to_age = 50 * 12), "to_age")
  expect_error(disabled_stream(e, 1950, 3, 2021, 6), "entitlement")
  expect_error(disabled_stream(e, 1970, 3, 2021, 6, step = 0), "step")
})
