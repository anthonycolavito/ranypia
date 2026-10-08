# `people =` with several rows per id: each row is its own worker-scenario,
# not silently collapsed to the first match.

awi_earnings <- function(years = 1986:2030, scale = 1) {
  setNames(current_law()$awi[years - 1936] * scale, years)
}

panel_of <- function(scales, years = 1986:2030) {
  data.frame(id = rep(seq_along(scales), each = length(years)),
             year = rep(years, length(scales)),
             earnings = unlist(lapply(scales, function(s) awi_earnings(years, s))))
}

test_that("duplicate people ids give one row each, matching single-worker calls", {
  panel <- panel_of(c(1, 2))
  people <- data.frame(id = c(1, 1, 2), birth_year = 1964, birth_month = 6,
                       claim_age = c(62 * 12 + 1, 70 * 12, 67 * 12))
  got <- retired_worker(panel, people = people)
  expect_equal(nrow(got), 3)
  expect_equal(got$id, c(1, 1, 2))
  want <- rbind(
    retired_worker(awi_earnings(), 1964, 6, c(62 * 12 + 1, 70 * 12)),
    retired_worker(awi_earnings(scale = 2), 1964, 6, 67 * 12)
  )
  expect_identical(got$benefit, want$benefit)
  expect_identical(got$aime, want$aime)
})

test_that("output follows earnings order, then people order within an id", {
  panel <- panel_of(c(1, 2))
  people <- data.frame(id = c(2, 1, 2), birth_year = 1964, birth_month = 6,
                       claim_age = c(70 * 12, 67 * 12, 64 * 12))
  got <- retired_worker(panel, people = people)
  expect_equal(got$id, c(1, 2, 2))
  expect_identical(got$benefit, c(
    retired_worker(awi_earnings(), 1964, 6, 67 * 12)$benefit,
    retired_worker(awi_earnings(scale = 2), 1964, 6, c(70 * 12, 64 * 12))$benefit
  ))
})

test_that("unique ids behave exactly as before", {
  panel <- panel_of(c(0.5, 1, 2))
  people <- data.frame(id = 3:1, birth_year = 1964, birth_month = 6, claim_age = 67 * 12)
  got <- retired_worker(panel, people = people)
  expect_equal(got$id, 1:3)
  expect_identical(got$benefit, retired_worker(rbind(awi_earnings(scale = 0.5), awi_earnings(),
                                                     awi_earnings(scale = 2)),
                                               1964, 6, 67 * 12)$benefit)
})

test_that("an earnings id with no people row still errors", {
  panel <- panel_of(c(1, 2))
  people <- data.frame(id = c(1, 1), birth_year = 1964, birth_month = 6,
                       claim_age = c(64, 67) * 12)
  expect_error(retired_worker(panel, people = people), "no row for earnings id 2")
})

test_that("disabled_worker and its child-care matrix follow the row map", {
  panel <- panel_of(c(1, 1.5), years = 1990:2020)
  people <- data.frame(id = c(1, 2, 2), birth_year = 1970, birth_month = 3,
                       onset_year = c(2021, 2018, 2021), onset_month = 6)
  got <- disabled_worker(panel, people = people)
  expect_equal(got$id, c(1, 2, 2))
  m2 <- rbind(awi_earnings(1990:2020, 1.5), awi_earnings(1990:2020, 1.5))
  expect_identical(got$benefit[2:3],
                   disabled_worker(m2, 1970, 3, c(2018, 2021), 6)$benefit)

  cc <- matrix(FALSE, 2, 31, dimnames = list(NULL, 1990:2020))
  cc[2, as.character(2000:2002)] <- TRUE
  got_cc <- disabled_worker(panel, people = people, childcare = cc)
  want_cc <- disabled_worker(m2, 1970, 3, c(2018, 2021), 6,
                             childcare = cc[c(2, 2), , drop = FALSE])
  expect_identical(got_cc$benefit[2:3], want_cc$benefit)
})
