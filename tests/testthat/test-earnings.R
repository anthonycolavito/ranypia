fx <- fixture("earnings")

test_that("AIME, indexing and QCs match pyanypia", {
  m <- fx$earnings
  expect_identical(aime(m, fx$elig_year, fx$comp_years, first_year = fx$first_year,
                        last_year = fx$last_year), fx$aime)
  expect_identical(indexed_earnings(m[1:20, ], fx$elig_year[1:20], first_year = fx$first_year),
                   fx$indexed)
  expect_identical(quarters_of_coverage(m[1:20, ], first_year = fx$first_year), fx$qcs)
})

test_that("computation years match pyanypia", {
  expect_identical(computation_years(fx$birth_year, fx$birth_month, fx$elig_year,
                                     fx$birth_day), fx$comp_years)
  expect_identical(computation_years(fx$birth_year, fx$birth_month, fx$elig_year,
                                     fx$birth_day, disabled = TRUE), fx$comp_disabled)
  expect_identical(computation_years(fx$birth_year, fx$birth_month, fx$elig_year,
                                     fx$birth_day, death_year = fx$birth_year + 40),
                   fx$comp_death)
})

test_that("years of coverage and insured status match pyanypia", {
  m <- fx$earnings
  expect_identical(years_of_coverage(m, fx$last_year, first_year = fx$first_year),
                   fx$yoc_specmin)
  expect_identical(years_of_coverage(m, fx$last_year, first_year = fx$first_year,
                                     kind = "wep"), fx$yoc_wep)
  expect_identical(fully_insured(m, fx$birth_year, fx$birth_month, fx$last_year,
                                 fx$through_month, first_year = fx$first_year,
                                 birth_day = fx$birth_day), fx$fully_insured)
  di <- disability_insured(m, fx$birth_year, fx$birth_month, fx$onset_year, fx$onset_month,
                           first_year = fx$first_year, birth_day = fx$birth_day)
  expect_identical(di, fx$disability_insured)
  expect_true(any(di) && !all(di))
})

test_that("vectorized AIME equals one row at a time", {
  m <- fx$earnings[1:30, ]
  whole <- aime(m, fx$elig_year[1:30], 35, first_year = fx$first_year)
  rows <- vapply(1:30, function(i) aime(m[i, ], fx$elig_year[i], 35,
                                         first_year = fx$first_year), numeric(1))
  expect_identical(whole, rows)
})

test_that("bad inputs are refused", {
  expect_error(aime(c("2000" = 1), 2030, 0), "comp_years: must be at least 1")
  expect_error(aime(c("2000" = -1), 2030, 35), "earnings: negative")
})

test_that("currently insured: 6 QCs in the 13 quarters ending with the given quarter", {
  recent <- setNames(rep(30000, 4), 2026:2029)
  expect_true(currently_insured(recent, 2030, 6))
  expect_false(fully_insured(recent, 1990, 1, 2030, 6))
  # the same four years, ending five years before: not currently insured
  expect_false(currently_insured(setNames(rep(30000, 4), 2021:2024), 2030, 6))
  # QCs in the window's first year count only from its first quarter: a
  # window ending 2031 Q3 starts 2028 Q3, leaving 2 + 4 = 6 (just enough);
  # ending 2031 Q4, it starts 2028 Q4, leaving 1 + 4 = 5
  two <- setNames(c(30000, 30000), 2028:2029)
  expect_true(currently_insured(two, 2031, 9))
  expect_false(currently_insured(two, 2031, 12))
  expect_identical(currently_insured(rbind(recent, recent * 0), 2030, 6), c(TRUE, FALSE))
})
