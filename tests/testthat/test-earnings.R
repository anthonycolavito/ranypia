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

test_that("qc_history replaces the pre-1978 approximation where given", {
  e <- setNames(c(800, 2000, 5000, 9000), 1975:1978)
  approx <- quarters_of_coverage(e)
  expect_equal(as.vector(approx), c(4, 4, 4, 4))
  got <- quarters_of_coverage(e, qc_history = c("1975" = 2, "1976" = NA))
  expect_equal(as.vector(got), c(2, 4, 4, 4))
  # a long record matches long earnings by id; other workers keep the rule
  panel <- data.frame(id = rep(c("a", "b"), each = 3), year = rep(1975:1977, 2),
                      earnings = 800)
  rec <- data.frame(id = "b", year = 1975:1977, qcs = c(1, 0, 3))
  q <- quarters_of_coverage(panel, qc_history = rec)
  expect_equal(unname(q[1, ]), c(4, 4, 4))
  expect_equal(unname(q[2, ]), c(1, 0, 3))
})

test_that("bad records name the problem", {
  e <- setNames(rep(800, 4), 1975:1978)
  expect_error(quarters_of_coverage(e, qc_history = c("1978" = 2)), "1951 to 1977")
  expect_error(quarters_of_coverage(e, qc_history = c("1975" = 5)), "0 to 4")
  expect_error(quarters_of_coverage(e, qc_history = c(2, 3)), "column names")
  panel <- data.frame(id = rep(1:2, each = 2), year = rep(1976:1977, 2), earnings = 800)
  expect_error(quarters_of_coverage(panel, qc_history = data.frame(id = 9, year = 1976, qcs = 1)),
               "no earnings")
  expect_error(quarters_of_coverage(panel, qc_history = data.frame(year = 1976, qcs = 1)),
               "id column")
})

test_that("actual records can change insured status", {
  # $300 a year 1955-1977 and nothing after: the annual rule gives 4 QCs a
  # year (92), but a record of one quarter a year gives 23, short of the 40
  # a worker born in 1930 needs
  e <- setNames(c(rep(300, 23), rep(0, 14)), 1955:1991)
  rec <- setNames(rep(1, 23), 1955:1977)
  expect_true(fully_insured(e, 1930, 6, 1992, 6))
  expect_false(fully_insured(e, 1930, 6, 1992, 6, qc_history = rec))
  expect_false(retired_worker(e, 1930, 6, 62 * 12 + 1, qc_history = rec)$insured)
  # the record follows a worker fanned out over several claim ages
  r2 <- retired_worker(e, 1930, 6, c(62, 65) * 12 + 1, qc_history = rec)
  expect_identical(r2$insured, c(FALSE, FALSE))
})
