test_that("named vectors fill gaps", {
  e <- ranypia:::as_earnings(c("2000" = 10, "2002" = 30))
  expect_identical(e$first, 2000)
  expect_identical(e$m, matrix(c(10, 0, 30), nrow = 1))
})

test_that("matrices need years", {
  expect_error(ranypia:::as_earnings(matrix(0, 2, 3)), "first_year")
  m <- matrix(1, 2, 3, dimnames = list(NULL, 2000:2002))
  expect_identical(ranypia:::as_earnings(m)$first, 2000)
})

test_that("negative earnings name the rows", {
  m <- matrix(0, 5, 3)
  m[4, 2] <- -1
  m[5, 1] <- -2
  expect_error(ranypia:::as_earnings(m, 2000),
               "earnings: negative in 2 row(s); first at row 4", fixed = TRUE)
})

test_that("earnings must fit the data years", {
  expect_error(ranypia:::as_earnings(rep(0, 5), 2104), "1937-2105")
})

test_that("long data become a matrix in id order", {
  panel <- data.frame(id = c("b", "b", "a"), year = c(2001, 2003, 2002),
                      earnings = c(1, 3, 2))
  m <- earnings_matrix(panel)
  expect_identical(attr(m, "ids"), c("b", "a"))
  expect_identical(unname(m[, "2002"]), c(0, 2))
  expect_identical(colnames(m), c("2001", "2002", "2003"))
})

test_that("year lookups refuse missing data", {
  s <- ranypia:::to_series(c("2000" = 1))
  expect_error(ranypia:::at(s, 1999, "cola"), "cola: no value for year 1999")
  expect_error(ranypia:::at(s, 2106, "awi"), "awi: year outside 1937-2105")
})

test_that("whole numbers are enforced", {
  expect_error(ranypia:::as_whole("claim_age", 744.5), "claim_age: must be whole numbers")
})
