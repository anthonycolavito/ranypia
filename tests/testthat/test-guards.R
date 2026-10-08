# ranypia covers the wage-indexed method only: eligibility in 1979 or later.

e <- setNames(current_law()$awi[(1951:1990) - 1936], 1951:1990)

test_that("eligibility before 1979 is refused", {
  expect_error(retired_worker(e, 1916, 6, 65 * 12), "before 1979")
  # born 1 January 1917: attains 62 on 31 December 1978
  expect_error(retired_worker(e, 1917, 1, 65 * 12, birth_day = 1), "before 1979")
  expect_error(deceased_worker(e, 1930, 6, 1977, list(1980, 6)), "before 1979")
  expect_error(disabled_worker(e, 1930, 6, 1978, 3, entitlement = list(1980, 7)), "before 1979")
  expect_error(retired_worker(rbind(e, e), c(1930, 1916), 6, 65 * 12), "1 row\\(s\\); first at row 2")
})

test_that("births in 1917-1921 warn that the transitional guarantee is not computed", {
  expect_warning(retired_worker(e, 1917, 6, 65 * 12), class = "ranypia_transitional_guarantee")
  expect_warning(retired_worker(e, 1921, 12, 65 * 12), "1 worker\\(s\\)")
  expect_warning(retired_worker(rbind(e, e, e), c(1918, 1920, 1930), 6, 65 * 12), "2 worker\\(s\\)")
  expect_warning(deceased_worker(e, 1925, 6, 1981, list(1982, 6)),
                 class = "ranypia_transitional_guarantee")
  expect_no_warning(retired_worker(e, 1922, 6, 65 * 12))
  # disability benefits have no transitional guarantee
  expect_no_warning(disabled_worker(e, 1919, 6, 1980, 3))
})
