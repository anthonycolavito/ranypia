# Actuarial Note 2026.3, Table 6: the scaled factors to three decimals.
TABLE6 <- list(
  very_low = c(.069, .085, .109, .130, .147, .161, .175, .187, .198, .207, .216, .224, .231,
               .237, .242, .247, .251, .255, .258, .261, .264, .266, .268, .270, .271, .272,
               .273, .273, .273, .273, .273, .272, .270, .268, .265, .260, .255, .248, .241,
               .232, .219, .212, .205, .197),
  low = c(.124, .153, .196, .235, .264, .290, .314, .336, .356, .373, .389, .403, .415, .426,
          .436, .445, .453, .459, .465, .470, .475, .479, .482, .486, .488, .490, .491, .492,
          .492, .492, .491, .489, .486, .482, .477, .468, .458, .447, .434, .417, .395, .381,
          .368, .355),
  medium = c(.276, .341, .436, .521, .587, .645, .699, .748, .791, .829, .863, .895, .923,
             .948, .970, .989, 1.006, 1.020, 1.033, 1.045, 1.055, 1.064, 1.071, 1.079, 1.085,
             1.089, 1.092, 1.093, 1.093, 1.093, 1.091, 1.086, 1.080, 1.071, 1.060, 1.041,
             1.018, .993, .964, .927, .878, .847, .818, .790),
  high = c(.442, .546, .698, .834, .940, 1.032, 1.118, 1.196, 1.265, 1.327, 1.381, 1.432,
           1.476, 1.516, 1.552, 1.582, 1.609, 1.632, 1.653, 1.672, 1.689, 1.702, 1.714, 1.727,
           1.735, 1.742, 1.747, 1.749, 1.749, 1.749, 1.746, 1.738, 1.728, 1.714, 1.696, 1.665,
           1.629, 1.589, 1.543, 1.483, 1.404, 1.356, 1.309, 1.264)
)

test_that("the unrounded factors round to the note's Table 6", {
  f <- scaled_factors(digits = 3)
  expect_identical(f$age, 21:64)
  for (type in names(TABLE6)) expect_equal(f[[type]], TABLE6[[type]], label = type)
})

test_that("career averages are 25, 45, 100 and 160 percent of the AWI", {
  f <- scaled_factors()
  top35 <- function(x) mean(sort(x, decreasing = TRUE)[1:35])
  expect_equal(vapply(f[-1], top35, 0), c(very_low = 0.25, low = 0.45, medium = 1, high = 1.6),
               tolerance = 1e-4)
})

test_that("the high earner matches the note's Table 7 examples to the cent", {
  e <- stylized_earnings(c(1949, 1973), type = "high")
  # 1949 at ages 21, 30, 40, 50, 64
  e49 <- e$earnings[e$id == 1 & e$age %in% c(21, 30, 40, 50, 64)]
  expect_equal(e49, c(2733.76, 15230.13, 33598.19, 53280.46, 56728.31))
  # 1973, every age
  expect_equal(e$earnings[e$id == 2], ranypia:::T7_EARNINGS)
  expect_equal(e$year[e$id == 2], 1994:2037)
})

test_that("custom levels scale the medium profile; presets are levels", {
  expect_equal(stylized_earnings(1970, level = 0.45)$earnings,
               stylized_earnings(1970, type = "low")$earnings)
  med <- stylized_earnings(1970)
  e110 <- stylized_earnings(1970, level = 1.1)
  expect_equal(e110$earnings, med$earnings * 1.1, tolerance = 1e-6)
  # career average indexed to 64: 110% of that year's AWI
  awi <- current_law()$awi
  ind <- e110$earnings * awi[2034 - 1936] / awi[e110$year - 1936]
  expect_equal(mean(sort(ind, decreasing = TRUE)[1:35]) / awi[2034 - 1936], 1.1,
               tolerance = 1e-4)
})

test_that("steady workers earn a fixed share of the AWI", {
  awi <- current_law()$awi
  e <- stylized_earnings(1980, level = 1.1, shape = "steady", start_age = 22)
  expect_identical(e$age, 22:64)
  expect_equal(e$earnings, 1.1 * awi[e$year - 1936], tolerance = 1e-7)
  s <- stylized_earnings(1980, type = "medium", shape = "steady")
  expect_equal(s$earnings, awi[s$year - 1936])
})

test_that("the maximum earner earns the taxable maximum", {
  e <- stylized_earnings(1960, type = "max", start_age = 22)
  expect_identical(e$earnings, current_law()$taxmax[e$year - 1936])
})

test_that("vectorized workers stack in order, with ids", {
  e <- stylized_earnings(1960, type = c("very_low", "max"), id = c("a", "b"))
  expect_identical(unique(e$id), c("a", "b"))
  expect_identical(nrow(e), 88L)
  b <- retired_worker(e, people = data.frame(id = c("a", "b"), birth_year = 1960,
                                             birth_month = 1, birth_day = 2,
                                             claim_age = 65 * 12))
  expect_true(all(b$benefit > 0))
  expect_true(b$benefit[1] < b$benefit[2])
})

test_that("real levels use the GDP price index", {
  # a made-up index: 2% a year, 100 in 2026
  idx <- setNames(100 * 1.02^((1937:2105) - 2026), 1937:2105)
  pol <- policy_replace(current_law(), gdp_price_hist = idx, gdp_price_growth = NULL)
  s <- stylized_earnings(1970, real_level = 60000, price_year = 2026, shape = "steady",
                         policy = pol)
  expect_equal(s$earnings, 60000 * 1.02^(s$year - 2026), tolerance = 1e-7)
  # scaled: the career average is $60,000 in 2026 prices, carried to 2034
  k <- stylized_earnings(1970, real_level = 60000, policy = pol)
  share <- 60000 * 1.02^(2034 - 2026) / pol$awi[2034 - 1936]
  expect_equal(k$earnings, stylized_earnings(1970, level = share, policy = pol)$earnings)
})

test_that("projected prices compound the growth rates", {
  pol <- policy_replace(current_law(), gdp_price_hist = c(`2025` = 100),
                        gdp_price_growth = setNames(rep(2, 80), 2026:2105))
  expect_equal(pol$gdp_price[2027 - 1936], 100 * 1.02^2)
})

test_that("bad inputs name the problem", {
  expect_error(stylized_earnings(1960, type = "average"), "type: must be one of")
  expect_error(stylized_earnings(1960, type = "low", level = 1), "not both")
  expect_error(stylized_earnings(1960, level = 1, real_level = 5e4), "not both")
  expect_error(stylized_earnings(1960, start_age = 18), "below 21")
  expect_error(stylized_earnings(1960, end_age = 66), "above 64")
  expect_error(stylized_earnings(1960, shape = "flat"), "shape")
  expect_error(stylized_earnings(1960, level = -1), "negative")
  no_prices <- policy_replace(current_law(), gdp_price_hist = NULL, gdp_price_growth = NULL)
  expect_error(stylized_earnings(1960, real_level = 5e4, policy = no_prices),
               "needs a price index")
  expect_error(stylized_earnings(1930, real_level = 5e4, shape = "steady"),
               "gdp_price: no value for year 1951")
})

test_that("current law carries the Trustees' GDP price index", {
  path <- test_path("../../data-raw/gdp_price_index_2026.csv")
  skip_if_not(file.exists(path), "data-raw is not in the built package")
  gdp <- utils::read.csv(path)
  for (alt in 1:3) {
    want <- ifelse(is.na(gdp$Historical), gdp[[c("Low", "Intermediate", "High")[alt]]],
                   gdp$Historical)
    expect_equal(current_law(alt)$gdp_price[gdp$Year - 1936], want, tolerance = 1e-12)
  }
  # a steady real earner earns the real amount in the price year
  s <- stylized_earnings(1970, real_level = 60000, price_year = 2026, shape = "steady")
  expect_equal(s$earnings[s$year == 2026], 60000)
})
