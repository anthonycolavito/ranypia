test_that("retired_worker matches pyanypia under all three alternatives", {
  for (alt in 1:3) {
    fx <- fixture(paste0("retired_alt", alt))
    i <- fx$inputs
    got <- retired_worker(i$earnings, i$birth_year, i$birth_month, i$claim_age,
                          first_year = i$first_year, birth_day = i$birth_day,
                          benefit_age = i$benefit_age, policy = current_law(alt))
    expect_outputs(got, fx$outputs)
  }
})

test_that("the sweep includes special minimum and uninsured workers", {
  o <- fixture("retired_alt2")$outputs
  expect_true(any(o$method == "special_minimum"))
  expect_true(any(!o$insured))
})

test_that("vectorized equals one worker at a time", {
  i <- fixture("retired_alt2")$inputs
  whole <- retired_worker(i$earnings[1:40, ], i$birth_year[1:40], i$birth_month[1:40],
                          i$claim_age[1:40], first_year = i$first_year,
                          birth_day = i$birth_day[1:40])
  for (r in 1:40) {
    one <- retired_worker(i$earnings[r, ], i$birth_year[r], i$birth_month[r], i$claim_age[r],
                          first_year = i$first_year, birth_day = i$birth_day[r])
    expect_identical(one$benefit, whole$benefit[r])
    expect_identical(one$mfb, whole$mfb[r])
  }
})

test_that("special minimum tables match pyanypia", {
  fx <- fixture("minimum")
  for (key in names(fx$tables)) {
    ym <- as.numeric(strsplit(key, "-")[[1]])
    sm <- special_minimum_pia(fx$yoc, ym[1], ym[2])
    expect_identical(sm$pia, fx$tables[[key]]$pia, label = key)
    expect_identical(sm$mfb, fx$tables[[key]]$mfb, label = key)
  }
})

test_that("long earnings and a people data frame give the same answer", {
  i <- fixture("retired_alt2")$inputs
  m <- i$earnings[1:25, ]
  years <- seq(i$first_year, length.out = ncol(m))
  panel <- data.frame(id = rep(sprintf("w%02d", 1:25), times = ncol(m)),
                      year = rep(years, each = 25), earnings = as.vector(m))
  panel <- panel[panel$earnings > 0, ]
  people <- data.frame(id = sprintf("w%02d", 25:1), birth_year = rev(i$birth_year[1:25]),
                       birth_month = rev(i$birth_month[1:25]),
                       birth_day = rev(i$birth_day[1:25]), claim_age = rev(i$claim_age[1:25]))
  from_panel <- retired_worker(panel, people = people)
  direct <- retired_worker(m, i$birth_year[1:25], i$birth_month[1:25], i$claim_age[1:25],
                           first_year = i$first_year, birth_day = i$birth_day[1:25])
  ord <- match(sprintf("w%02d", 1:25), from_panel$id)
  expect_identical(from_panel$benefit[ord], direct$benefit)
  expect_identical(names(from_panel)[1], "id")
})
