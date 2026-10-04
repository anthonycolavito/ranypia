fx <- fixture("formula")

test_that("bend points match pyanypia", {
  expect_identical(bend_points(fx$bp_years), fx$bend_points)
  expect_identical(family_max_bend_points(fx$bp_years), fx$mfb_bend_points)
})

test_that("PIA and family maximum match pyanypia", {
  expect_identical(pia(fx$aime, fx$elig), fx$pia)
  expect_identical(family_max(fx$pia_in, fx$elig), fx$family_max)
  four <- policy_replace(current_law(), pia_bend_base = c(180, 1085, 2000),
                         pia_pct = c(0.9, 0.32, 0.15, 0.05))
  expect_identical(pia(fx$aime[1:200], fx$elig[1:200], four), fx$four_bracket_pia)
})

test_that("COLAs match pyanypia", {
  expect_identical(apply_colas(fx$cola_amount, fx$cola_elig, fx$cola_by, fx$cola_bm),
                   fx$cola_out)
})

test_that("COLAs with no increase due return the amount", {
  expect_identical(apply_colas(1000, 2030, 2030, 11), 1000)
})
