test_that("round_benefit matches pyanypia across eras", {
  fx <- fixture("rounding")
  for (i in seq_along(fx$years)) {
    expect_identical(round_benefit(fx$amounts, fx$years[i]), fx$round_benefit[i, ])
  }
  for (i in seq_along(fx$cola_year)) {
    expect_identical(ranypia:::apply_cola_once(fx$amounts, fx$cola_pct[i], fx$cola_year[i]),
                     fx$apply_cola[i, ])
  }
})

test_that("round_benefit takes per-element years", {
  expect_identical(round_benefit(c(100.04, 100.04), c(1981, 1982)),
                   c(round_benefit(100.04, 1981), round_benefit(100.04, 1982)))
  expect_identical(round_benefit(100.04, 1982), 100.0)
})
