fx <- fixture("wep")

test_that("the windfall percentage matches pyanypia", {
  for (r in seq_along(fx$pct_elig)) {
    e <- fx$pct_elig[r]
    for (k in 1:2) {
      got <- ranypia:::windfall_first_pct(rep(e, 35), rep(e + c(0, 3)[k], 35), 0:34, 0.9)
      expect_identical(got, fx$pct[r, k, ])
    }
  }
})

test_that("retired workers with a pension match pyanypia under the WEP", {
  i <- fx$inputs
  got <- retired_worker(i$earnings, i$birth_year, i$birth_month, i$claim_age,
                        first_year = i$first_year, birth_day = i$birth_day,
                        benefit_age = i$benefit_age, noncovered_pension = i$pension,
                        policy = policy_replace(current_law(), wep_enabled = TRUE))
  expect_outputs(got, fx$outputs)
})

test_that("the WEP is off under current law", {
  i <- fx$inputs
  on <- retired_worker(i$earnings[1:20, ], i$birth_year[1:20], i$birth_month[1:20],
                       i$claim_age[1:20], first_year = i$first_year,
                       birth_day = i$birth_day[1:20], noncovered_pension = 2000)
  off <- retired_worker(i$earnings[1:20, ], i$birth_year[1:20], i$birth_month[1:20],
                        i$claim_age[1:20], first_year = i$first_year,
                        birth_day = i$birth_day[1:20])
  expect_identical(on$pia, off$pia)
})

test_that("GPO matches pyanypia", {
  expect_identical(gpo_offset(fx$gpo_benefit, fx$gpo_pension), fx$gpo)
})
