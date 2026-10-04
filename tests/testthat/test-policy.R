fx <- fixture("policy")

test_that("every derived series matches pyanypia for alternatives 1-3", {
  for (alt in 1:3) {
    pol <- current_law(alt)
    want <- fx[[paste0("alt", alt)]]
    for (name in c("awi", "cola", "taxmax", "base77", "qc_amount", "yoc_specmin",
                   "yoc_wep", "nra", "drc_rate_by_elig")) {
      expect_identical(pol[[name]], want[[name]], label = paste(alt, name))
    }
    expect_identical(pol$spec_min_pia, want$spec_min_pia)
    expect_identical(pol$spec_min_mfb, want$spec_min_mfb)
    expect_identical(pol$spec_min_pia_aug2001, want$spec_min_pia_aug2001)
    expect_identical(pol$spec_min_mfb_aug2001, want$spec_min_mfb_aug2001)
  }
})

test_that("AWI growth flows into the taxable maximum and QC amounts", {
  pol <- current_law()
  higher <- policy_replace(pol, awi_growth = pol$awi_growth + 1)
  expect_identical(higher$awi, fx$awi_plus_1$awi)
  expect_identical(higher$taxmax, fx$awi_plus_1$taxmax)
  expect_identical(higher$qc_amount, fx$awi_plus_1$qc_amount)
})

test_that("pinned values apply to one year only", {
  p <- policy_with_series(current_law(), "taxmax", c("2030" = 250000))
  expect_identical(p$taxmax[2030 - 1936], 250000)
  expect_identical(p$taxmax[2031 - 1936], current_law()$taxmax[2031 - 1936])
})

test_that("bad policies are refused", {
  expect_error(policy_replace(current_law(), pia_pct = c(0.9, 0.32)), "pia_pct")
  expect_error(policy_with_series(current_law(), "bogus", c("2030" = 1)), "unknown series")
  expect_error(policy_replace(current_law(), bogus = 1), "bogus")
  expect_error(current_law(4), "alt")
})

test_that("current law has the WEP and GPO off", {
  pol <- current_law()
  expect_false(pol$wep_enabled)
  expect_false(pol$gpo_enabled)
  expect_output(print(pol), "90% / 32% / 15%")
})
