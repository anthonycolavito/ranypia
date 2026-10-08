# lifetime_benefits(): every path checked row by row against the functions
# it is built from.

awi <- function(years, scale = 1) current_law()$awi[years - 1936] * scale
long <- function(...) {
  parts <- list(...)
  do.call(rbind, lapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    data.frame(id = p$id, year = p$years, earnings = p$earnings)
  }))
}
rec <- function(id, years, scale) list(id = id, years = years, earnings = awi(years, scale))
yrs <- 1986:2030

test_that("a single retiree: own benefit from the claim month to the month before death", {
  people <- data.frame(id = 7, birth_year = 1964, birth_month = 6, claim_age = 66 * 12,
                       death_age = 80 * 12)
  lb <- lifetime_benefits(long(rec(7, yrs, 1)), people)
  expect_equal(min(lb$age), 66 * 12)
  expect_equal(max(lb$age), 80 * 12 - 1)
  expect_true(all(lb$own_type == "retired") && all(is.na(lb$aux_type)))
  want <- retired_worker(setNames(awi(yrs), yrs), 1964, 6, 66 * 12, benefit_age = lb$age)
  expect_identical(lb$own_benefit, want$benefit)
  expect_identical(lb$total, lb$own_benefit)
})

test_that("one month a year is the same as every month, filtered; chunking changes nothing", {
  people <- data.frame(id = 1:2, birth_year = c(1964, 1966), birth_month = c(6, 2),
                       death_age = c(75, 90) * 12, claim_age = c(62 * 12 + 1, 67 * 12),
                       spouse_id = c(2, 1))
  e <- long(rec(1, yrs, 1.2), rec(2, yrs, 0.2))
  all12 <- lifetime_benefits(e, people)
  june <- lifetime_benefits(e, people, months = 6)
  expect_identical(june, all12[all12$benefit_month == 6, ])
  expect_identical(lifetime_benefits(e, people, months = 6, chunk_size = 7), june)
})

test_that("disability: entitled after the waiting period, converted at NRA", {
  e <- long(rec(3, 1990:2020, 1))
  people <- data.frame(id = 3, birth_year = 1970, birth_month = 3, onset_year = 2021,
                       onset_month = 6, claim_age = 67 * 12, death_age = 85 * 12)
  lb <- lifetime_benefits(e, people)
  expect_equal(c(lb$benefit_year[1], lb$benefit_month[1]), c(2021, 12))
  nra <- normal_retirement_age(1970, 3)
  expect_identical(lb$own_type, ifelse(lb$age < nra, "disabled", "retired"))
  want <- disabled_worker(setNames(awi(1990:2020), 1990:2020), 1970, 3, 2021, 6,
                          benefit = list(lb$benefit_year, lb$benefit_month))
  expect_identical(lb$own_benefit, want$benefit)
})

test_that("a spouse with no earnings: paid from the later of their claim and the worker's", {
  # worker claims at 67 (June 2031); the spouse files at 62+1 (Sept 2030)
  people <- data.frame(id = 1:2, birth_year = c(1964, 1968), birth_month = c(6, 8),
                       death_age = c(95, 95) * 12, claim_age = c(67 * 12, 62 * 12 + 1),
                       spouse_id = c(2, 1))
  lb <- lifetime_benefits(long(rec(1, yrs, 1)), people, months = 6)
  sp <- lb[lb$id == 2, ]
  expect_true(all(is.na(sp$own_type)) && all(sp$own_benefit == 0))
  paid <- sp[!is.na(sp$aux_type), ]
  expect_equal(paid$benefit_year[1], 2031)
  # spousal entitlement in June 2031, at age 62 and 10 months
  w <- lb[lb$id == 1 & lb$benefit_year == 2035, ]
  wr <- retired_worker(setNames(awi(yrs), yrs), 1964, 6, 67 * 12, benefit_age = w$age)
  fb <- family_benefits(wr$pia, wr$mfb, list(auxiliary("spouse", 1968, 8,
                                                       claim_age = 62 * 12 + 10)), 2035, 6)
  expect_identical(paid$aux_benefit[paid$benefit_year == 2035], fb$benefit)
  expect_true(fb$reduction_factor < 1)
})

test_that("widowhood: the widow's limit and dual entitlement, against direct calls", {
  # husband claims at 62+1 and dies at 70 (June 2034); wife has a small own
  # benefit from 64, then files as a widow at her NRA, 67 (August 2035)
  people <- data.frame(id = 1:2, birth_year = c(1964, 1968), birth_month = c(6, 8),
                       death_age = c(70, 90) * 12, claim_age = c(62 * 12 + 1, 64 * 12),
                       spouse_id = c(2, 1), survivor_claim_age = c(NA, 67 * 12))
  e <- long(rec(1, yrs, 1), rec(2, yrs, 0.25))
  lb <- lifetime_benefits(e, people, months = 9)
  wd <- lb[lb$id == 2 & lb$benefit_year == 2040, ]
  expect_identical(wd$aux_type, "widow")
  d <- deceased_worker(setNames(awi(yrs), yrs), 1964, 6, 2034, list(2040, 9))
  wf <- benefit_factor(1964, 6, 62 * 12 + 1)
  own <- retired_worker(setNames(awi(yrs, 0.25), yrs), 1968, 8, 64 * 12, benefit_age = wd$age)
  fb <- family_benefits(d$pia, d$mfb,
                        list(auxiliary("widow", 1968, 8, claim_age = 67 * 12, worker_factor = wf,
                                       own_pia = own$pia, own_factor = own$factor)),
                        2040, 9, survivor = TRUE)
  expect_identical(wd$aux_benefit, fb$benefit)
  expect_identical(wd$own_benefit, own$benefit)
  # the limit binds: no more than 82.5% of the PIA in all
  expect_lte(wd$total, floor(0.825 * d$pia) + 1)
  # nothing is paid on the husband's record once he has died
  expect_true(all(lb$benefit_year[lb$id == 1] <= 2034))
})

test_that("a young survivor family: children to 18, the parent until the youngest is 16", {
  # worker dies at 40 in March 2010; widow 38; children born 2003 and 2006
  people <- data.frame(id = 1:2, birth_year = c(1970, 1972), birth_month = 3,
                       death_age = c(40, 85) * 12, spouse_id = c(2, 1))
  kids <- data.frame(id = c(101, 102), parent_id = 1, birth_year = c(2003, 2006),
                     birth_month = c(5, 9))
  e <- long(rec(1, 1992:2009, 1))
  lb <- lifetime_benefits(e, people, children = kids, months = 6)
  k1 <- lb[lb$id == 101, ]
  k2 <- lb[lb$id == 102, ]
  expect_equal(range(k1$benefit_year), c(2010, 2020))  # 18 in May 2021
  expect_equal(range(k2$benefit_year), c(2010, 2024))  # 18 in Sept 2024
  mom <- lb[lb$id == 2 & !is.na(lb$aux_type), ]
  care <- mom[mom$aux_type == "parent_with_child", ]
  expect_equal(range(care$benefit_year), c(2010, 2022))  # youngest 16 in Sept 2022
  expect_true(all(mom$aux_type[mom$benefit_year >= 2032] == "widow"))  # 60 in 2032
  # one month against direct calls: June 2015, three survivors
  d <- deceased_worker(setNames(awi(1992:2009), 1992:2009), 1970, 3, 2010, list(2015, 6))
  g <- widow_guarantee_pia(setNames(awi(1992:2009), 1992:2009), 1970, 3, 2010, 3, 1972, 3,
                           list(2015, 6))
  fb <- family_benefits(d$pia, d$mfb,
                        list(auxiliary("parent_with_child", 1972, 3),
                             auxiliary("child", 2003, 5), auxiliary("child", 2006, 9)),
                        2015, 6, survivor = TRUE)
  got <- lb[lb$benefit_year == 2015, ]
  expect_identical(got$aux_benefit[match(c(2, 101, 102), got$id)], fb$benefit)
  # the widow benefit at 60 uses the re-indexed guarantee where it applies
  w60 <- mom[mom$benefit_year == 2032, ]
  d32 <- deceased_worker(setNames(awi(1992:2009), 1992:2009), 1970, 3, 2010, list(2032, 6))
  g32 <- widow_guarantee_pia(setNames(awi(1992:2009), 1992:2009), 1970, 3, 2010, 3, 1972, 3,
                             list(2032, 6))
  f32 <- family_benefits(d32$pia, d32$mfb,
                         list(auxiliary("widow", 1972, 3, claim_age = 720, guarantee_pia = g32)),
                         2032, 6, survivor = TRUE)
  expect_identical(w60$aux_benefit, f32$benefit)
})

test_that("a living retiree's family: spouse caring for a child, and the child", {
  people <- data.frame(id = 1:2, birth_year = c(1964, 1985), birth_month = 6,
                       death_age = c(90, 90) * 12, claim_age = c(67 * 12, NA),
                       spouse_id = c(2, 1))
  kids <- data.frame(id = 9, parent_id = 1, birth_year = 2025, birth_month = 1)
  lb <- lifetime_benefits(long(rec(1, yrs, 1)), people, children = kids, months = 6)
  y <- lb[lb$benefit_year == 2033, ]
  expect_identical(y$aux_type[y$id == 2], "spouse_with_child")
  w <- retired_worker(setNames(awi(yrs), yrs), 1964, 6, 67 * 12, benefit_age = y$age[y$id == 1])
  fb <- family_benefits(w$pia, w$mfb, list(auxiliary("spouse_with_child", 1985, 6),
                                           auxiliary("child", 2025, 1)), 2033, 6)
  expect_identical(c(y$aux_benefit[y$id == 2], y$aux_benefit[y$id == 9]), fb$benefit)
  # the child turns 16 in January 2041: the spouse's benefit stops (no claim_age)
  between <- lb$id == 2 & lb$benefit_year >= 2041 & lb$benefit_year < 2054
  expect_true(all(is.na(lb$aux_type[between])))
  # the worker dies at 90 in June 2054: she is then a widow (past 60)
  expect_identical(unique(lb$aux_type[lb$id == 2 & lb$benefit_year > 2054]), "widow")
  expect_equal(max(lb$benefit_year[lb$id == 9]), 2042)  # 18 in January 2043
})

test_that("inputs are checked", {
  e <- long(rec(1, yrs, 1))
  p <- data.frame(id = 1:2, birth_year = 1964, birth_month = 6, death_age = 900,
                  claim_age = 804, spouse_id = c(2, NA))
  expect_error(lifetime_benefits(e, p), "spouses must name each other")
  p2 <- data.frame(id = 5, birth_year = 1964, birth_month = 6, death_age = 900)
  expect_error(lifetime_benefits(e, p2), "id 1 is not in people")
  p3 <- data.frame(id = 1, birth_year = 1964, birth_month = 6, death_age = 900,
                   survivor_claim_age = 700)
  expect_error(lifetime_benefits(e, p3), "survivor_claim_age")
  expect_error(lifetime_benefits(e, p3[, 1:3]), "death_age")
})

test_that("a currently insured worker's children and widow get benefits, the widow not at 60", {
  # four years of work (16 QCs) ending at 40: fully insured needs 18, but he
  # is currently insured, so the children and their mother are paid (sec.
  # 202(d), (g)); a widow's benefit at 60 needs fully insured status
  people <- data.frame(id = 1:2, birth_year = c(1990, 1992), birth_month = 3,
                       death_age = c(40, 85) * 12, spouse_id = c(2, 1))
  kids <- data.frame(id = 101, parent_id = 1, birth_year = 2024, birth_month = 5)
  e <- long(rec(1, 2026:2029, 1))
  expect_false(fully_insured(setNames(awi(2026:2029), 2026:2029), 1990, 3, 2030, 3))
  lb <- lifetime_benefits(e, people, children = kids, months = 6)
  expect_true(all(lb$aux_benefit[lb$id == 101] > 0))
  expect_equal(range(lb$benefit_year[lb$id == 101]), c(2030, 2041))
  mom <- lb[lb$id == 2 & !is.na(lb$aux_type), ]
  expect_identical(unique(mom$aux_type), "parent_with_child")
  expect_false(any(lb$aux_type[lb$id == 2] %in% "widow"))
})

test_that("qc_history reaches the panel runner", {
  e <- data.frame(id = 1, year = 1955:1977, earnings = 300)
  people <- data.frame(id = 1, birth_year = 1930, birth_month = 6, claim_age = 65 * 12,
                       death_age = 80 * 12)
  with_rule <- lifetime_benefits(e, people, months = 6)
  expect_true(all(with_rule$own_type == "retired"))
  rec <- data.frame(id = 1, year = 1955:1977, qcs = 1)
  with_rec <- lifetime_benefits(e, people, months = 6, qc_history = rec)
  expect_equal(nrow(with_rec), 0)  # never insured: no entitlement, no rows
})
