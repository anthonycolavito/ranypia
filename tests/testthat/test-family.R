make_aux <- function(members) {
  lapply(seq_len(nrow(members)), function(j) {
    x <- members[j, ]
    g <- if ("guarantee_pia" %in% names(x)) x$guarantee_pia else NULL
    if (!is.null(g) && is.na(g)) g <- NULL
    auxiliary(x$kind, x$birth_year, x$birth_month, x$claim_age, x$birth_day,
              guarantee_pia = g)
  })
}

expect_family <- function(fam, case) {
  expect_identical(fam$full, as.numeric(case$full))
  expect_identical(fam$after_max, as.numeric(case$after_max))
  expect_identical(fam$reduction_factor, as.numeric(case$reduction_factor))
  expect_identical(fam$benefit, as.numeric(case$benefit))
}

test_that("life families match pyanypia", {
  fx <- jsonlite::fromJSON(test_path("fixtures", "family_life.json"), simplifyVector = FALSE)
  for (case in fx) {
    members <- as_doubles(do.call(rbind, lapply(case$members, as.data.frame)))
    fam <- family_benefits(case$pia, case$mfb, make_aux(members), case$ben_year,
                           case$ben_month)
    expect_family(fam, lapply(case[c("full", "after_max", "reduction_factor", "benefit")],
                              unlist))
  }
})

test_that("survivors, with the widow guarantee, match pyanypia", {
  fx <- jsonlite::fromJSON(test_path("fixtures", "family_survivor.json"),
                           simplifyVector = FALSE)
  guarantees <- 0
  for (case in fx) {
    earn <- stats::setNames(unlist(case$earnings),
                            seq(case$first_year, length.out = length(case$earnings)))
    birth <- unlist(case$birth)
    death <- unlist(case$death)
    benefit <- list(case$ben_year, case$ben_month)
    d <- deceased_worker(earn, birth[1], birth[2], death[1], benefit, birth_day = birth[3])
    expect_identical(d$pia, case$worker$pia)
    expect_identical(d$mfb, case$worker$mfb)
    aux <- list()
    for (x in case$members) {
      g <- NULL
      if (x$kind %in% c("widow", "disabled_widow")) {
        disabled <- x$kind == "disabled_widow"
        g <- widow_guarantee_pia(
          earn, birth[1], birth[2], death[1], death[2], x$birth_year, x$birth_month,
          benefit, widow_birth_day = x$birth_day,
          disabled_onset_year = if (disabled) (x$ent - 12) %/% 12,
          entitlement_year = if (disabled) x$ent %/% 12,
          birth_day = birth[3], death_day = death[3])
        expect_identical(g, x$guarantee_pia)
        guarantees <- guarantees + (g > 0)
      }
      aux[[length(aux) + 1]] <- auxiliary(x$kind, x$birth_year, x$birth_month, x$claim_age,
                                          x$birth_day, guarantee_pia = g)
    }
    fam <- family_benefits(d$pia, d$mfb, aux, case$ben_year, case$ben_month, survivor = TRUE)
    expect_family(fam, lapply(case[c("full", "after_max", "reduction_factor", "benefit")],
                              unlist))
  }
  expect_gt(guarantees, 10)
})

test_that("the family maximum binds and absent members get nothing", {
  fam <- family_benefits(2000, 3500, rep(list(auxiliary("child")), 4), 2030)
  expect_lte(sum(fam$after_max), 1500)
  fam2 <- family_benefits(c(2000, 2000), c(3500, 3500),
                          list(auxiliary("child", present = c(TRUE, FALSE))), 2030)
  expect_identical(fam2$benefit, c(1000, 0))
})

test_that("a survivor kind in a life family is refused", {
  expect_error(family_benefits(1000, 1500, list(auxiliary("widow")), 2030),
               "not a life beneficiary")
})
