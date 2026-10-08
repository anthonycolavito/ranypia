# Benefits for spouses, children and survivors under the family maximum
# (ported from pyanypia's family.py). Not covered: divorced spouses, who are
# paid outside the maximum.

LIFE_RATES <- c(spouse = 0.5, spouse_with_child = 0.5, child = 0.5)
SURVIVOR_RATES <- c(child = 0.75, parent_with_child = 0.75, widow = 1.0, disabled_widow = 1.0)
REDUCIBLE <- c("spouse", "widow", "disabled_widow")
DISABLED_WIDOW_FACTOR <- 0.715
WIDOW_MAX_REDUCTION <- 0.285
WIDOW_KINDS <- c("widow", "disabled_widow")
# The widow(er)'s limit: 82 1/2 percent of the PIA (POMS RS 00615.320)
WIDOW_LIMIT_PCT <- 0.825

#' A spouse, child or survivor
#'
#' Describes one family member; every argument but `kind` may be a vector
#' over families.
#'
#' @param kind `"spouse"` (aged spouse), `"spouse_with_child"` (caring for
#'   the worker's child, never reduced), `"child"`, and for survivors
#'   `"widow"`, `"disabled_widow"` and `"parent_with_child"`.
#' @param birth_year,birth_month,birth_day The member's date of birth.
#' @param claim_age Months from the member's adjusted birth month to
#'   entitlement; matters only for spouses and widow(er)s.
#' @param present `FALSE` leaves a family without this member.
#' @param guarantee_pia A widow(er)'s re-indexed guarantee PIA in the
#'   benefit month ([widow_guarantee_pia()]); the higher of it and the
#'   worker's PIA is the base of their benefit.
#' @param worker_factor For a widow(er): the deceased worker's claiming
#'   factor (`factor` from [retired_worker()]), or `NA` if the worker was never
#'   entitled. Below 1 (the worker claimed early), the widow(er)'s limit
#'   applies: their benefit is at most the larger of the worker's reduced
#'   benefit and 82.5% of the PIA (POMS RS 00615.320). Above 1 (delayed
#'   credits), the worker's benefit with credits becomes the base of the
#'   widow(er)'s benefit when it is higher than the PIA (RS 00615.706). For a
#'   worker who died past NRA without claiming, use the factor for a claim in
#'   the month of death, which counts credits up to but not including it.
#' @param own_pia,own_factor Dual entitlement: the member's own PIA in the
#'   benefit month, on their own record, and their own claiming factor
#'   (default 1; `factor` from [retired_worker()]). `NA` or 0 means no own
#'   benefit. Only the excess over the own benefit is paid on this record
#'   (POMS RS 00615.020): a spouse gets the full spouse benefit less the own
#'   PIA, then reduced for age; a widow(er) gets the reduced widow(er)'s
#'   benefit less the reduced own benefit; anyone else (a spouse with a child
#'   in care, a child, a parent) gets their benefit less their own benefit.
#' @param end_age Age, in months, from which the member is no longer
#'   entitled (`NULL`, the default, never ends). A child's benefit ends with
#'   the month before they attain 18 (`end_age = 216`), or 19 for a
#'   full-time elementary or secondary student, and continues for a child
#'   disabled before 22 (20 CFR 404.352); a spouse or parent with a child in
#'   care stops when the youngest child attains 16.
#' @return A `ranypia_auxiliary`.
#' @export
#' @examples
#' auxiliary("spouse", 1966, 3, claim_age = 64 * 12)
#' # a widow whose husband claimed early, with her own record
#' auxiliary("widow", 1966, 3, claim_age = 67 * 12, worker_factor = 0.7,
#'           own_pia = 900, own_factor = 0.75)
#' auxiliary("child", 2015, 5, end_age = 18 * 12)
auxiliary <- function(kind, birth_year = 1960, birth_month = 1, claim_age = 0,
                      birth_day = 15, present = TRUE, guarantee_pia = NULL,
                      worker_factor = NULL, own_pia = NULL, own_factor = 1,
                      end_age = NULL) {
  kinds <- union(names(LIFE_RATES), names(SURVIVOR_RATES))
  if (!kind %in% kinds) {
    abort_field("kind", sprintf("must be one of %s", paste(kinds, collapse = ", ")))
  }
  if (!is.null(worker_factor) && !kind %in% WIDOW_KINDS) {
    abort_field("worker_factor", "applies only to a widow(er)")
  }
  structure(list(kind = kind, birth_year = birth_year, birth_month = birth_month,
                 claim_age = claim_age, birth_day = birth_day, present = present,
                 guarantee_pia = guarantee_pia, worker_factor = worker_factor,
                 own_pia = own_pia, own_factor = own_factor, end_age = end_age),
            class = "ranypia_auxiliary")
}

# Numeric input where NA means "does not apply".
as_num_na <- function(name, x) {
  if (!is.numeric(x) && !is.logical(x)) abort_field(name, "must be numeric")
  x <- as.numeric(x)
  check(name, !is.na(x) & !is.finite(x), "must be finite or NA")
  x
}

aux_reduction <- function(aux, n, policy) {
  claim <- recycle_to(as_whole("claim_age", aux$claim_age), "claim_age", n)
  present <- recycle_to(as.logical(aux$present), "present", n)
  if (!aux$kind %in% REDUCIBLE) return(rep(1, n))
  if (aux$kind == "disabled_widow") {
    check("claim_age", present & (claim < 600 | claim >= 720),
          "a disabled widow(er) claims at 50-59")
    return(rep(DISABLED_WIDOW_FACTOR, n))
  }
  ky <- recycle_to(adjusted_birth(aux$birth_year, aux$birth_month, aux$birth_day)$year,
                   "birth_year", n)
  if (aux$kind == "spouse") {
    earliest <- recycle_to(earliest_claim_age(aux$birth_day), "birth_day", n)
    check("claim_age", present & claim < earliest,
          "a spouse claims before the earliest retirement age")
    nra <- at(policy$nra, ky + 62, "nra")
    return(spouse_reduction_factor(pmax(nra - claim, 0), policy))
  }
  check("claim_age", present & claim < 720, "a widow(er) claims before 60")
  # a widow(er)'s NRA is the worker's schedule shifted two years
  nra <- at(policy$nra, ky + 60, "nra")
  months <- pmax(nra - claim, 0)
  1 - (months / (nra - 720)) * WIDOW_MAX_REDUCTION
}

#' Family benefits
#'
#' Benefits for a worker's family in a benefit month, from the worker's PIA
#' and family maximum in that month ([retired_worker()],
#' [disabled_worker()], or with `survivor = TRUE`, [deceased_worker()]).
#' Each member's full benefit is a share of the PIA; if together they exceed
#' what the family maximum leaves (all of it for survivors, the excess over
#' the worker's PIA otherwise) all are cut in proportion; then spouses and
#' widow(er)s are reduced for age.
#'
#' Three rules beyond AnyPIA follow the statute and SSA's POMS (see
#' [auxiliary()]): the widow(er)'s limit and the deceased worker's delayed
#' credits (`worker_factor`); dual entitlement (`own_pia`), where `benefit`
#' is only what is payable on this record; and members who stop at an
#' `end_age`. For benefit months from October 1999, a dually entitled
#' member counts against the family maximum only for what is payable to
#' them, and the others share the rest up to their full benefits (POMS RS
#' 00615.768). Without those arguments, results are exactly AnyPIA's.
#'
#' @param worker_pia,worker_mfb The worker's PIA and family maximum, one per
#'   family.
#' @param auxiliaries A list of [auxiliary()] members.
#' @param benefit_year,benefit_month The benefit month.
#' @param survivor Whether the worker has died.
#' @param policy A [current_law()] policy.
#' @return A long tibble, one row per family and member: `family`, `member`,
#'   `kind`, `full`, `after_max`, `reduction_factor`, `benefit` (whole
#'   dollars payable on this record; for a dually entitled member, the excess
#'   over their own benefit).
#' @export
#' @examples
#' family_benefits(2000, 3500, list(auxiliary("child"), auxiliary("child")),
#'                 benefit_year = 2030)
family_benefits <- function(worker_pia, worker_mfb, auxiliaries, benefit_year,
                            benefit_month = 12, survivor = FALSE, policy = current_law()) {
  if (inherits(auxiliaries, "ranypia_auxiliary")) auxiliaries <- list(auxiliaries)
  rates <- if (survivor) SURVIVOR_RATES else LIFE_RATES
  p <- as_num("worker_pia", worker_pia)
  mfb <- as_num("worker_mfb", worker_mfb)
  by <- as_whole("benefit_year", benefit_year)
  bm <- as_whole("benefit_month", benefit_month)
  n <- common_length(p, mfb, by, bm)
  p <- recycle_to(p, "worker_pia", n)
  mfb <- recycle_to(mfb, "worker_mfb", n)
  by <- recycle_to(by, "benefit_year", n)
  bm <- recycle_to(bm, "benefit_month", n)
  cy <- cola_year(by, bm)
  ben_idx <- month_index(by, bm)
  fulls <- list()
  factors <- list()
  limits <- list()
  duals <- list()
  owns <- list()
  own_reduced <- list()
  for (aux in auxiliaries) {
    if (!aux$kind %in% names(rates)) {
      abort_field("kind", sprintf("'%s' is not a %s beneficiary; expected one of %s", aux$kind,
                                  if (survivor) "survivor" else "life",
                                  paste(sort(names(rates)), collapse = ", ")))
    }
    present <- recycle_to(as.logical(aux$present), "present", n)
    if (!is.null(aux$end_age)) {
      kb <- adjusted_birth(aux$birth_year, aux$birth_month, aux$birth_day)
      age <- ben_idx - recycle_to(month_index(kb$year, kb$month), "birth_year", n)
      present <- present & age < recycle_to(as_num("end_age", aux$end_age), "end_age", n)
    }
    base <- p
    if (!is.null(aux$guarantee_pia)) {
      g <- recycle_to(as_num("guarantee_pia", aux$guarantee_pia), "guarantee_pia", n)
      base <- ifelse(g > p, g, p)
    }
    limit <- rep(Inf, n)
    if (!is.null(aux$worker_factor)) {
      wf <- recycle_to(as_num_na("worker_factor", aux$worker_factor), "worker_factor", n)
      check("worker_factor", !is.na(wf) & wf <= 0, "must be positive")
      # delayed credits: the worker's benefit becomes the base if higher
      deemed <- ifelse(!is.na(wf) & wf > 1, round_benefit(ifelse(is.na(wf), 0, wf) * p, cy), 0)
      base <- ifelse(deemed > base, deemed, base)
      # the widow(er)'s limit, for a worker who claimed early
      early <- !is.na(wf) & wf < 1
      limit <- ifelse(early, pmax(round_benefit(WIDOW_LIMIT_PCT * p, cy),
                                  round_benefit(ifelse(early, wf, 0) * p, cy)), Inf)
    }
    dual <- rep(FALSE, n)
    own <- rep(0, n)
    if (!is.null(aux$own_pia)) {
      op <- recycle_to(as_num_na("own_pia", aux$own_pia), "own_pia", n)
      of <- recycle_to(as_num("own_factor", aux$own_factor), "own_factor", n)
      check("own_pia", !is.na(op) & op < 0, "must not be negative")
      dual <- present & !is.na(op) & op > 0
      own <- ifelse(dual, op, 0)
      own_reduced[[length(fulls) + 1]] <- round_benefit(of * own, cy)
    } else {
      own_reduced[[length(fulls) + 1]] <- rep(0, n)
    }
    fulls[[length(fulls) + 1]] <- ifelse(present, round_benefit(base * rates[[aux$kind]], cy), 0)
    factors[[length(factors) + 1]] <- aux_reduction(aux, n, policy)
    limits[[length(limits) + 1]] <- limit
    duals[[length(duals) + 1]] <- dual
    owns[[length(owns) + 1]] <- own
  }
  total <- numeric(n)
  for (f in fulls) total <- total + f
  available <- if (survivor) mfb else mfb - p
  ratio <- ifelse(total > 0, available / total, 1)
  ratio <- pmin(pmax(ratio, 0), 1)
  afters <- lapply(fulls, function(f) round_benefit(ratio * f, cy))

  # A dually entitled member counts against the maximum only for what is
  # payable to them before any age reduction; the other members share the
  # rest, never above their full benefit (POMS RS 00615.768, benefits for
  # October 1999 on). It does not apply when every member is dually entitled.
  k_all <- seq_along(auxiliaries)
  any_dual <- Reduce(`|`, duals, rep(FALSE, n))
  if (any(any_dual)) {
    dual_payable <- numeric(n)
    other_full <- numeric(n)
    for (k in k_all) {
      dual_payable <- dual_payable + ifelse(duals[[k]], pmax(afters[[k]] - owns[[k]], 0), 0)
      other_full <- other_full + ifelse(duals[[k]], 0, fulls[[k]])
    }
    redo <- any_dual & other_full > 0 & ben_idx >= month_index(1999, 10)
    ratio2 <- ifelse(other_full > 0, (available - dual_payable) / other_full, 1)
    ratio2 <- pmin(pmax(ratio2, 0), 1)
    for (k in k_all) {
      afters[[k]] <- ifelse(redo & !duals[[k]], round_benefit(ratio2 * fulls[[k]], cy),
                            afters[[k]])
    }
  }

  out <- vector("list", length(auxiliaries))
  for (k in k_all) {
    kind <- auxiliaries[[k]]$kind
    after <- afters[[k]]
    reducible <- kind %in% REDUCIBLE
    reduce <- function(x) if (reducible) round_benefit(factors[[k]] * x, cy) else x
    single <- pmin(reduce(after), limits[[k]])
    payable <- if (kind == "spouse") {
      # RS 00615.020 method C: the excess of the full spouse benefit over the
      # own PIA, reduced for age
      ifelse(duals[[k]], reduce(pmax(after - owns[[k]], 0)), single)
    } else if (kind %in% WIDOW_KINDS) {
      # method B: each benefit reduced on its own, then the excess
      ifelse(duals[[k]], pmax(single - own_reduced[[k]], 0), single)
    } else {
      # never reduced for age: the benefit less the (reduced) own benefit
      ifelse(duals[[k]], pmax(after - own_reduced[[k]], 0), single)
    }
    out[[k]] <- tibble::tibble(family = seq_len(n), member = k, kind = kind,
                               full = fulls[[k]], after_max = after,
                               reduction_factor = factors[[k]],
                               benefit = floor_dollar(payable))
  }
  res <- do.call(rbind, out)
  if (is.null(res)) {
    res <- tibble::tibble(family = integer(), member = integer(), kind = character(),
                          full = numeric(), after_max = numeric(),
                          reduction_factor = numeric(), benefit = numeric())
  }
  res[order(res$family, res$member), , drop = FALSE]
}
