# Benefits for spouses, children and survivors under the family maximum
# (ported from pyanypia's family.py). Not covered: divorced spouses, who are
# paid outside the maximum.

LIFE_RATES <- c(spouse = 0.5, spouse_with_child = 0.5, child = 0.5)
SURVIVOR_RATES <- c(child = 0.75, parent_with_child = 0.75, widow = 1.0, disabled_widow = 1.0)
REDUCIBLE <- c("spouse", "widow", "disabled_widow")
DISABLED_WIDOW_FACTOR <- 0.715
WIDOW_MAX_REDUCTION <- 0.285

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
#' @return A `ranypia_auxiliary`.
#' @export
#' @examples
#' auxiliary("spouse", 1966, 3, claim_age = 64 * 12)
auxiliary <- function(kind, birth_year = 1960, birth_month = 1, claim_age = 0,
                      birth_day = 15, present = TRUE, guarantee_pia = NULL) {
  kinds <- union(names(LIFE_RATES), names(SURVIVOR_RATES))
  if (!kind %in% kinds) {
    abort_field("kind", sprintf("must be one of %s", paste(kinds, collapse = ", ")))
  }
  structure(list(kind = kind, birth_year = birth_year, birth_month = birth_month,
                 claim_age = claim_age, birth_day = birth_day, present = present,
                 guarantee_pia = guarantee_pia), class = "ranypia_auxiliary")
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
#' @param worker_pia,worker_mfb The worker's PIA and family maximum, one per
#'   family.
#' @param auxiliaries A list of [auxiliary()] members.
#' @param benefit_year,benefit_month The benefit month.
#' @param survivor Whether the worker has died.
#' @param policy A [current_law()] policy.
#' @return A long tibble, one row per family and member: `family`, `member`,
#'   `kind`, `full`, `after_max`, `reduction_factor`, `benefit` (whole
#'   dollars).
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
  cy <- cola_year(recycle_to(by, "benefit_year", n), recycle_to(bm, "benefit_month", n))
  fulls <- list()
  factors <- list()
  for (aux in auxiliaries) {
    if (!aux$kind %in% names(rates)) {
      abort_field("kind", sprintf("'%s' is not a %s beneficiary; expected one of %s", aux$kind,
                                  if (survivor) "survivor" else "life",
                                  paste(sort(names(rates)), collapse = ", ")))
    }
    present <- recycle_to(as.logical(aux$present), "present", n)
    base <- p
    if (!is.null(aux$guarantee_pia)) {
      g <- recycle_to(as_num("guarantee_pia", aux$guarantee_pia), "guarantee_pia", n)
      base <- ifelse(g > p, g, p)
    }
    fulls[[length(fulls) + 1]] <- ifelse(present, round_benefit(base * rates[[aux$kind]], cy), 0)
    factors[[length(factors) + 1]] <- aux_reduction(aux, n, policy)
  }
  total <- numeric(n)
  for (f in fulls) total <- total + f
  available <- if (survivor) mfb else mfb - p
  ratio <- ifelse(total > 0, available / total, 1)
  ratio <- pmin(pmax(ratio, 0), 1)
  out <- vector("list", length(auxiliaries))
  for (k in seq_along(auxiliaries)) {
    kind <- auxiliaries[[k]]$kind
    after <- round_benefit(ratio * fulls[[k]], cy)
    reduced <- if (kind %in% REDUCIBLE) round_benefit(factors[[k]] * after, cy) else after
    out[[k]] <- tibble::tibble(family = seq_len(n), member = k, kind = kind,
                               full = fulls[[k]], after_max = after,
                               reduction_factor = factors[[k]],
                               benefit = floor_dollar(reduced))
  }
  res <- do.call(rbind, out)
  if (is.null(res)) {
    res <- tibble::tibble(family = integer(), member = integer(), kind = character(),
                          full = numeric(), after_max = numeric(),
                          reduction_factor = numeric(), benefit = numeric())
  }
  res[order(res$family, res$member), , drop = FALSE]
}
