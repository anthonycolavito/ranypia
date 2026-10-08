# Lifetime benefits on a panel: every person's benefits, by type, in each
# month from first entitlement until death. Built only from the one-call
# functions and family_benefits(); no formula is computed here.

#' Lifetime benefits for a panel
#'
#' Each person's monthly Social Security benefits from first entitlement
#' until death: their own retired- or disabled-worker benefit, and a
#' benefit on their spouse's record as an aged spouse, a spouse caring for
#' a child under 16, a widow(er), or a widowed parent caring for a child
#' under 16; and each child's benefit on a parent's record. Family members
#' on one record share the family maximum, and the widow(er)'s limit,
#' survivors' delayed credits and dual entitlement apply (see
#' [family_benefits()]). Amounts are in nominal dollars; mortality comes in
#' as `death_age`, so discounting and survival weighting are left to the
#' caller.
#'
#' Claiming is a given input. `claim_age` is when a person files for their
#' own retirement benefit and, as deemed filing requires, any spouse
#' benefit, which starts once the spouse is entitled too.
#' `survivor_claim_age` is when a widow(er) files for survivor benefits
#' (default: as soon as possible, the later of age 60 and the month of
#' death). A disabled person is entitled after the five-month waiting
#' period and converts to a retirement benefit at normal retirement age;
#' disability is used only if it starts before `claim_age`. People with a
#' child under 16 in care are assumed to file as soon as they can.
#'
#' A disabled worker's survivors get the PIA computed with the period of
#' disability excluded (see [deceased_worker()]).
#'
#' Survivors need the worker entitled or fully insured at death for a
#' widow(er)'s benefit, and fully or currently insured ([currently_insured()])
#' for child's and mother's or father's benefits.
#'
#' Not covered: the earnings test, divorce, remarriage, the marriage-length
#' requirements, disabled widow(er)s, disability recovery, a child entitled
#' on more than one record, and a worker who becomes insured only after the
#' month they claim.
#'
#' @param earnings Earnings for the people in `people`: a long data frame
#'   with `id`, `year`, `earnings` (people with no rows have no earnings), or
#'   a matrix with one row per row of `people`.
#' @param people A data frame, one row per person, with `id`, `birth_year`,
#'   `birth_month` and `death_age` (age in the month of death, in months;
#'   no benefit is paid for that month or later), and optionally
#'   `birth_day` (15), `claim_age` (`NA`: never files for retirement),
#'   `onset_year`, `onset_month`, `onset_day` (disability onset; `NA`:
#'   none), `spouse_id` (`NA`: unmarried) and `survivor_claim_age` (months).
#'   Spouses must name each other.
#' @param children Optional data frame of children, one row each: `id`,
#'   `parent_id` (the record they are paid on, an `id` in `people`),
#'   `birth_year`, `birth_month`, and optionally `birth_day` (15) and
#'   `end_age` (216, the month they attain 18; 228 for a student to 19;
#'   `Inf` for a child disabled before 22). A child is paid while the
#'   parent is entitled, or after the parent's death.
#' @param months Calendar months to compute in each year: `1:12` (default)
#'   for every month, or a single month such as `6` for one row a year.
#' @inheritParams retired_worker
#' @param chunk_size Rows per call to the one-call functions; it does not
#'   change results.
#' @return A tibble with one row per person (and child) and month from
#'   their first possible entitlement until death: `id`, `role`
#'   (`"person"` or `"child"`), `benefit_year`, `benefit_month`, `age`
#'   (months), `own_type` (`"retired"`, `"disabled"` or `NA`),
#'   `own_benefit`, `aux_type` (`"spouse"`, `"spouse_with_child"`,
#'   `"widow"`, `"parent_with_child"`, `"child"` or `NA`), `aux_record` (the
#'   `id` whose record pays it), `aux_benefit` (the amount payable on that
#'   record; `aux_type` is `NA` when nothing is, as for a spouse whose own
#'   benefit is larger) and `total`, all in whole dollars.
#' @export
#' @examples
#' awi <- function(years, scale = 1) current_law()$awi[years - 1936] * scale
#' years <- 1986:2030
#' panel <- data.frame(id = rep(1:2, each = length(years)), year = rep(years, 2),
#'                     earnings = c(awi(years), awi(years, 0.3)))
#' people <- data.frame(id = 1:2, birth_year = c(1964, 1966), birth_month = 6,
#'                      death_age = c(78, 92) * 12, claim_age = c(67, 62) * 12 + c(0, 1),
#'                      spouse_id = c(2, 1))
#' lb <- lifetime_benefits(panel, people, months = 6)
#' lb[lb$id == 2 & lb$benefit_year %in% c(2029, 2031, 2043), ]
lifetime_benefits <- function(earnings, people, children = NULL, months = 1:12,
                              first_year = NULL, policy = current_law(),
                              chunk_size = 250000) {
  if (!is.data.frame(people)) abort_field("people", "must be a data frame")
  need <- setdiff(c("id", "birth_year", "birth_month", "death_age"), names(people))
  if (length(need)) abort_field("people", paste("needs columns", paste(need, collapse = ", ")))
  if (anyDuplicated(people$id)) abort_field("people", "ids must be unique")
  check_stream_args(1, chunk_size)
  months <- as_whole("months", months)
  check("months", months < 1 | months > 12, "must be 1 to 12")
  np <- nrow(people)
  col <- function(name, default) {
    if (name %in% names(people)) people[[name]] else rep(default, np)
  }

  # earnings, one row per person
  e <- as_earnings(earnings, first_year)
  if (is.null(e$ids)) {
    if (nrow(e$m) != np) {
      abort_field("earnings", "a matrix needs one row per row of people")
    }
    M <- e$m
  } else {
    stray <- setdiff(e$ids, people$id)
    if (length(stray)) abort_field("earnings", sprintf("id %s is not in people", stray[1]))
    M <- matrix(0, np, ncol(e$m), dimnames = list(NULL, colnames(e$m)))
    r <- match(people$id, e$ids)
    M[!is.na(r), ] <- e$m[r[!is.na(r)], ]
  }
  first <- e$first

  # ---- per person ----
  by <- as_whole("birth_year", people$birth_year)
  bm <- as_whole("birth_month", people$birth_month)
  bd <- as_whole("birth_day", col("birth_day", 15))
  death_age <- as_whole("death_age", people$death_age)
  claim <- as_num_na("claim_age", col("claim_age", NA))
  surv_claim <- as_num_na("survivor_claim_age", col("survivor_claim_age", NA))
  oy <- as_num_na("onset_year", col("onset_year", NA))
  om <- as_num_na("onset_month", col("onset_month", NA))
  od <- as_num_na("onset_day", col("onset_day", 15))
  od[is.na(od)] <- 15
  kb <- adjusted_birth(by, bm, bd)
  k <- month_index(kb$year, kb$month)
  death <- k + death_age
  nra <- normal_retirement_age(by, bm, bd, policy)
  check("claim_age", !is.na(claim) & claim < earliest_claim_age(bd),
        "before the earliest retirement age")
  check("survivor_claim_age", !is.na(surv_claim) & surv_claim < 720, "before 60")
  check("onset_month", !is.na(oy) & is.na(om), "missing where onset_year is given")
  sp <- match(col("spouse_id", NA), people$id)
  check("spouse_id", !is.na(col("spouse_id", NA)) & is.na(sp), "not an id in people")
  check("spouse_id", !is.na(sp) & sp == seq_len(np), "a person cannot be their own spouse")
  check("spouse_id", !is.na(sp) & (is.na(sp[ifelse(is.na(sp), 1, sp)]) |
                                     sp[ifelse(is.na(sp), 1, sp)] != seq_len(np)),
        "spouses must name each other")

  # disability: entitlement after the waiting period, before NRA, death and
  # any retirement claim; then disability insured status
  di_ent <- month_index(ifelse(is.na(oy), 2000, oy), ifelse(is.na(om), 1, om)) +
    ifelse(od == 1, 0, 1) + 5
  di <- !is.na(oy) & di_ent - k < nra & di_ent < death & (is.na(claim) | di_ent <= k + claim)
  if (any(di)) {
    w <- which(di)
    ent <- from_month_index(di_ent[w])
    d0 <- disabled_worker(M[w, , drop = FALSE], by[w], bm[w], oy[w], om[w], first_year = first,
                          birth_day = bd[w], onset_day = od[w],
                          entitlement = list(ent$year, ent$month), policy = policy)
    di[w] <- d0$insured
  }
  # retirement: insured at the claim, and the claiming factor
  ret <- !di & !is.na(claim)
  own_factor <- rep(NA_real_, np)
  own_factor[di] <- 1
  if (any(ret)) {
    w <- which(ret)
    r0 <- retired_worker(M[w, , drop = FALSE], by[w], bm[w], claim[w], first_year = first,
                         birth_day = bd[w], policy = policy)
    ret[w] <- r0$insured & k[w] + claim[w] < death[w]
    own_factor[w] <- r0$factor
  }
  own_start <- ifelse(di, di_ent, ifelse(ret, k + claim, Inf))
  own_start[own_start >= death] <- Inf
  entitled <- is.finite(own_start)

  # survivors: widow(er)s need the worker entitled, or fully insured at
  # death; children and a parent caring for a child need fully or currently
  # insured (sec. 202(d), (g))
  dmi <- from_month_index(death)
  surv_ok <- entitled | fully_insured(M, by, bm, dmi$year, dmi$month, first_year = first,
                                      birth_day = bd, policy = policy)
  surv_child <- surv_ok | currently_insured(M, dmi$year, dmi$month, first_year = first,
                                            policy = policy)
  # the deceased worker's factor for a widow(er): the claiming factor with
  # every credit earned before death; a worker past NRA who never filed gets
  # credits up to the month of death
  worker_factor <- rep(NA_real_, np)
  never <- !entitled & death_age >= nra
  fc <- ifelse(ret & entitled, claim, ifelse(never, death_age, NA))
  wf_rows <- which(!is.na(fc))
  if (length(wf_rows)) {
    worker_factor[wf_rows] <- benefit_factor(by[wf_rows], bm[wf_rows], fc[wf_rows],
                                             bd[wf_rows], fc[wf_rows] + 12, policy)
  }
  worker_factor[di & entitled] <- 1

  # ---- children ----
  nc <- 0
  if (!is.null(children)) {
    if (!is.data.frame(children)) abort_field("children", "must be a data frame")
    need <- setdiff(c("id", "parent_id", "birth_year", "birth_month"), names(children))
    if (length(need)) {
      abort_field("children", paste("needs columns", paste(need, collapse = ", ")))
    }
    nc <- nrow(children)
  }
  if (nc > 0) {
    ccol <- function(name, default) {
      if (name %in% names(children)) children[[name]] else rep(default, nc)
    }
    cp <- match(children$parent_id, people$id)
    check("parent_id", is.na(cp), "not an id in people")
    ckb <- adjusted_birth(as_whole("birth_year", children$birth_year),
                          as_whole("birth_month", children$birth_month),
                          as_whole("birth_day", ccol("birth_day", 15)))
    ck <- month_index(ckb$year, ckb$month)
    c_end <- ck + as_num("end_age", ccol("end_age", 216))
    rec_from <- ifelse(entitled, own_start, ifelse(surv_child, death, Inf))
    c_start <- pmax(rec_from[cp], ck)
  } else {
    cp <- ck <- c_end <- c_start <- numeric()
  }
  # months a record has a child under 16 in care
  care_keys <- numeric()
  if (nc > 0) {
    care_end <- pmin(c_end, ck + 192)
    cnt <- pmax(ifelse(is.finite(care_end - c_start), care_end - c_start, 0), 0)
    cw <- rep(seq_len(nc), times = cnt)
    ct <- c_start[cw] + sequence(cnt, from = 0)
    care_keys <- unique(cp[cw] * 1e5 + ct)
  }
  first_care <- rep(Inf, np)
  if (nc > 0) {
    fc_care <- tapply(c_start[c_start < pmin(c_end, ck + 192)],
                      cp[c_start < pmin(c_end, ck + 192)], min)
    first_care[as.integer(names(fc_care))] <- fc_care
  }

  # ---- each person's months ----
  spousal_start <- rep(Inf, np)
  widow_start <- rep(Inf, np)
  has_sp <- !is.na(sp)
  j <- sp[has_sp]
  i <- which(has_sp)
  spousal_start[i] <- ifelse(!is.na(claim[i]) & entitled[j],
                             pmax(k[i] + ifelse(is.na(claim[i]), 0, claim[i]), own_start[j]),
                             Inf)
  spousal_start[i][spousal_start[i] >= death[j]] <- Inf
  widow_start[i] <- ifelse(surv_ok[j],
                           pmax(death[j], k[i] + ifelse(is.na(surv_claim[i]), 720, surv_claim[i])),
                           Inf)
  care_start <- rep(Inf, np)
  care_start[i] <- first_care[j]
  start <- pmin(own_start, spousal_start, widow_start, care_start)
  grid <- function(start, end) {
    cnt <- pmax(ifelse(is.finite(start), end - start, 0), 0)
    w <- rep(seq_along(start), times = cnt)
    t <- start[w] + sequence(cnt, from = 0)
    keep <- (t %% 12 + 1) %in% months
    list(w = w[keep], t = t[keep])
  }
  pg <- grid(start, death)
  pw <- pg$w
  pt <- pg$t
  n <- length(pw)
  own_type <- rep(NA_character_, n)
  own_benefit <- rep(0, n)
  own_pia <- rep(NA_real_, n)
  own_mfb <- rep(NA_real_, n)

  # ---- own benefits ----
  run_rows <- function(rows, fun) {
    out <- lapply(chunks(length(rows), chunk_size), function(r) fun(rows[r]))
    do.call(rbind, out)
  }
  own_rows <- which(pt >= own_start[pw])
  rr <- own_rows[ret[pw[own_rows]]]
  if (length(rr)) {
    res <- run_rows(rr, function(x) {
      w <- pw[x]
      retired_worker(M[w, , drop = FALSE], by[w], bm[w], claim[w], first_year = first,
                     birth_day = bd[w], benefit_age = pt[x] - k[w], policy = policy)
    })
    own_benefit[rr] <- res$benefit
    own_pia[rr] <- res$pia
    own_mfb[rr] <- res$mfb
    own_type[rr] <- "retired"
  }
  dr <- own_rows[di[pw[own_rows]]]
  if (length(dr)) {
    res <- run_rows(dr, function(x) {
      w <- pw[x]
      ent <- from_month_index(di_ent[w])
      ben <- from_month_index(pt[x])
      disabled_worker(M[w, , drop = FALSE], by[w], bm[w], oy[w], om[w], first_year = first,
                      birth_day = bd[w], onset_day = od[w],
                      entitlement = list(ent$year, ent$month),
                      benefit = list(ben$year, ben$month), policy = policy)
    })
    own_benefit[dr] <- res$benefit
    own_pia[dr] <- res$pia
    own_mfb[dr] <- res$mfb
    own_type[dr] <- ifelse(pt[dr] - k[pw[dr]] < nra[pw[dr]], "disabled", "retired")
  }
  own_key <- pw * 1e5 + pt

  # ---- each person's auxiliary benefit on their spouse's record ----
  aux_type <- rep(NA_character_, n)
  aux_claim <- rep(0, n)
  sr <- which(has_sp[pw])
  sj <- sp[pw[sr]]
  si <- pw[sr]
  st <- pt[sr]
  alive <- st < death[sj]
  care <- (sj * 1e5 + st) %in% care_keys
  live_ent <- alive & st >= own_start[sj]
  aux_type[sr] <- ifelse(live_ent & care, "spouse_with_child",
                  ifelse(live_ent & st >= spousal_start[si], "spouse",
                  ifelse(!alive & st >= widow_start[si], "widow",
                  ifelse(!alive & surv_child[sj] & care, "parent_with_child", NA))))
  aux_claim[sr] <- ifelse(aux_type[sr] %in% "spouse", spousal_start[si] - k[si],
                   ifelse(aux_type[sr] %in% "widow", widow_start[si] - k[si], 0))

  # ---- record values: the worker's PIA and maximum in each month ----
  fam_rows <- which(!is.na(aux_type))
  cg <- grid(c_start, c_end)
  child_n <- length(cg$w)
  rec_j <- c(sp[pw[fam_rows]], cp[cg$w])
  rec_t <- c(pt[fam_rows], cg$t)
  rkey <- unique(rec_j * 1e5 + rec_t)
  rj <- rkey %/% 1e5
  rt <- rkey - rj * 1e5
  rpia <- rmfb <- rep(NA_real_, length(rkey))
  rsurv <- rt >= death[rj]
  live <- which(!rsurv)
  if (length(live)) {
    hit <- match(rkey[live], own_key)
    rpia[live] <- own_pia[hit]
    rmfb[live] <- own_mfb[hit]
  }
  dead <- which(rsurv)
  if (length(dead)) {
    res <- run_rows(dead, function(x) {
      w <- rj[x]
      ben <- from_month_index(rt[x])
      # a disabled worker's survivors: the computation with the freeze
      dw <- di[w] & entitled[w]
      ent <- from_month_index(di_ent[w])
      deceased_worker(M[w, , drop = FALSE], by[w], bm[w], dmi$year[w],
                      list(ben$year, ben$month), first_year = first, birth_day = bd[w],
                      onset_year = ifelse(dw, oy[w], NA), onset_month = ifelse(dw, om[w], NA),
                      onset_day = od[w], entitlement = list(ent$year, ent$month),
                      policy = policy)
    })
    rpia[dead] <- res$pia
    rmfb[dead] <- res$mfb
  }

  # ---- families: one per record and month ----
  # members: the spouse slot (from fam_rows) and the children (from cg)
  f_spouse <- match(rkey, sp[pw[fam_rows]] * 1e5 + pt[fam_rows])
  f_spouse_row <- fam_rows[f_spouse]
  ckey <- cp[cg$w] * 1e5 + cg$t
  cfam <- match(ckey, rkey)
  slot <- stats::ave(cfam, cfam, FUN = seq_along)
  nslot <- if (child_n) max(slot) else 0
  aux_benefit <- rep(0, n)
  child_benefit <- rep(0, child_n)
  skind <- ifelse(is.na(f_spouse_row), "none", aux_type[f_spouse_row])
  groups <- split(seq_along(rkey), paste(rsurv, skind))
  for (g in groups) {
    f <- g
    kind <- skind[f[1]]
    ben <- from_month_index(rt[f])
    auxes <- list()
    if (kind != "none") {
      x <- f_spouse_row[f]
      wi <- pw[x]
      jj <- rj[f]
      dual <- !is.na(own_type[x])
      gp <- NULL
      if (kind == "widow") {
        gp <- rep(0, length(f))
        early_death <- death_age[jj] < 62 * 12 + 1
        if (any(early_death)) {
          e1 <- which(early_death)
          gp[e1] <- widow_guarantee_pia(
            M[jj[e1], , drop = FALSE], by[jj[e1]], bm[jj[e1]], dmi$year[jj[e1]],
            dmi$month[jj[e1]], by[wi[e1]], bm[wi[e1]], list(ben$year[e1], ben$month[e1]),
            widow_birth_day = bd[wi[e1]], first_year = first, birth_day = bd[jj[e1]],
            death_day = 15, policy = policy)
        }
      }
      auxes[[1]] <- auxiliary(
        kind, by[wi], bm[wi], claim_age = aux_claim[x], birth_day = bd[wi],
        guarantee_pia = gp,
        worker_factor = if (kind == "widow") worker_factor[jj],
        own_pia = ifelse(dual, own_pia[x], NA),
        own_factor = ifelse(dual, own_factor[wi], 1))
    }
    if (nslot > 0) {
      for (s in seq_len(nslot)) {
        cr <- match(f * 1e3 + s, cfam * 1e3 + slot)  # child row in this slot
        cw <- cg$w[ifelse(is.na(cr), 1, cr)]
        if (all(is.na(cr))) next
        auxes[[length(auxes) + 1]] <- auxiliary(
          "child", ifelse(is.na(cr), 2000, ckb$year[cw]),
          ifelse(is.na(cr), 1, ckb$month[cw]), present = !is.na(cr))
        attr(auxes[[length(auxes)]], "rows") <- cr
      }
    }
    if (!length(auxes)) next
    fb <- family_benefits(rpia[f], rmfb[f], auxes, ben$year, ben$month,
                          survivor = rsurv[f[1]], policy = policy)
    for (m in seq_along(auxes)) {
      amt <- fb$benefit[fb$member == m]
      if (m == 1 && kind != "none") {
        aux_benefit[f_spouse_row[f]] <- amt
      } else {
        cr <- attr(auxes[[m]], "rows")
        ok <- !is.na(cr)
        child_benefit[cr[ok]] <- amt[ok]
      }
    }
  }

  # nothing payable on the spouse's record (an own benefit above it): no
  # auxiliary benefit to report
  aux_type[!is.na(aux_type) & aux_benefit == 0] <- NA
  person <- tibble::tibble(
    id = people$id[pw], role = "person", benefit_year = pt %/% 12,
    benefit_month = pt %% 12 + 1, age = pt - k[pw], own_type = own_type,
    own_benefit = own_benefit, aux_type = aux_type,
    aux_record = ifelse(is.na(aux_type), NA, people$id[sp[pw]]),
    aux_benefit = ifelse(is.na(aux_type), 0, aux_benefit))
  out <- person
  if (child_n) {
    kid <- tibble::tibble(
      id = children$id[cg$w], role = "child", benefit_year = cg$t %/% 12,
      benefit_month = cg$t %% 12 + 1, age = cg$t - ck[cg$w],
      own_type = NA_character_, own_benefit = 0, aux_type = "child",
      aux_record = people$id[cp[cg$w]], aux_benefit = child_benefit)
    out <- rbind(person, kid)
  }
  out$total <- out$own_benefit + out$aux_benefit
  out
}
