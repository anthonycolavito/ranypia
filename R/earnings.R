# Earnings-side computations: capping, wage indexing, computation years,
# AIME, quarters and years of coverage, insured status (ported from
# pyanypia's earnings.py).

year_cols <- function(first, width) seq(first, length.out = width)

rows_of <- function(x, name, n) recycle_to(as_whole(name, x), name, n)

# Multiplies each column j of m by v[j] (or divides).
col_div <- function(m, v) m / rep(v, each = nrow(m))

capped <- function(m, first, policy) {
  cap <- at(policy$taxmax, year_cols(first, ncol(m)), "taxmax")
  pmin(m, rep(cap, each = nrow(m)))
}

# Capped earnings indexed to elig - 2, nominal from elig - 2 on, zero before 1951.
indexed <- function(m, first, elig, policy) {
  years <- year_cols(first, ncol(m))
  cm <- capped(m, first, policy)
  awi_idx <- at(policy$awi, elig - 2, "awi")
  awi_y <- at(policy$awi, years, "awi")
  ind <- floor(col_div(awi_idx * cm, awi_y) * 100 + 0.5) / 100
  yr <- rep(years, each = nrow(m))
  x <- ifelse(yr < elig - 2, ind, cm)
  x <- ifelse(yr >= 1951, x, 0)
  dim(x) <- dim(m)
  x
}

# Zeroes earnings after each row's last year.
window <- function(m, first, last) {
  if (is.null(last)) return(m)
  yr <- rep(year_cols(first, ncol(m)), each = nrow(m))
  x <- ifelse(yr <= last, m, 0)
  dim(x) <- dim(m)
  x
}

# Mask of each row's n highest values; equal values go to the later year, as
# sorting (value, year) pairs does in PiaMethod::orderEarnings.
select_top <- function(x, n) {
  rows <- nrow(x)
  width <- ncol(x)
  row_id <- rep(seq_len(rows), times = width)
  col_id <- rep(seq_len(width), each = rows)
  o <- order(row_id, as.vector(x), col_id)
  rank <- integer(rows * width)
  rank[o] <- rep(seq_len(width), times = rows)
  mask <- rank > (width - rep(n, times = width))
  dim(mask) <- dim(x)
  mask
}

# Row sums of the masked values, added in year order like the C++ loop.
sum_by_year <- function(x, mask) {
  total <- numeric(nrow(x))
  for (j in seq_len(ncol(x))) total <- total + ifelse(mask[, j], x[, j], 0)
  total
}

#' Capped and indexed earnings
#'
#' `capped_earnings()` limits each year's earnings to the taxable maximum.
#' `indexed_earnings()` also indexes them to the average wage two years
#' before eligibility; later years enter at face value, years before 1951 as
#' zero.
#'
#' @param earnings A matrix (rows = workers), a named vector, or a long data
#'   frame; see [ranypia-package].
#' @param elig_year Eligibility year(s).
#' @param first_year First year of a matrix without year column names.
#' @param policy A [current_law()] policy.
#' @return A matrix with one row per worker.
#' @export
capped_earnings <- function(earnings, first_year = NULL, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  capped(e$m, e$first, policy)
}

#' @rdname capped_earnings
#' @export
indexed_earnings <- function(earnings, elig_year, first_year = NULL, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  indexed(e$m, e$first, rows_of(elig_year, "elig_year", nrow(e$m)), policy)
}

#' Average indexed monthly earnings
#'
#' The average over the `comp_years` highest years of capped, wage-indexed
#' earnings, truncated to the dollar. Years after `last_year` are ignored, as
#' a benefit computed for a given month ignores later earnings.
#'
#' @inheritParams capped_earnings
#' @param comp_years Number of computation years (see [computation_years()]).
#' @param last_year Last year of earnings to count (default: all).
#' @return AIME per worker.
#' @export
#' @examples
#' aime(c("2000" = 50000, "2001" = 52000), 2030, 35)
aime <- function(earnings, elig_year, comp_years, first_year = NULL, last_year = NULL,
                 policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  n <- nrow(e$m)
  elig <- rows_of(elig_year, "elig_year", n)
  k <- rows_of(comp_years, "comp_years", n)
  check("comp_years", k < 1, "must be at least 1")
  last <- if (is.null(last_year)) NULL else rows_of(last_year, "last_year", n)
  x <- indexed(window(e$m, e$first, last), e$first, elig, policy)
  floor(sum_by_year(x, select_top(x, k)) / (k * 12))
}

#' Elapsed and computation years
#'
#' `elapsed_years()`: years after the year of attaining 21 (or after 1950)
#' and before eligibility, or before death; at least 2.
#' `computation_years()`: elapsed years less dropout years (5, or one per
#' five elapsed years up to 5 for a disabled worker); at least 2.
#'
#' @inheritParams eligibility_year
#' @param elig_year Eligibility year.
#' @param disabled Whether the worker is disabled.
#' @return Years.
#' @export
#' @examples
#' computation_years(1960, 6, 2022)
elapsed_years <- function(birth_year, birth_month, elig_year, birth_day = 15,
                          death_year = NULL) {
  ky <- adjusted_birth(birth_year, birth_month, birth_day)$year
  e2 <- as_whole("elig_year", elig_year) - 1
  if (!is.null(death_year)) e2 <- pmin(e2, as_whole("death_year", death_year) - 1)
  pmax(e2 - pmax(ky + 21, 1950), 2)
}

#' @rdname elapsed_years
#' @export
computation_years <- function(birth_year, birth_month, elig_year, birth_day = 15,
                              disabled = FALSE, death_year = NULL) {
  elapsed <- elapsed_years(birth_year, birth_month, elig_year, birth_day, death_year)
  drop <- ifelse(disabled, pmin(elapsed %/% 5, 5), 5)
  pmax(elapsed - drop, 2)
}

qcs <- function(m, first, policy) {
  amt <- at(policy$qc_amount, year_cols(first, ncol(m)), "qc_amount")
  q <- pmin(4, floor(col_div(m, amt)))
  dim(q) <- dim(m)
  q
}

#' Quarters of coverage
#'
#' Annual quarters of coverage, `min(4, earnings %/% QC amount)`, on
#' uncapped earnings. Before 1978 SSA counted calendar-quarter wages; this
#' annual rule with the $50 amount is an approximation there.
#'
#' @inheritParams capped_earnings
#' @return A matrix with one row per worker.
#' @export
quarters_of_coverage <- function(earnings, first_year = NULL, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  qcs(e$m, e$first, policy)
}

pre1951_total <- function(m, first) {
  years <- year_cols(first, ncol(m))
  pre <- years < 1951
  if (!any(pre)) return(numeric(nrow(m)))
  pmin(rowSums(m[, pre, drop = FALSE]), 42000)
}

# QCs earned from quarter q1 through q2 (quarter index 4 * year + 0..3). A
# year's QCs count in any of its quarters, up to the quarters of it in range
# (QcArray::accumulate).
accumulate <- function(qc, first, q1, q2) {
  n <- nrow(qc)
  width <- ncol(qc)
  cum <- matrix(0, n, width + 1)
  for (j in seq_len(width)) cum[, j + 1] <- cum[, j] + qc[, j]
  rows <- seq_len(n)
  year_qcs <- function(y) {
    j <- y - first + 1
    inside <- j >= 1 & j <= width
    ifelse(inside, qc[cbind(rows, pmin(pmax(j, 1), width))], 0)
  }
  years_between <- function(ya, yb) {
    ja <- pmin(pmax(ya - first, 0), width)
    jb <- pmin(pmax(yb - first + 1, 0), width)
    ifelse(jb > ja, cum[cbind(rows, jb + 1)] - cum[cbind(rows, ja + 1)], 0)
  }
  y1 <- q1 %/% 4
  k1 <- q1 %% 4
  y2 <- q2 %/% 4
  k2 <- q2 %% 4
  spanning <- pmin(year_qcs(y1), 4 - k1) + years_between(y1 + 1, y2 - 1) +
    pmin(year_qcs(y2), k2 + 1)
  within <- pmin(k2 - k1 + 1, year_qcs(y1))
  ifelse(q1 > q2, 0, ifelse(y1 == y2, within, spanning))
}

NO_FREEZE <- 10000

# PiaCal::fins1Cal's fully insured test as of quarter `through_q`.
fully_insured_at <- function(m, qc, first, ky, through_q, freeze_from) {
  lump <- pmin(trunc(pre1951_total(m, first) / 400), 56)
  total <- lump + accumulate(qc, first, rep(4 * 1951, length(through_q)), through_q)
  e2 <- pmin(through_q %/% 4, ky + 61)
  e1 <- pmax(ky + 21, 1950)
  frozen <- ifelse(freeze_from <= e2, e2 - pmax(freeze_from, e1 + 1) + 1, 0)
  total >= pmin(40, pmax(6, e2 - e1 - frozen))
}

#' Insured status
#'
#' `fully_insured()`: a quarter of coverage for each year elapsed after 21
#' and before 62 (at least 6, at most 40), counting QCs through the quarter
#' of (`through_year`, `through_month`).
#' `disability_insured()`: fully insured at entitlement, and 20 quarters of
#' coverage in the 40 ending with the quarter of onset (fewer for workers
#' disabled before 31). Entitlement defaults to the end of the five-month
#' waiting period.
#'
#' @inheritParams capped_earnings
#' @inheritParams eligibility_year
#' @param through_year,through_month The month at which to test.
#' @param onset_year,onset_month,onset_day Date of disability onset.
#' @param entitlement Optional `list(year, month)` of entitlement.
#' @return Logical per worker.
#' @export
fully_insured <- function(earnings, birth_year, birth_month, through_year,
                          through_month = 12, first_year = NULL, birth_day = 15,
                          policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  n <- nrow(e$m)
  ky <- recycle_to(adjusted_birth(birth_year, birth_month, birth_day)$year, "birth_year", n)
  q <- 4 * rows_of(through_year, "through_year", n) +
    (rows_of(through_month, "through_month", n) - 1) %/% 3
  fully_insured_at(e$m, qcs(e$m, e$first, policy), e$first, ky, q, rep(NO_FREEZE, n))
}

age21_quarter <- function(ky, km) 4 * (ky + 21) + (km - 1) %/% 3 + 1

# Whether the window ending d2 holds QCs in at least half its quarters, and
# its start. 40 quarters; with `special`, a worker whose window reaches back
# before 21 uses the quarters since 21, at least 12.
twenty_of_forty <- function(qc, first, d2, age21, special) {
  d1 <- d2 - 39
  if (special) d1 <- ifelse(d1 < age21, ifelse(d2 - age21 < 11, d2 - 11, age21), d1)
  list(ok = accumulate(qc, first, d1, d2) >= (d2 - d1 + 1) %/% 2, start = d1)
}

disability_insured_at <- function(m, qc, first, ky, km, window_from, window_to,
                                  entitlement_q, freeze_from) {
  age21 <- age21_quarter(ky, km)
  ok <- logical(nrow(m))
  trials <- window_to - window_from
  top <- max(c(trials, 0))
  for (i in seq(top, 0)) {
    ok <- ok | (i <= trials & twenty_of_forty(qc, first, window_from + i, age21, FALSE)$ok)
  }
  young <- !ok & (window_from - 39 < age21)
  for (i in seq(0, top)) {
    ok <- ok | (young & i <= trials &
                  twenty_of_forty(qc, first, window_from + i, age21, TRUE)$ok)
  }
  ok & fully_insured_at(m, qc, first, ky, entitlement_q, freeze_from)
}

#' @rdname fully_insured
#' @export
disability_insured <- function(earnings, birth_year, birth_month, onset_year, onset_month,
                               first_year = NULL, birth_day = 15, onset_day = 15,
                               entitlement = NULL, policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  n <- nrow(e$m)
  oy <- rows_of(onset_year, "onset_year", n)
  om <- rows_of(onset_month, "onset_month", n)
  od <- rows_of(onset_day, "onset_day", n)
  waiting <- month_index(oy, om) + ifelse(od == 1, 0, 1)
  if (is.null(entitlement)) {
    ent <- waiting + 5
  } else {
    ent <- month_index(rows_of(entitlement[[1]], "entitlement_year", n),
                       rows_of(entitlement[[2]], "entitlement_month", n))
    waiting <- ent - 5
  }
  kb <- adjusted_birth(birth_year, birth_month, birth_day)
  ky <- recycle_to(kb$year, "birth_year", n)
  km <- recycle_to(kb$month, "birth_month", n)
  disability_insured_at(e$m, qcs(e$m, e$first, policy), e$first, ky, km,
                        month_index(oy, om) %/% 3, waiting %/% 3, ent %/% 3, oy)
}

#' Years of coverage
#'
#' Years of coverage for the special minimum or the WEP's 30 years, through
#' `last_year`, on uncapped earnings. Earnings before 1951 count one year per
#' $900, up to 14.
#'
#' @inheritParams aime
#' @param kind `"special_minimum"` or `"wep"`.
#' @return Years per worker.
#' @export
years_of_coverage <- function(earnings, last_year, first_year = NULL,
                              kind = c("special_minimum", "wep"), policy = current_law()) {
  kind <- match.arg(kind)
  e <- as_earnings(earnings, first_year)
  n <- nrow(e$m)
  last <- rows_of(last_year, "last_year", n)
  years <- year_cols(e$first, ncol(e$m))
  series <- if (kind == "special_minimum") policy$yoc_specmin else policy$yoc_wep
  pre_years <- pmin(14, floor(pre1951_total(e$m, e$first) / 900))
  amt <- rep(Inf, length(years))
  post <- years >= 1951
  amt[post] <- at(series, years[post], "years_of_coverage")
  yr <- rep(years, each = n)
  hit <- e$m > rep(amt, each = n) - 0.009 & yr <= last
  dim(hit) <- dim(e$m)
  pre_years + rowSums(hit)
}
