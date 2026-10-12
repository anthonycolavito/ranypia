# The retirement earnings test (Social Security Act sec. 203(b), (f)),
# measured on annual earnings. The month-by-month charging of the excess
# against a family's benefits, and the adjustment of the reduction factor
# that follows (sec. 202(q)(7)), are in the benefit streams.

# Annual exempt amounts as SSA published them
# (https://www.ssa.gov/oact/cola/rtea.html and rteahistory.html, checked
# 2026-10-12). The lower amount applies under 65 (before 2000) or before
# the year of attaining NRA (from 2000); the higher amount at 65-71
# (1975-1982), 65-69 (1983-1999), and from 2000 in the year of attaining NRA,
# for the months before it.
RET_LOWER_HIST <- c(
  `1975` = 2520, `1976` = 2760, `1977` = 3000, `1978` = 3240, `1979` = 3480,
  `1980` = 3720, `1981` = 4080, `1982` = 4440, `1983` = 4920, `1984` = 5160,
  `1985` = 5400, `1986` = 5760, `1987` = 6000, `1988` = 6120, `1989` = 6480,
  `1990` = 6840, `1991` = 7080, `1992` = 7440, `1993` = 7680, `1994` = 8040,
  `1995` = 8160, `1996` = 8280, `1997` = 8640, `1998` = 9120, `1999` = 9600,
  `2000` = 10080, `2001` = 10680, `2002` = 11280, `2003` = 11520, `2004` = 11640,
  `2005` = 12000, `2006` = 12480, `2007` = 12960, `2008` = 13560, `2009` = 14160,
  `2010` = 14160, `2011` = 14160, `2012` = 14640, `2013` = 15120, `2014` = 15480,
  `2015` = 15720, `2016` = 15720, `2017` = 16920, `2018` = 17040, `2019` = 17640,
  `2020` = 18240, `2021` = 18960, `2022` = 19560, `2023` = 21240, `2024` = 22320,
  `2025` = 23400, `2026` = 24480
)
RET_HIGHER_HIST <- c(
  `1975` = 2520, `1976` = 2760, `1977` = 3000, `1978` = 4000, `1979` = 4500,
  `1980` = 5000, `1981` = 5500, `1982` = 6000, `1983` = 6600, `1984` = 6960,
  `1985` = 7320, `1986` = 7800, `1987` = 8160, `1988` = 8400, `1989` = 8880,
  `1990` = 9360, `1991` = 9720, `1992` = 10200, `1993` = 10560, `1994` = 11160,
  `1995` = 11280, `1996` = 12500, `1997` = 13500, `1998` = 14500, `1999` = 15500,
  `2000` = 17000, `2001` = 25000, `2002` = 30000, `2003` = 30720, `2004` = 31080,
  `2005` = 31800, `2006` = 33240, `2007` = 34440, `2008` = 36120, `2009` = 37680,
  `2010` = 37680, `2011` = 37680, `2012` = 38880, `2013` = 40080, `2014` = 41400,
  `2015` = 41880, `2016` = 41880, `2017` = 44880, `2018` = 45360, `2019` = 46920,
  `2020` = 48600, `2021` = 50520, `2022` = 51960, `2023` = 56520, `2024` = 59520,
  `2025` = 62160, `2026` = 65160
)

# Sec. 203(f)(8): the monthly amount for year t is the base times
# AWI(t - 2) / AWI(base year - 2), rounded to the nearest $10 ($5 rounds
# up), never below the year before, and set only when a benefit increase
# takes effect the December before (so it stays put after a zero COLA).
# Published amounts are kept; later years are projected from them.
project_exempt_amounts <- function(p, awi, cola) {
  one <- function(hist, base, base_year) {
    out <- to_series(hist)
    last <- max(as.numeric(names(hist)))
    for (y in seq(last + 1, LAST_YEAR)) {
      prev <- out[yi(y - 1)]
      if (is.na(cola[yi(y - 1)]) || cola[yi(y - 1)] <= 0) {
        out[yi(y)] <- prev
        next
      }
      monthly <- base * awi[yi(y - 2)] / awi[yi(base_year - 2)]
      out[yi(y)] <- max(prev, 12 * floor(monthly / 10 + 0.5) * 10)
    }
    out
  }
  list(lower = one(p$ret_lower_hist, p$ret_lower_base, 1994),
       higher = one(p$ret_higher_hist, p$ret_higher_base, 2002))
}

#' Retirement earnings test
#'
#' Excess earnings charged against benefits for a year (sec. 203(f)),
#' measured on annual earnings. Before normal retirement age (NRA), each
#' $2 of earnings above the lower exempt amount withholds $1. In the year of
#' attaining NRA, only earnings before the NRA month count, at $1 for each
#' $3 above the higher amount; from the NRA month on there is no test.
#' The rules from 2000 on follow the Senior Citizens' Freedom to Work Act;
#' for earlier years the test ran to age 72 (to 1982) or 70 (1983-1999),
#' with the higher amount from age 65 (the NRA for those cohorts) at $1 for
#' $2 before 1990 and $1 for $3 from 1990.
#'
#' Earnings are taken as spread evenly over the year: in a year the test
#' stops partway through (NRA, or 70 or 72 before 2000) the earnings
#' counted are the year's earnings times the months tested over 12. The
#' first year of entitlement is where the monthly test (sec. 203(f)(1)(E))
#' applies; measured annually, `grace_year = "prorate"` (default) counts
#' the year's earnings times the months entitled and tested over 12, which
#' sits between the law's two limits: `"none"` (stopped work at
#' entitlement, nothing charged) and `"annual"` (kept working, the whole
#' year's earnings count).
#'
#' `earnings` are all earnings from work in the year (wages and net
#' self-employment earnings, not capped at the taxable maximum). Pass the
#' total including any noncovered work if you have it.
#'
#' @param earnings Earnings for the year.
#' @param year The year (1978 or later).
#' @param birth_year,birth_month,birth_day Date of birth.
#' @param entitled_from Age, in months, at the first month of entitlement
#'   (claim age for a retired worker, entitlement age for an auxiliary).
#'   Months before it are not tested.
#' @param nra Normal retirement age in months for the beneficiary (default:
#'   the retired-worker NRA for the birth date). Pass the auxiliary's own
#'   NRA for a spouse or widow(er).
#' @param grace_year How the first year of entitlement is measured:
#'   `"prorate"`, `"none"` or `"annual"`.
#' @param policy A [current_law()] policy.
#' @return A tibble, one row per input: `tested_months` (months of the year
#'   subject to the test while entitled), `tested_earnings`, `exempt`
#'   (annual exempt amount), `rate` (share of earnings above it withheld),
#'   `excess` (whole dollars charged against benefits) and `first_year`
#'   (whether this is the first year of entitlement).
#' @export
#' @examples
#' # Born June 1964, claims at 62 and 1 month, earns $40,000 in 2027
#' earnings_test(40000, 2027, 1964, 6, entitled_from = 62 * 12 + 1)
#'
#' # the same earnings in the year of attaining NRA (67 in June 2031)
#' earnings_test(40000, 2031, 1964, 6, entitled_from = 62 * 12 + 1)
earnings_test <- function(earnings, year, birth_year, birth_month, birth_day = 15,
                          entitled_from = 0, nra = NULL, grace_year = c("prorate", "none", "annual"),
                          policy = current_law()) {
  grace_year <- match.arg(grace_year)
  e <- as_num("earnings", earnings)
  y <- as_whole("year", year)
  kb <- adjusted_birth(birth_year, birth_month, birth_day)
  ent <- as_whole("entitled_from", entitled_from)
  nra_v <- if (is.null(nra)) normal_retirement_age(birth_year, birth_month, birth_day, policy) else
    as_whole("nra", nra)
  n <- common_length(e, y, kb$year, kb$month, ent, nra_v)
  e <- recycle_to(e, "earnings", n)
  y <- recycle_to(y, "year", n)
  ky <- recycle_to(kb$year, "birth_year", n)
  km <- recycle_to(kb$month, "birth_month", n)
  ent <- recycle_to(ent, "entitled_from", n)
  nra_v <- recycle_to(nra_v, "nra", n)
  check("year", y < 1978, "before 1978 (the annual test starts in 1978)")
  check("earnings", e < 0, "negative")
  ret_test_year(e, y, month_index(ky, km), ent, nra_v, grace_year, policy)
}

# The test for each person-year, with the birth month index `k` (adjusted
# birth month) given. Ages are months from `k`.
ret_test_year <- function(e, y, k, ent, nra, grace_year, policy) {
  n <- length(e)
  if (n == 0) {
    return(tibble::tibble(tested_months = numeric(), tested_earnings = numeric(),
                          exempt = numeric(), rate = numeric(), excess = numeric(),
                          first_year = logical()))
  }
  jan <- 12 * y                              # month index of January of the year
  start_age <- jan - k                       # age in January
  # the age from which the test stops
  stop_age <- ifelse(y >= 2000, nra, ifelse(y <= 1982, 72 * 12, 70 * 12))
  # months of the year before the test stops (whether or not entitled)
  before_stop <- pmin(pmax(stop_age - start_age, 0), 12)
  # months tested while entitled
  tested <- pmax(pmin(start_age + 12, stop_age) - pmax(start_age, ent), 0)
  first <- ent >= start_age & ent < start_age + 12
  # the higher amount: from 2000, the year of attaining NRA; before 2000,
  # any year the NRA (65) is attained by its end
  higher <- start_age + 12 > nra
  exempt <- ifelse(higher, policy$ret_higher[yi(y)], policy$ret_lower[yi(y)])
  rate <- ifelse(higher & y >= 1990, policy$ret_rate_higher, policy$ret_rate_lower)
  # earnings counted: the months before the test stops, and in the first
  # year of entitlement the months entitled
  share <- if (grace_year == "annual") before_stop / 12 else
    ifelse(first, tested / 12, before_stop / 12)
  counted <- e * share
  excess <- floor(rate * pmax(counted - exempt, 0))
  if (grace_year == "none") excess[first] <- 0
  excess[tested == 0 | !isTRUE(policy$ret_enabled)] <- 0
  tibble::tibble(tested_months = tested, tested_earnings = counted, exempt = exempt,
                 rate = rate, excess = excess, first_year = first)
}

# Earnings in year `y` for each row of an earnings matrix (0 outside it).
earnings_in_year <- function(m, first, y) {
  col <- y - first + 1
  ok <- col >= 1 & col <= ncol(m)
  out <- rep(0, nrow(m))
  out[ok] <- m[cbind(which(ok), col[ok])]
  out[is.na(out)] <- 0
  out
}

# The age (months) from which the test stops in each month index `t`.
ret_stop_age <- function(t, nra) {
  y <- t %/% 12
  ifelse(y >= 2000, nra, ifelse(y <= 1982, 72 * 12, 70 * 12))
}

# Charges each year's excess earnings against a retired worker's benefits,
# year by year from the claim (sec. 203(b), (f)(1)): whole months' benefits
# from the first month tested until the excess is used up, the last month
# only in part. `benefit_fn(rows, t, wm, wc)` gives the scheduled benefit for
# workers `rows` in month indexes `t`, with `wm` months withheld before NRA
# and `wc` credited months withheld after it. Returns the months withheld
# before NRA (`wm`, one per worker), the credit events from months withheld
# after NRA (`credit_w`, `credit_year`: credited from January of the year
# after), and every withheld amount (`w`, `t`, `amount`).
charge_retired <- function(m, first, k, claim, last, nra, grace_year, policy, benefit_fn) {
  n <- length(k)
  wm <- rep(0, n)
  out_w <- out_t <- out_a <- numeric()
  cr_w <- cr_y <- numeric()
  ent <- k + claim
  # months tested: entitled, before the test stops, and paid
  y0 <- min(ent %/% 12)
  y1 <- max((k + pmax(nra, 72 * 12)) %/% 12)
  for (y in seq(y0, y1)) {
    t0 <- 12 * y
    cand <- which(ent <= t0 + 11 & k + last >= t0 & t0 - k < ret_stop_age(t0, nra))
    if (!length(cand)) next
    res <- ret_test_year(earnings_in_year(m[cand, , drop = FALSE], first, y), rep(y, length(cand)),
                         k[cand], claim[cand], nra[cand], grace_year, policy)
    hit <- cand[res$excess > 0]
    if (!length(hit)) next
    remaining <- res$excess[res$excess > 0]
    # the year's months for those workers, in calendar order
    rw <- rep(hit, each = 12)
    rt <- t0 + rep(0:11, times = length(hit))
    age <- rt - k[rw]
    tested <- age >= claim[rw] & age < ret_stop_age(rt, nra[rw]) & age <= last[rw]
    rw <- rw[tested]
    rt <- rt[tested]
    if (!length(rw)) next
    # credits from withheld months after NRA in earlier years
    wc <- credits_for(rw, rt, k, cr_w, cr_y)
    ben <- benefit_fn(rw, rt, wm[rw], wc)
    rem <- remaining[match(rw, hit)]
    # whole months until the excess is used up: running total within worker
    before <- stats::ave(ben, rw, FUN = function(x) cumsum(x) - x)
    amount <- pmin(ben, pmax(rem - before, 0))
    took <- amount > 0
    out_w <- c(out_w, rw[took])
    out_t <- c(out_t, rt[took])
    out_a <- c(out_a, amount[took])
    pre <- took & (rt - k[rw]) < nra[rw]
    if (any(pre)) {
      add <- tapply(rep(1, sum(pre)), rw[pre], sum)
      wm[as.integer(names(add))] <- wm[as.integer(names(add))] + add
    }
    post <- took & !pre
    if (any(post)) {
      cr_w <- c(cr_w, rw[post])
      cr_y <- c(cr_y, rep(y, sum(post)))
    }
  }
  list(wm = wm, credit_w = cr_w, credit_year = cr_y, w = out_w, t = out_t, amount = out_a)
}

# Credited months for rows (w, t): months withheld after NRA in years before
# t's year, or every one of them once the worker is 70.
credits_for <- function(w, t, k, cr_w, cr_y, age70 = TRUE) {
  if (!length(cr_w)) return(rep(0, length(w)))
  out <- rep(0, length(w))
  y <- t %/% 12
  at70 <- age70 & (t - k[w]) >= 70 * 12
  for (yr in unique(cr_y)) {
    cnt <- tabulate(cr_w[cr_y == yr], nbins = max(c(w, cr_w)))
    use <- yr < y | at70
    out[use] <- out[use] + cnt[w[use]]
  }
  out
}
