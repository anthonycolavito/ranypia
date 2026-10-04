# Projection formulas for derived policy series (scalar loops, run once per
# policy). Ported from pyanypia's _project.py, itself transliterated from
# AnyPIA (avgwg.cpp, wbgenrl.cpp, qcamt.cpp, piaparms.cpp). Operation order
# and rounding are copied exactly; do not simplify the arithmetic.
# Series are full-length vectors indexed by year - 1936.

YEAR79 <- 1979
AUTO_YEAR <- 1978
MAXEARN <- 99999999.0
SPEC_MIN_MAX_YEARS <- 20

yi <- function(y) y - FIRST_YEAR + 1

get0 <- function(series, y) {
  if (y < FIRST_YEAR || y > LAST_YEAR) return(0)
  v <- series[yi(y)]
  if (is.na(v)) 0 else v
}

project_fq <- function(fq, fqinc, first, last) {
  for (y in seq(first, last)) {
    fq[yi(y)] <- round_wage(fq[yi(y - 1)] * (fqinc[yi(y)] / 100 + 1))
  }
  fq
}

# WageBaseGeneral::project. wage_base_ind: 0 = OASDI, 2 = old-law (1977).
project_base <- function(base, fq, cpi, wage_base_ind, first, last) {
  defcomp <- 0.0149249
  f <- function(y) fq[yi(y)]
  y <- first
  while (y <= last) {
    iflag <- 0
    while (get0(cpi, y + iflag - 1) < 0.1) {
      base[yi(y + iflag)] <- base[yi(y - 1)]
      iflag <- iflag + 1
      if (y + iflag > last) return(base)
    }
    i3 <- y + iflag
    baseun <- base[yi(y - 1)]
    if (i3 < 1995) {
      for (i2 in 0:iflag) {
        yr <- y + i2
        if (wage_base_ind != 1 && yr > 1989 && yr < 1993) {
          factor <- if (yr == 1990) {
            f(yr - 2) / f(yr - 3) + 0.02
          } else if (yr == 1991) {
            (f(yr - 2) + 0.02 * f(yr - 3)) / (f(yr - 3) + 0.02 * f(yr - 4))
          } else {
            (f(yr - 2) * (1 + defcomp)) / (f(yr - 3) + 0.02 * f(yr - 4))
          }
        } else {
          factor <- f(yr - 2) / f(yr - 3)
        }
        baseun <- (baseun + 0.001) * factor
      }
    } else {
      factor <- f(i3 - 2) / f(1992)
      baseun <- if (wage_base_ind == 2) {
        45000 * factor
      } else if (wage_base_ind < 2) {
        60600 * factor
      } else {
        MAXEARN
      }
    }
    if (wage_base_ind != 2 && i3 >= YEAR79 && i3 < 1982) {
      base[yi(i3)] <- if (i3 == 1979) 22900 else if (i3 == 1980) 25900 else 29700
    } else {
      base[yi(i3)] <- 300 * floor(baseun / 300 + 0.5)
    }
    if (base[yi(i3)] < base[yi(i3 - 1)]) base[yi(i3)] <- base[yi(i3 - 1)]
    y <- i3 + 1
  }
  base
}

# Qcamt: $50 through 1977, then indexed from `base` in 1978.
project_qc_amounts <- function(fq, last, base = 250) {
  out <- rep(NA_real_, N_YEARS)
  out[yi(FIRST_YEAR:(AUTO_YEAR - 1))] <- 50
  out[yi(AUTO_YEAR)] <- base
  for (y in seq(AUTO_YEAR, last)) {
    factor <- fq[yi(y - 2)] / fq[yi(AUTO_YEAR - 2)]
    k <- trunc((factor * base + 4.99) / 10)
    out[yi(y)] <- k * 10
    if (out[yi(y)] < out[yi(y - 1)]) out[yi(y)] <- out[yi(y - 1)]
  }
  out
}

# PiaParamsLC::projectSpecMin from January 1979: tables by years of
# coverage over 10 (rows) and year (columns), and the August 2001 amounts.
project_special_min <- function(cpiinc, last_year, amount) {
  pia_t <- matrix(NA_real_, SPEC_MIN_MAX_YEARS, N_YEARS)
  mfb_t <- matrix(NA_real_, SPEC_MIN_MAX_YEARS, N_YEARS)
  pia01 <- numeric(SPEC_MIN_MAX_YEARS)
  mfb01 <- numeric(SPEC_MIN_MAX_YEARS)
  cpi <- function(y) cpiinc[yi(y)]
  cola <- function(amt, year) apply_cola_once(amt, cpi(year), year)
  cola_mfb <- function(mfb, year, pia) {
    max(apply_cola_once(mfb, cpi(year), year), round_benefit(1.5 * pia, year))
  }
  for (k in seq_len(SPEC_MIN_MAX_YEARS)) {
    pia <- k * amount
    mfb <- round_benefit(1.5 * pia, YEAR79)
    pia_t[k, yi(1978)] <- pia
    mfb_t[k, yi(1978)] <- mfb
    for (year in seq(YEAR79, min(2000, last_year))) {
      pia <- cola(pia, year)
      pia_t[k, yi(year)] <- pia
      mfb <- cola_mfb(mfb, year, pia)
      mfb_t[k, yi(year)] <- mfb
    }
    # recalculate December 1999 with the extra 0.1 percent
    pia1999 <- apply_cola_once(pia_t[k, yi(1998)], cpi(1999) + 0.1, 1999)
    mfb1999 <- max(apply_cola_once(mfb_t[k, yi(1998)], cpi(1999) + 0.1, 1999),
                   round_benefit(1.5 * pia1999, 1999))
    # December 2000, effective August 2001; the MFB is floored against the
    # uncorrected `pia`, as the C++ does
    pia1999 <- cola(pia1999, 2000)
    pia01[k] <- pia1999
    mfb1999 <- cola_mfb(mfb1999, 2000, pia)
    mfb01[k] <- mfb1999
    pia <- cola(pia1999, 2001)
    pia_t[k, yi(2001)] <- pia
    mfb <- cola_mfb(mfb1999, 2001, pia)
    mfb_t[k, yi(2001)] <- mfb
    for (year in seq(2002, last_year)) {
      pia <- cola(pia, year)
      pia_t[k, yi(year)] <- pia
      mfb <- cola_mfb(mfb, year, pia)
      mfb_t[k, yi(year)] <- mfb
    }
  }
  list(pia = pia_t, mfb = mfb_t, pia_aug2001 = pia01, mfb_aug2001 = mfb01)
}
