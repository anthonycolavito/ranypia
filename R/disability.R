# Disability-specific pieces: the 1980-amendments family maximum and the
# child-care dropout years (ported from pyanypia's disability.py).

MAX_DROPOUT_YEARS <- 3 # ordinary and child-care dropout years together

#' Disability family maximum
#'
#' At eligibility: 85% of the AIME, but not less than the PIA nor more than
#' 150% of it. Carry it forward with [apply_colas()]; it is never less than
#' the PIA payable.
#'
#' @param pia_elig PIA at eligibility.
#' @param aime AIME.
#' @param elig_year Eligibility year.
#' @param policy A [current_law()] policy.
#' @return Family maximums.
#' @export
di_family_max <- function(pia_elig, aime, elig_year, policy = current_law()) {
  p <- as_num("pia_elig", pia_elig)
  a <- as_num("aime", aime)
  e <- as_whole("elig_year", elig_year)
  n <- common_length(p, a, e)
  p <- recycle_to(p, "pia_elig", n)
  a <- recycle_to(a, "aime", n)
  e <- recycle_to(e, "elig_year", n)
  mfb85 <- round_benefit(0.85 * a, e - 1)
  mfb150 <- round_benefit(1.5 * p, e - 1)
  ifelse(mfb85 > mfb150, mfb150, ifelse(mfb85 < p, p, mfb85))
}

#' AIME with child-care dropout years
#'
#' Up to three dropout years in all, the extra ones being years with a
#' child under 3 in care and no earnings (ChildCareCalc). Earnings after
#' `last_year` are ignored (for a disabled worker, the years after onset);
#' dropout candidates run through `through_year` (the year before the
#' benefit month; default `last_year`).
#'
#' @inheritParams aime
#' @param ordinary_dropout Ordinary dropout years already allowed.
#' @param childcare A logical matrix shaped like the earnings.
#' @param through_year Last year a dropout year may fall in.
#' @return AIME per worker.
#' @export
childcare_aime <- function(earnings, elig_year, comp_years, ordinary_dropout, childcare,
                           first_year = NULL, last_year = NULL, through_year = NULL,
                           policy = current_law()) {
  e <- as_earnings(earnings, first_year)
  m <- e$m
  first <- e$first
  rows <- nrow(m)
  width <- ncol(m)
  elig <- rows_of(elig_year, "elig_year", rows)
  k <- rows_of(comp_years, "comp_years", rows)
  drop0 <- rows_of(ordinary_dropout, "ordinary_dropout", rows)
  cc <- matrix(as.logical(childcare), nrow = rows, ncol = width)
  years <- year_cols(first, width)
  last <- if (is.null(last_year)) NULL else rows_of(last_year, "last_year", rows)
  through <- if (!is.null(through_year)) {
    rows_of(through_year, "through_year", rows)
  } else if (is.null(last)) {
    rep(years[width], rows)
  } else {
    last
  }
  yr <- matrix(rep(years, each = rows), rows, width)
  in_range <- yr >= max(first, 1951) & yr <= through
  x <- indexed(window(m, first, last), first, elig, policy)
  # years outside the computation range can be neither chosen nor dropped
  sel <- (select_top(ifelse(in_range, x, -1), k) & in_range) * 1
  empty <- capped(m, first, policy) <= 0.01 & in_range
  drop_max <- ifelse(drop0 < MAX_DROPOUT_YEARS, pmin(MAX_DROPOUT_YEARS - drop0, k - 2), 0)
  drops <- numeric(rows)
  for (i in which(drop_max > 0)) {
    row <- sel[i, ] # 1 chosen, 0 not, -1 dropped
    for (j in which(row == 1 & cc[i, ] & empty[i, ])) {
      row[j] <- -1
      drops[i] <- drops[i] + 1
      if (drops[i] >= drop_max[i]) break
    }
    if (drops[i] < drop_max[i]) {
      # a chosen empty year without a child in care can be swapped for an
      # unchosen empty year with one, freeing another dropout
      out1 <- which(row == 1 & !cc[i, ] & empty[i, ])
      in2 <- which(row == 0 & cc[i, ] & empty[i, ])
      for (s in seq_len(min(length(out1), length(in2), drop_max[i] - drops[i]))) {
        row[out1[s]] <- 0
        row[in2[s]] <- -1
        drops[i] <- drops[i] + 1
      }
    }
    sel[i, ] <- row
  }
  total <- numeric(rows)
  for (j in seq_len(width)) total <- total + ifelse(sel[, j] == 1, x[, j], 0)
  floor(total / ((k - drops) * 12))
}
