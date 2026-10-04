# Policy parameters: the primitive inputs a reform can change, and the
# series derived from them (ported from pyanypia's policy.py).

#' @noRd
SERIES <- c("awi", "cola", "taxmax", "base77", "qc_amount", "yoc_specmin", "yoc_wep", "nra")

PRIMITIVES <- c(
  "awi_hist", "awi_growth", "cola_hist", "cola_proj", "taxmax_hist", "base77_hist",
  "pia_bend_base", "pia_pct", "mfb_bend_base", "mfb_pct", "ar_rate_first",
  "ar_rate_later", "ar_months_first", "spouse_ar_rate_first", "drc_rate", "qc_base",
  "spec_min_amount", "wep_enabled", "gpo_enabled", "gpo_fraction", "overrides"
)

statutory_nra <- function(elig_year) {
  ifelse(elig_year < 2000, 780,
         ifelse(elig_year < 2005, 780 + 2 * (elig_year - 1999),
                ifelse(elig_year < 2017, 792,
                       ifelse(elig_year < 2022, 792 + 2 * (elig_year - 2016), 804))))
}

statutory_drc <- function(elig_year) {
  ifelse(elig_year < 1979, 1 / 1200,
         ifelse(elig_year < 1987, 1 / 400,
                ifelse(elig_year < 2005, ((elig_year - 1985) %/% 2) / 2400 + 1 / 400,
                       2 / 300)))
}

#' Present-law policy
#'
#' The policy parameters every function reads: present law under a 2026
#' Trustees Report alternative. A `ranypia_policy` holds primitive inputs
#' (the average wage index history and projected growth, COLAs,
#' taxable-maximum history, the 1979 bend points, formula percentages,
#' reduction and credit rates, and so on) and the series derived from them
#' (`awi`, `cola`, `taxmax`, `qc_amount`, bend points via the AWI, the
#' special-minimum tables, ...), each indexed by `year - 1936` for
#' 1937-2105.
#'
#' Change primitives with [policy_replace()]; the derived series follow.
#' Pin individual derived values with [policy_with_series()].
#'
#' @param alt Trustees Report alternative: 1 (low cost), 2 (intermediate) or
#'   3 (high cost).
#' @return A `ranypia_policy`.
#' @export
#' @examples
#' pol <- current_law()
#' pol$taxmax[2026 - 1936]
current_law <- function(alt = 2) {
  if (!alt %in% 1:3) abort_field("alt", sprintf("must be 1, 2 or 3, not %s", alt))
  d <- trustees2026
  new_policy(list(
    awi_hist = d$FQ,
    awi_growth = d[[paste0("FQINC_PROJ_ALT", alt)]],
    cola_hist = d$CPIINC,
    cola_proj = d[[paste0("CPIINC_PROJ_ALT", alt)]],
    taxmax_hist = d$BASE_OASDI,
    base77_hist = d$BASE_77,
    pia_bend_base = c(180, 1085),
    pia_pct = c(0.90, 0.32, 0.15),
    mfb_bend_base = c(230, 332, 433),
    mfb_pct = c(1.50, 2.72, 1.34, 1.75),
    ar_rate_first = 5 / 900,
    ar_rate_later = 5 / 1200,
    ar_months_first = 36,
    spouse_ar_rate_first = 25 / 3600,
    drc_rate = NULL,
    qc_base = 250,
    spec_min_amount = 11.50,
    wep_enabled = FALSE,
    gpo_enabled = FALSE,
    gpo_fraction = 2 / 3,
    overrides = list()
  ))
}

#' Change policy parameters
#'
#' Returns a copy of `policy` with primitive fields changed; every derived
#' series is rebuilt, so a change to, say, `awi_growth` flows into the
#' taxable maximum and bend points.
#'
#' @param policy A `ranypia_policy`.
#' @param ... Fields to change, e.g. `pia_pct = c(0.9, 0.32, 0.10)`.
#' @return A `ranypia_policy`.
#' @export
#' @examples
#' reform <- policy_replace(current_law(), pia_pct = c(0.90, 0.30, 0.15))
policy_replace <- function(policy, ...) {
  changes <- list(...)
  bad <- setdiff(names(changes), PRIMITIVES)
  if (length(bad)) abort_field(bad[1], "is not a policy field you can replace")
  prims <- policy[PRIMITIVES]
  names(prims) <- PRIMITIVES
  for (nm in names(changes)) prims[nm] <- list(changes[[nm]])
  new_policy(prims)
}

#' Pin derived policy values
#'
#' Sets a derived series (`"awi"`, `"cola"`, `"taxmax"`, `"base77"`,
#' `"qc_amount"`, `"yoc_specmin"`, `"yoc_wep"` or `"nra"`) for specific
#' years. These are final values: later years are not re-projected from
#' them.
#'
#' @param policy A `ranypia_policy`.
#' @param name The series.
#' @param values A numeric vector named by year.
#' @return A `ranypia_policy`.
#' @export
#' @examples
#' p <- policy_with_series(current_law(), "taxmax", c("2030" = 250000))
policy_with_series <- function(policy, name, values) {
  if (!name %in% SERIES) {
    abort_field("series", sprintf("unknown series '%s'; expected one of %s", name,
                                  paste(SERIES, collapse = ", ")))
  }
  overrides <- policy$overrides
  current <- overrides[[name]]
  if (is.null(current)) current <- numeric()
  current[names(values)] <- values
  overrides[[name]] <- current
  policy_replace(policy, overrides = overrides)
}

new_policy <- function(p) {
  if (length(p$pia_pct) != length(p$pia_bend_base) + 1) {
    abort_field("pia_pct", "needs exactly one more entry than pia_bend_base")
  }
  if (length(p$mfb_pct) != 4 || length(p$mfb_bend_base) != 3) {
    abort_field("mfb_pct", "needs 4 percentages, with 3 mfb_bend_base points")
  }
  bad <- setdiff(names(p$overrides), SERIES)
  if (length(bad)) {
    abort_field("series", sprintf("unknown series '%s'; expected one of %s", bad[1],
                                  paste(SERIES, collapse = ", ")))
  }
  finish <- function(name, s) {
    o <- p$overrides[[name]]
    if (!is.null(o)) {
      yrs <- as.numeric(names(o))
      check(name, yrs < FIRST_YEAR | yrs > LAST_YEAR, "override year outside 1937-2105")
      s[yi(yrs)] <- as.numeric(o)
    }
    s
  }
  years <- FIRST_YEAR:LAST_YEAR
  hist_last <- function(x) max(as.numeric(names(x)))

  awi <- project_fq(to_series(p$awi_hist), to_series(p$awi_growth),
                    hist_last(p$awi_hist) + 1, LAST_YEAR)
  awi <- finish("awi", awi)
  cola <- to_series(p$cola_hist)
  proj <- to_series(p$cola_proj)
  cola[!is.na(proj)] <- proj[!is.na(proj)]
  cola <- finish("cola", cola)
  taxmax <- finish("taxmax", project_base(to_series(p$taxmax_hist), awi, cola, 0,
                                          hist_last(p$taxmax_hist) + 1, LAST_YEAR))
  base77 <- finish("base77", project_base(to_series(p$base77_hist), awi, cola, 2,
                                          hist_last(p$base77_hist) + 1, LAST_YEAR))
  qc_amount <- finish("qc_amount", project_qc_amounts(awi, LAST_YEAR, p$qc_base))
  post51 <- years >= 1951
  yoc_specmin <- ifelse(post51, ifelse(years < 1991, 0.25, 0.15) * base77, NA_real_)
  yoc_wep <- ifelse(post51, 0.25 * base77, NA_real_)
  nra <- finish("nra", statutory_nra(years))
  drc <- if (is.null(p$drc_rate)) statutory_drc(years) else rep(as.numeric(p$drc_rate), N_YEARS)
  sm <- project_special_min(cola, LAST_YEAR, p$spec_min_amount)

  structure(c(p, list(
    awi = awi, cola = cola, taxmax = taxmax, base77 = base77, qc_amount = qc_amount,
    yoc_specmin = finish("yoc_specmin", yoc_specmin), yoc_wep = finish("yoc_wep", yoc_wep),
    nra = nra, drc_rate_by_elig = drc,
    spec_min_pia = sm$pia, spec_min_mfb = sm$mfb,
    spec_min_pia_aug2001 = sm$pia_aug2001, spec_min_mfb_aug2001 = sm$mfb_aug2001
  )), class = "ranypia_policy")
}

#' @export
print.ranypia_policy <- function(x, ...) {
  cat("<ranypia_policy>\n")
  cat("  PIA formula:", paste0(x$pia_pct * 100, "%", collapse = " / "),
      "with 1979 bend points", paste(x$pia_bend_base, collapse = ", "), "\n")
  cat("  2026 taxable maximum:", format(x$taxmax[yi(2026)], big.mark = ","), "\n")
  cat("  WEP:", if (isTRUE(x$wep_enabled)) "on" else "off",
      " GPO:", if (isTRUE(x$gpo_enabled)) "on" else "off", "\n")
  n_over <- length(x$overrides)
  if (n_over) cat("  pinned series:", paste(names(x$overrides), collapse = ", "), "\n")
  invisible(x)
}
