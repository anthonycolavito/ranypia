# Builds R/sysdata.rda from data-raw/trustees2026.json (see make_sysdata.py)
# and data-raw/gdp_price_index_2026.csv.
# Run from the repo root: Rscript data-raw/make_sysdata.R
raw <- jsonlite::fromJSON("data-raw/trustees2026.json", simplifyVector = TRUE)
series <- function(x) stats::setNames(as.numeric(x$values), x$first + seq_along(x$values) - 1L)
trustees2026 <- lapply(raw, series)

# The GDP price index (1959 = 1) under the 2026 Trustees Report: history
# through 2025, and the low-cost (alternative 1), intermediate (2) and
# high-cost (3) projections from 2026 to 2100, stored as annual growth in
# percent so a policy can change it like the other projections.
gdp <- utils::read.csv("data-raw/gdp_price_index_2026.csv")
hist <- gdp[!is.na(gdp$Historical), ]
trustees2026$GDP_PRICE <- stats::setNames(hist$Historical, hist$Year)
last <- max(hist$Year)
for (alt in 1:3) {
  col <- c("Low", "Intermediate", "High")[alt]
  proj <- gdp[gdp$Year > last & !is.na(gdp[[col]]), ]
  level <- c(hist$Historical[hist$Year == last], proj[[col]])
  trustees2026[[paste0("GDP_PRICE_PROJ_ALT", alt)]] <-
    stats::setNames(100 * (level[-1] / level[-length(level)] - 1), proj$Year)
}

save(trustees2026, file = "R/sysdata.rda", compress = "xz", version = 2)
cat("wrote R/sysdata.rda with", length(trustees2026), "series\n")
