# Builds R/sysdata.rda from data-raw/trustees2026.json (see make_sysdata.py).
# Run from the repo root: Rscript data-raw/make_sysdata.R
raw <- jsonlite::fromJSON("data-raw/trustees2026.json", simplifyVector = TRUE)
series <- function(x) stats::setNames(as.numeric(x$values), x$first + seq_along(x$values) - 1L)
trustees2026 <- lapply(raw, series)
save(trustees2026, file = "R/sysdata.rda", compress = "xz", version = 2)
cat("wrote R/sysdata.rda with", length(trustees2026), "series\n")
