# The year axis every policy series is stored on: 1937-2105.

FIRST_YEAR <- 1937
LAST_YEAR <- 2105
N_YEARS <- LAST_YEAR - FIRST_YEAR + 1

# A year-indexed vector from a named vector (names = years), NA elsewhere.
to_series <- function(values) {
  out <- rep(NA_real_, N_YEARS)
  yrs <- as.numeric(names(values))
  if (any(yrs < FIRST_YEAR | yrs > LAST_YEAR)) {
    abort_field("series", sprintf("years outside %d-%d", FIRST_YEAR, LAST_YEAR))
  }
  out[yrs - FIRST_YEAR + 1] <- as.numeric(values)
  out
}

# series[years], refusing years outside the data or without a value.
at <- function(series, years, name) {
  check(name, years < FIRST_YEAR | years > LAST_YEAR,
        sprintf("year outside %d-%d", FIRST_YEAR, LAST_YEAR))
  vals <- series[years - FIRST_YEAR + 1]
  if (anyNA(vals)) {
    abort_field(name, sprintf("no value for year %d", years[is.na(vals)][1]))
  }
  if (!is.null(dim(years))) dim(vals) <- dim(years)
  vals
}
