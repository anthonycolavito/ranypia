# Compares a worker tibble with a fixture's outputs, column by column.
expect_outputs <- function(got, want) {
  for (col in c("elig_year", "aime", "pia_elig", "pia", "mfb", "nra", "factor", "benefit",
                "method", "insured")) {
    expect_identical(got[[col]], want[[col]], label = col)
  }
}
