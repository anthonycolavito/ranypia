# Reads a fixture written by tools/make_fixtures.py. Whole numbers arrive
# from JSON as integers; ranypia works in doubles, so convert them.
as_doubles <- function(x) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, as_doubles)
    x
  } else if (is.list(x)) {
    lapply(x, as_doubles)
  } else if (is.integer(x)) {
    storage.mode(x) <- "double"
    x
  } else {
    x
  }
}

fixture <- function(name) {
  as_doubles(jsonlite::fromJSON(test_path("fixtures", paste0(name, ".json")),
                                simplifyVector = TRUE))
}
