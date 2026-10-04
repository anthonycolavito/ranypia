fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", paste0(name, ".json")), simplifyVector = TRUE)
}
