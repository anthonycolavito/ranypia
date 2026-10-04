# Input coercion and validation shared by every exported function.

abort_field <- function(name, what) {
  rlang::abort(paste0(name, ": ", what), class = "ranypia_error")
}

# Raises if any element of `bad` is TRUE, naming the count and first row.
check <- function(name, bad, what) {
  dim_bad <- dim(bad)
  bad <- as.logical(bad)
  if (!any(bad, na.rm = TRUE)) {
    return(invisible())
  }
  if (length(bad) == 1L) abort_field(name, what)
  rows <- if (is.null(dim_bad)) bad else rowSums(matrix(bad, nrow = dim_bad[1])) > 0
  idx <- which(rows)
  abort_field(name, sprintf("%s in %d row(s); first at row %d", what, length(idx), idx[1]))
}

as_num <- function(name, x) {
  if (!is.numeric(x) && !is.logical(x)) abort_field(name, "must be numeric")
  x <- as.numeric(x)
  check(name, !is.finite(x), "must be finite")
  x
}

as_whole <- function(name, x) {
  if (is.logical(x)) abort_field(name, "must be whole numbers, not logical")
  x <- as_num(name, x)
  check(name, x != floor(x), "must be whole numbers")
  x
}

# Recycles each argument to the common length n (each must be length 1 or n).
recycle_to <- function(x, name, n) {
  if (length(x) == n) return(x)
  if (length(x) == 1L) return(rep(x, n))
  abort_field(name, sprintf("has length %d; expected 1 or %d", length(x), n))
}

common_length <- function(...) {
  lens <- vapply(list(...), length, integer(1))
  max(lens, 1L)
}

# Earnings as a matrix (rows = workers) plus its first year and row ids.
# Accepts a matrix (columns named by year, or `first_year`), a named
# numeric vector (one worker), an unnamed vector with `first_year`, or a
# long data frame with `id`, `year` and `earnings` columns.
as_earnings <- function(earnings, first_year = NULL) {
  ids <- NULL
  if (is.data.frame(earnings)) {
    wide <- earnings_matrix(earnings)
    ids <- attr(wide, "ids")
    m <- wide
    first <- as.numeric(colnames(wide)[1])
  } else if (is.matrix(earnings)) {
    m <- earnings
    if (is.null(first_year)) {
      if (is.null(colnames(m))) {
        abort_field("first_year", "required when the earnings matrix has no year column names")
      }
      yrs <- as.numeric(colnames(m))
      if (anyNA(yrs) || any(diff(yrs) != 1)) {
        abort_field("earnings", "column names must be consecutive years")
      }
      first <- yrs[1]
    } else {
      first <- as.numeric(first_year)
    }
  } else if (is.numeric(earnings) && !is.null(names(earnings)) && is.null(first_year)) {
    yrs <- as.numeric(names(earnings))
    if (anyNA(yrs)) abort_field("earnings", "names must be years")
    first <- min(yrs)
    m <- matrix(0, nrow = 1, ncol = max(yrs) - first + 1)
    m[1, yrs - first + 1] <- earnings
  } else if (is.numeric(earnings)) {
    if (is.null(first_year)) {
      abort_field("first_year", "required when earnings has no year names")
    }
    first <- as.numeric(first_year)
    m <- matrix(earnings, nrow = 1)
  } else {
    abort_field("earnings", "must be a matrix, a named numeric vector or a long data frame")
  }
  storage.mode(m) <- "double"
  check("earnings", !is.finite(m), "not finite")
  check("earnings", m < 0, "negative")
  if (first < FIRST_YEAR || first + ncol(m) - 1 > LAST_YEAR) {
    abort_field("earnings", sprintf("years must lie within %d-%d", FIRST_YEAR, LAST_YEAR))
  }
  dimnames(m) <- NULL
  list(m = m, first = first, ids = ids)
}

#' Long earnings to a wide matrix
#'
#' Turns panel data with one row per worker-year into the matrix the other
#' functions use: one row per worker (in order of first appearance of `id`),
#' one column per year from the earliest to the latest, zero where a year is
#' missing.
#'
#' @param data A data frame with columns `id`, `year` and `earnings`.
#' @return A numeric matrix with year column names and an `ids` attribute.
#' @export
#' @examples
#' panel <- data.frame(id = c(1, 1, 2), year = c(2000, 2001, 2001),
#'                     earnings = c(30000, 31000, 50000))
#' earnings_matrix(panel)
earnings_matrix <- function(data) {
  needed <- c("id", "year", "earnings")
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    abort_field("earnings", paste("long data needs columns", paste(missing, collapse = ", ")))
  }
  ids <- unique(data$id)
  years <- seq(min(data$year), max(data$year))
  wide <- tidyr::pivot_wider(
    tibble::tibble(id = data$id, year = data$year, earnings = as.numeric(data$earnings)),
    names_from = "year", values_from = "earnings", values_fill = 0,
    names_sort = TRUE
  )
  wide <- wide[match(ids, wide$id), , drop = FALSE]
  m <- matrix(0, nrow = length(ids), ncol = length(years),
              dimnames = list(NULL, as.character(years)))
  cols <- setdiff(names(wide), "id")
  m[, cols] <- as.matrix(wide[, cols])
  attr(m, "ids") <- ids
  m
}
