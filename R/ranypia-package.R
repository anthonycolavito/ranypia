#' ranypia: Social Security benefit-formula functions
#'
#' Vectorized functions for the Social Security benefit formula, matching
#' SSA's Detailed Calculator (AnyPIA, 2026 Trustees Report) to the cent.
#' An R port of the Python package pyanypia.
#'
#' Conventions shared by every function:
#' * **Ages are whole months**, counted from the month of the day before
#'   birth (see [adjusted_birth()]).
#' * **Earnings** are a matrix with one row per worker (columns named by
#'   year, or `first_year =`), a named vector for one worker, or a long data
#'   frame with `id`, `year` and `earnings` columns.
#' * **`policy`** is the last argument everywhere, defaulting to
#'   [current_law()].
#'
#' @keywords internal
"_PACKAGE"
