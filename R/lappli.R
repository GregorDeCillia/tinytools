#' @include import.R
NULL

#' Improve `lapply()` and `sapply()` by passing on item index and item name
#'
#' Improve [base::lapply()] and [base::sapply()] functions by allowing
#' extra arguments `.i` (item index) and `.n` (item name) to be passed
#' into the function given in `FUN`.
#' Besides this extra feature, there is no difference to [base::lapply()] and
#' [base::sapply()].
#' @param FUN Here comes the great difference to [base::lapply()] and
#'   [base::sapply()]. When using `lappli` and `sappli`, the function
#'   passed into `FUN` may also have extra arguments `.i` (for item index) and
#'   `.n` for item name.
#' @inheritParams base::lapply
#' @examples
#' lappli(
#'   list(x1 = "x1", x2 = "x2"),
#'   function(x, before, .i, .n) paste0(before, "Entry-", .i, ": Argument ", .n, " = ", x),
#'   before = "###"
#' )
#' @rdname lappli
#' @export
lappli <- function(X, FUN, ...) {
  if (!is.function(FUN)) 
    stop("Unexpected argument in 'lappli'. Argument 'FUN' must be a function.")
  use_i <- ".i" %in% rlang::fn_fmls_names(FUN)
  use_n <- ".n" %in% rlang::fn_fmls_names(FUN)
  x_names <- names(X)
  X <- as.list(X)
  Y <- lapply(
    seq_along(X),
    function(i, ...) {
      if (use_i && use_n) {
        FUN(X[[i]], ..., .i = i, .n = x_names[i])
      } else if (use_i) {
        FUN(X[[i]], ..., .i = i)
      } else if (use_n) {
        FUN(X[[i]], ..., .n = x_names[i])
      } else {
        FUN(X[[i]], ...)
      }
    },
    ...
  )
  if (!is.null(x_names))
    names(Y) <- x_names
  Y
}

#' @inheritParams base::sapply
#' @examples
#' sappli(
#'   list(x1 = "x1", x2 = "x2"),
#'   function(x, before, .i, .n) paste0(before, "Entry-", .i, ": Argument ", .n, " = ", x),
#'   before = "###"
#' )
#' @rdname lappli
#' @export
sappli <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  if (!is.function(FUN)) 
    stop("Unexpected argument in 'sappli'. Argument 'FUN' must be a function.")
  if (!is.logical(USE.NAMES)) 
    stop("Unexpected argument in 'sappli'. Argument 'USE.NAMES' must be a 'TRUE' or 'FALSE'")
  use_i <- ".i" %in% rlang::fn_fmls_names(FUN)
  use_n <- ".n" %in% rlang::fn_fmls_names(FUN)
  x_names <- names(X)
  X <- as.list(X)
  Y <- sapply(
    seq_along(X),
    function(i, ...)  {
      if (use_i && use_n) {
        FUN(X[[i]], ..., .i = i, .n = x_names[i])
      } else if (use_i) {
        FUN(X[[i]], ..., .i = i)
      } else if (use_n) {
        FUN(X[[i]], ..., .n = x_names[i])
      } else {
        FUN(X[[i]], ...)
      }
    },
    ...,
    simplify = simplify,
    USE.NAMES = USE.NAMES
  )
  if (!is.null(x_names)) {
    names(Y) <- x_names
  } else if (USE.NAMES & is.character(X)) {
    names(Y) <- X
  }
  Y
}