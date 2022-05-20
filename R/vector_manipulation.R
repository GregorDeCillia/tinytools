#' @include import.R
NULL

#' Stringify vector
#'
#' Turn a vector (e.g. `c(1, 2, 3)`) into a string (e.g. `"'1', '2', '3'"`).
#' This function is particularly useful when creating error messages,
#' describing problematic values.
#' @param x An object, which should be turned into a string. Usually an atomic
#'   object (a vector).
#' @param quote A string, holding  the symbol which should be used for quoting
#'   every entry of `x`. Default is `'` which gives the result `'entry1'`.
#' @param collapse A string, used for separating each vector entry.
#' @param before A string placed before each vector entry.
#' @param after A string placed after each vector entry.
#' @param n_max A positive integer or `Inf`, defining the maximum number of
#'   displayed vector elements. All further elements are displayed with the
#'   text defined in `further`.
#' @param further A string indicating further (after `n_max` displayed elements)
#'   vector elements.
#' @return A string showing the entries of the vector.
#' @export
stringify <- function(
  x,
  before = NULL,
  after = NULL,
  collapse = ", ",
  quote = "'",
  n_max = 10,
  further = "..."
) {
  err_h <- function(msg)
    stop(paste("Error while calling `stringify()`:", msg), call. = FALSE)
  if (!is.null(collapse) && (
    !is.character(collapse) || length(collapse) != 1 || is.na(collapse)
  ))
    err_h("Argument `collapse` must be a non-missing string value or `NULL`.")
  if (!is.null(quote) && (
    !is.character(quote) || length(quote) != 1 || is.na(quote)
  ))
    err_h("Argument `quote` must be a non-missing string value or `NULL`.")
  if (!is.null(before) && (
    !is.character(before) || length(before) != 1 || is.na(before)
  ))
    err_h("Argument `before` must be a non-missing string value or `NULL`.")
  if (!is.null(after) && (
    !is.character(after) || length(after) != 1 || is.na(after)
  ))
    err_h("Argument `after` must be a non-missing string value or `NULL`.")
  if (!is.numeric(n_max) || length(n_max) != 1 || is.na(n_max) || n_max < 1 ||
    (is.finite(n_max) && as.integer(n_max) != n_max)
  )
    err_h("Argument `n_max` be a positive integer value or `Inf`.")
  if (!is.character(further) || length(further) != 1 || is.na(further))
    err_h("Argument `further` must be a non-missing string value.")
  # stringify
  x <- tryCatch(
    as.character(x),
    error = function(e) err_h(paste0(
      "Argument `x` could not be converted into a character vector:\n", e))
  )
  add_dots <- length(x) > n_max
  if (length(x) > n_max)
    x <- x[1:n_max]
  if (!is.null(quote))
    x <- c(
      paste0(quote, x, quote),
      if (add_dots) further else NULL
    )
  paste(paste0(before, x, after), collapse = collapse)
}

#' Shift vector
#' 
#' @param x A vector
#' @param by Number of positions to shift. Negative values produce a shift to
#'   the left
#' @param rotate Should the lost values be inserted as input values?
#'   If `rotate = FALSE`, then `NA` will be inserted instead.
#' @export
shift_vec <- function(x, by = 1L, rotate = FALSE) {
  len <- length(x)
  if (rotate == TRUE) {
    ind <- ((seq_len(len) - by - 1L) %% len) + 1L
  } else {
    ind <- seq_len(len) - by
    ind[which(ind <= 0)] <- NA
    ind[which(ind > len)] <- NA
  }
  x[ind]
}

#' Transform numeric vector to vector of strings
#' 
#' The following functions transform numbers to strings:
#' - `format_abs()`: formatted as absolute values (without decimal places)
#' - `format_rel()`: formatted as relative values (with decimal places)
#' 
#' This functions can be used to create LaTeX text or normal text.
#' For LaTeX text the minus symbol has some extra LaTeX formatting commands.
#' @param x A numerical vector.
#' @param signed A logical flag, defining if positive numbers should also have
#'   a leading signature sign.
#' @param latex_minus A logical value. If set to `TRUE`, then possible minus
#'   signs are replace by a special LaTeX command. If the resulting texts
#'   are compiled by a LaTeX compile, then the resulting text contains
#'   minus signs where no line break is possible between the minus sign and
#'   the following number. For this, the preamble of the surrounding LaTeX
#'   document must contain the following line of code:
#'   `\\usepackage[shortcuts]{extdash}`
#' @param ... Various other arguments passed into the [format()] function.
#'   These arguments can be used in order to change the behavior
#'   of `format_abs()`, `format_rel()`, etc. 
#' @rdname format_number
#' @export
format_abs <- function(x, signed = FALSE, latex_minus = FALSE, ...) {
  x_str <- unlist(lapply(
    abs(x),
    function(val) {
      do.call(
        format,
        args = utils::modifyList(
          list(
            x = round(val),
            nsmall = 0,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          ),
          list(...)
        )
      )   
    }
  ))
  paste0(
    ifelse(
      x >= 0,
      if (isTRUE(signed)) "+" else "",
      if (isTRUE(latex_minus)) "\\=/" else "-"
    ),
    x_str
  )
}

#' @param nsmall A positive integer, defining the minimum number of displayed
#'   decimal places
#' @param nsmall_max A positive integer, defining the maximum number of displayed
#'   decimal places
#' @rdname format_number
#' @export
format_rel <- function(x, signed = FALSE, latex_minus = FALSE, nsmall = 1, nsmall_max = 2, ...) {
  x_str <- unlist(lapply(
    {
      y <- round(abs(x)*10^nsmall)/10^nsmall
      ind <- which(y == 0)
      if (length(ind) > 0)
        y[ind] <- round(abs(x[ind])*10^nsmall_max)/10^nsmall_max
      y
    },
    function(val) {
      do.call(
        format,
        args = utils::modifyList(
          list(
            x = val,
            nsmall = nsmall,
            digits = 1,
            big.mark = ".",
            decimal.mark = ",",
            trim = TRUE,
            scientific = FALSE
          ),
          list(...)
        )
      )
    }
  ))
  paste0(
    ifelse(
      x >=0,
      if (isTRUE(signed)) "+" else "",
      if (isTRUE(latex_minus)) "\\=/" else "-"
    ),
    x_str
  )
}

