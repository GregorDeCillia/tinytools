#' @include import.R
NULL

#' Stringify vector
#'
#' Turn a vector (e.g. `c(1, 2, 3)`) into a string (e.g. `"'1', '2', '3'"`).
#' This function is particularly useful when creating error messages,
#' describing problematic values.
#' @param x An object, which should be turned into a string. Usually an atomic
#'   object (a vector).
#' @param str_quote A string, holding  the symbol which should be used for quoting
#'   every entry of `x`. Default is `'` which gives the result `'entry1'`.
#' @param str_collapse A string, used for separating each vector entry.
#' @param str_before A string placed before each vector entry.
#' @param str_after A string placed after each vector entry.
#' @param new_line A logical. If set to `TRUE`, then a new line command is
#'    placed at the end of the resulting string.
#' @return A string showing the entries of the vector.
#' @export
stringify <- function(
  x,
  str_collapse = ", ",
  str_quote = "'",
  str_before = "",
  str_after = "",
  new_line = FALSE
) {
  err_h <- function(msg)
    stop(paste("Error while calling `stringify()`:", msg), call. = FALSE)
  if (!is.null(str_collapse) && (
    !is.character(str_collapse) || length(str_collapse) > 1 || is.na(str_collapse)
  ))
    err_h("Argument `str_collapse` must be a non-missing string value or `NULL`.")
  if (!is.null(str_quote) && (
    !is.character(str_quote) || length(str_quote) > 1 || is.na(str_quote)
  ))
    err_h("Argument `str_quote` must be a non-missing string value or `NULL`.")
  if (!is.null(str_before) && (
    !is.character(str_before) || length(str_before) > 1 || is.na(str_before)
  ))
    err_h("Argument `str_before` must be a non-missing string value or `NULL`.")
  if (!is.null(str_after) && (
    !is.character(str_after) || length(str_after) > 1 || is.na(str_after)
  ))
    err_h("Argument `str_after` must be a non-missing string value or `NULL`.")
  if (!is.logical(new_line) || length(new_line) > 1 || is.na(new_line))
    err_h("Argument `new_line` must be a non-missing logical value.")
  # stringify
  x <- tryCatch(
    as.character(x),
    error = function(e) err_h(paste0(
      "Argument `x` could not be converted into a character vector:\n", e))
  )
  if (!is.null(str_quote))
    x <- paste0(str_quote, x, str_quote)
  x <- paste(paste0(str_before, x, str_after), collapse = str_collapse)
  if (isTRUE(new_line))
    x <- paste0(x, "\n")
  x
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
            x = round(x),
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

