#' @include import.R
NULL

#' Stringify vector
#'
#' @param x A vector
#' @param quote A logical. If set to `TRUE`, then each entry of `x` is quoted with `'`.
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
  quote = TRUE,
  str_before = "",
  str_after = "",
  new_line = FALSE
) {
  (if (isTRUE(quote)) paste0("'", x, "'") else x) %>%
    paste0(str_before, ., str_after) %>%
    paste(collapse = str_collapse) %>%
    {
      if (isTRUE(new_line)) {
        paste0(., "\n")
      } else {
        .
      }
    }
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
  lapply(
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
  ) %>%
    unlist %>%
    paste0(
      ifelse(
        x >=0,
        if (isTRUE(signed)) "+" else "",
        if (isTRUE(latex_minus)) "\\=/" else "-"
      ),
      .
    )
}

#' @param nsmall A positive integer, defining the minimum number of displayed
#'   decimal places
#' @param nsmall_max A positive integer, defining the maximum number of displayed
#'   decimal places
#' @rdname format_number
#' @export
format_rel <- function(x, signed = FALSE, latex_minus = FALSE, nsmall = 1, nsmall_max = 2, ...) {
  lapply(
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
  ) %>%
    unlist %>%
    paste0(
      ifelse(
        x >=0,
        if (isTRUE(signed)) "+" else "",
        if (isTRUE(latex_minus)) "\\=/" else "-"
      ),
      .
    )
}

