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

