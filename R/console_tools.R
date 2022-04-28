#' @include import.R
NULL

#' Parse a copied range (possibly Excel) to a data.frame
#'
#' This function allows the user to copy a range (for example in Excel) and
#' paste it to a string variable
#' in R and then parse the string into a data.frame
#' @param x The copied string holding the values from the range
#' @param col_names Optional character vector holding the names which should be assigned to
#'   the data.frame columns. If omitted, then the column names `X1`, `X2` ,... are used.
#' @param num_cols Optional vector holding the column numbers which should be
#'   converted into a numeric columns. Of omitted, then all columns are assumed
#'   to be numeric.
#' @param big_mark A string holding the character symbol for the big mark in the
#'   numeric columns.
#' @param dec_mark A string holding the character symbol for the decimal mark.
#' @param col_sep A string holding the column separator.
#' @param row_sep A string holding the row separator.
#' @return A data.frame holding the parsed data.
#' @export
parse_copied_range <- function(x, col_names = NULL, num_cols = NULL, big_mark = ".", dec_mark = ",", col_sep = "\t", row_sep = "\n") {
  err_h <- composerr("Fehler waehrend des Aufrufs von 'parse_copied_range()'")
  if (!is.character(x) || length(x) != 1)
    err_h("Argument 'x' muss ein string sein.")
  x <- strsplit(strsplit(x, row_sep)[[1]], col_sep)
  x <- lapply(seq_len(length(x[[1]])), function(i) {
    unlist(lapply(x, function(y) y[i]))
  })
  if (is.null(col_names))
    col_names <- paste0("X", seq_len(length(x)))
  if (is.null(num_cols))
    num_cols <- seq_len(length(x))
  if (is.logical(num_cols))
    num_cols <- which(num_cols)
  if (length(x) != length(col_names))
    err_h(paste(
      sprintf(
        "Die Spaltenanzahl der kopierte Range (%d) in Argument 'x'",
        length(x)
      ),
      sprintf(
        "passt nicht mit der Anzahl der Spalten (%d) in Argument 'col_names' zusammen.",
        length(col_names)
      )
    ))
  if (!is.numeric(num_cols) || any(is.na(num_cols)) ||
      any(!num_cols %in% seq_len(length(x))))
    err_h("Argument 'num_cols' muss ein Vektor sein, der Spalten-Ids beinhaltet.")
  if (is.null(big_mark) || is.na(big_mark))
    big_mark <- ""
  if (!is.character(big_mark))
    err_h("Argument 'big_mark' muss ein character vector sein.")
  if (!is.character(dec_mark))
    err_h("Argument 'dec_mark' muss ein character vector sein.")
  names(x) <- col_names
  x[num_cols] <- lapply(
    x[num_cols],
    function(y)
      as.numeric(gsub(
        sprintf("[%s]", dec_mark),
        ".",
        gsub(sprintf("[%s]", big_mark), "", y)
      ))
  )
  data.frame(x)
}

#' The function `table` with `useNA = "always"`
#'
#' @param ... All arguments for `table`
#' @export
ttable <- function(...) {
  table(..., useNA = "always")
}
