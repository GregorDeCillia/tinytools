#' @include import.R
NULL

#' Add items to a list object
#'
#' @param obj A list object
#' @param ... Optional named or unnamed arguments which should be appended
#'   as new items to `obj`.
#' @param overwrite A logical value, defining if existing entries should be
#'   overwritten (`overwrite = TRUE`) or not.
#' @param remove_null A logical value, defining if for the resulting list object
#'   all `NULL` values should be stripped. 
#' @return The modified list object, holding additional list entries.
#' @export
add_list_items <- function(obj, ..., overwrite = TRUE, remove_null = FALSE) {
  if (isTRUE(overwrite)) {
    items <- list(...)
    items_n <- names(items)
    for (i in seq_along(items)) {
      if (items_n[i] == "") {
        obj[[length(obj) + 1]] <- items[[i]]
      } else {
        obj[[items_n[i]]] <- items[[i]]
      }
    }
  } else {
    obj <- append(
      obj,
      list(...)
    )
  }
  if (isTRUE(remove_null)) {
    obj <- plyr::compact(obj)
  }
  obj
}
