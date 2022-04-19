#' @include import.R closure.R
NULL

#' Overwrite the default values of a given function
#' 
#' This functions takes a function and modifies its default values.
#' @param fn The function for which the defaults should be set.
#' @param ... Various named functions arguments, which should be set as default
#' @return A new function, with overwritten defaults
#' @export
set_fn_defaults <- function(fn, ...) {
  defaults <- plyr::compact(list(...))
  fn_new <- restrict_fn_env(
    vars = "defaults",
    fn = function(...) {
      do.call(
        fn,
        args = utils::modifyList(
          defaults,
          list(...),
          keep.null = TRUE
        )
      )
    }
  )
  if (rlang::is_primitive(fn)) {
    attr(fn_new, "args") <- defaults
  } else {
    attr(fn_new, "args") <- utils::modifyList(
      rlang::fn_fmls(fn),
      defaults,
      keep.null = TRUE
    )   
  }
  fn_new
}

#' Get all arguments (including their default values) of a function
#' 
#' This function only works for non-primitive functions.
#' @param fn A function.
#' @return A list holding all fn arguments and their default values.
#' @export
get_defaults <- function(fn) {
  args <- attr(fn, "args")
  if (is.null(args) && !rlang::is_primitive(fn)) {
    rlang::fn_fmls(fn)
  } else if (!is.null(args)) {
    args
  } else {
    NULL
  }
}