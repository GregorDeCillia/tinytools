#' @include function_manipulation.R
NULL

#' Compose error handlers (concatenate error messages)
#'
#' This function appends character strings to the error messages
#' existing error handling functions.
#' An error handling function may look as follows:
#' `err_h <- function(msg) stop(paste("Error in Objekt 'x':", msg, 'Please try again!'))`.
#' With `composerr()` it is possible to add additional text between
#' - `"Error in Object 'x'"` and `msg` (use argument `text_1`)
#' - `msg` and `Please try again!` (use argument `text_2`)
#' @param text_1 A character string, which will be appended
#'   at the beginning of the error message. The argument `sep_1` will be used
#'   as text separator.
#' @param err_prior Optional error handling function
#'   to which the message part should be appended.
#' @param text_2 A character string, which will be appended
#'   at the end of the error message. The argument `sep_2` will be used
#'   as text separator.
#' @param sep_1 A character string that is used as separator for the
#'   concatenation of `text_1` at the beginning of the error message.
#' @param sep_2 A character string that is used as separator for the
#'   concatenation of `text_2` at the end of the error message.
#' @param handler_default An optional error handling function used
#'   as default for the argument `handler` of the returned error handling
#'   function `function(msg, handler) {handler(enrich_msg(msg))}`.
#' @return A new error handling function that has an extended error message.
#' @rdname composerr
#' @export
#' @examples 
#' \dontrun{
#' # ------     composerr      ----------
#' # -- create a modified error handler in the same scope --
#' # check if variable 'obj' exists and holds value TRUE
#' obj <- FALSE
#' # original error handler
#' err_h <- composerr("Something is wrong with obj")
#' if (!exists("obj"))
#'   err_h("obj does not exist")
#' # Error: create more precise error handler (same scope)
#' err_h2 <- composerr("obj has wrong value", "err_h")
#' if (!obj)
#'   err_h2("Value is FALSE", handler = warning)
#' #--- resulting WARNING ---
#' # Warning: Something is wrong with obj: obj has wrong value: Value is FALSE}
composerr <- function(
  text_1 = NULL,
  err_prior = NULL,
  text_2 = NULL,
  sep_1 = ": ",
  sep_2 = ": ",
  handler_default = NULL
) {
  err_handler_composerr <- function(msg) stop(paste("Error while calling `composerr()`:", msg), call. = FALSE)
  if (!is.null(text_1)) {
    if (!is.character(text_1))
      err_handler_composerr("argument 'text_1' must be a character string.")
    if (!is.character(sep_1) || length(sep_1) != 1)
      err_handler_composerr("argument 'sep_1' must be a character string.")
  }
  if (!is.null(text_2)) {
    if (!is.character(text_2))
      err_handler_composerr("argument 'text_2' must be a character string.")
    if (!is.character(sep_2) || length(sep_2) != 1)
      err_handler_composerr("argument 'sep_2' must be a character string.")
  }
  enrich_msg <- restrict_fn_env(
    function(msg = NULL) {
      paste(
        c(
          paste(c(text_1, msg), collapse = sep_1),
          text_2
        ),
        collapse = sep_2
      )
    },
    vars = c("text_1", "sep_1", "text_2", "sep_2")
  )
  # If no parent error handler is given, then setup a new error handler from scratch
  if (is.null(err_prior))
    err_prior <- restrict_fn_env(
      function(msg = NULL, handler = stop) {
        if (identical(handler, stop) || identical(handler, warning)) {
          handler(msg, call. = FALSE)
        } else {
          handler(msg)
        }
      }
    )
  if (!is.function(err_prior))
    err_handler_composerr("Argument `err_prior` must be a function or omitted.")
  if (!is.null(handler_default) && !is.function(handler_default))
    err_handler_composerr("Argument `handler_default` must be a function or omitted.")
  fn_args <- rlang::fn_fmls_names(err_prior)
  if (length(fn_args) == 0)
    err_handler_composerr("The function passed to argument `err_prior` must take at least one argument.")
  if ("handler" %in% fn_args) {
    return(restrict_fn_env(
      function(msg = NULL, handler = handler_default) {
        if (is.null(handler)) {
          err_prior(enrich_msg(msg))
        } else {
          err_prior(enrich_msg(msg), handler = handler)
        }
      },
      vars = c("err_prior", "enrich_msg", "handler_default")
    ))
  } else {
    return(restrict_fn_env(
      function(msg = NULL) {
        err_prior(enrich_msg(msg))
      },
      vars = c("err_prior", "enrich_msg")
    ))
  }
}
