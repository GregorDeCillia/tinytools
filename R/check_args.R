#' @export
#' @rdname checks
check_atomic <- function(
  obj,
  err_h = composerr(),
  allow_null = FALSE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `check_atomic()`")
  composerr_validate(err_h, err_call)
  if (!is.numeric(min_len) || length(min_len) != 1L || !is.finite(min_len) ||
      min_len < 0 || as.integer(min_len) != min_len)
    err_call("Argument `min_len` must be a single finite positive integer or zero.")
  if (!is.numeric(max_len) || length(max_len) != 1L || is.na(max_len) ||
      is.nan(max_len) || max_len < 0 || as.integer(max_len) != max_len)
    err_call("Argument `max_len` must be a single positive integer or zero.")
  if (!is.null(len) && (!is.numeric(len) || length(len) != 1L ||
      !is.finite(len) || len < 0 || as.integer(len) != len))
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # check_atomic
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  err_h <- composerr(
    paste(stringify(obj_name, str_quote = "`")),
    err_h,
    sep_1 = " "
  )
  if (!isTRUE(allow_null) && is.null(obj)) 
    err_h("is `NULL`, but should not be `NULL`.")
  if (!is.null(obj) && !is.atomic(obj)) 
    err_h("is not an atomic object (numeric, complex, character, logical or raw).")
  if (!is.null(obj) && is.atomic(obj)) {
    if (length(obj) < min_len) 
      err_h(paste0(
        "has length ", stringify(length(obj)),
        ", but should have at least length ", stringify(min_len), "."
      ))
    if (length(obj) > max_len) 
      err_h(paste0(
        "has length ", stringify(length(obj)),
        ", but should have at most length ", stringify(max_len), "."
      ))
    if (!is.null(len) && length(obj) != len) 
      err_h(paste0(
        "has length ", stringify(length(obj)),
        ", but should have length ", stringify(len), "."
      ))
  }
  invisible(obj)
}

#' Useful argument checks with error handling
#'
#' @param obj The object that should be checked
#' @param err_h An error handling function, which will be called, when the
#'   check fails.
#' @param min_val A number or `+/-Inf` used for the check: `obj >= min_val`.
#' @param max_val A number or `+/-Inf` used for the check: `obj <= max_val`.
#' @param allow_null A logical value, defining if `NULL` is a allowed value for `obj`.
#' @param allow_inf A logical value, defining if `+/-Inf` are allowed values for `obj`.
#' @param allow_na A logical value, defining if `NA` is allowed for `obj`.
#' @param allow_nan A logical value, defining if `NaN` is allowed for `obj`.
#' @param min_len A number, defining the minimal length of the vector `obj`.
#' @param max_len A number, defining the maximal length of the vector `obj`.
#' @param len A number, defining the exact length of the vector `obj`.
#' @param obj_name An optional string holding the name of the `obj`, which
#'   should be used for referring to in the error message.
#' @return The object given in `obj`
#' @rdname checks
check_numeric <- function(
  obj,
  err_h,
  min_val = -Inf,
  max_val = Inf,
  allow_null = FALSE,
  allow_inf = TRUE,
  allow_na = FALSE,
  allow_nan = FALSE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `check_numeric()`")
  composerr_validate(err_h, err_call)
  if (!is.numeric(min_val) || length(min_val) != 1L || is.nan(min_val) || is.na(min_val))
    err_call("Argument `min_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.numeric(max_val) || length(max_val) != 1L || is.nan(max_val) || is.na(max_val))
    err_call("Argument `max_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.logical(allow_inf) || length(allow_inf) != 1L || is.na(allow_inf))
    err_call("Argument `allow_inf` must be a single non missing boolean value.")
  if (!is.logical(allow_na) || length(allow_na) != 1L || is.na(allow_na))
    err_call("Argument `allow_na` must be a single non missing boolean value.")
  if (!is.logical(allow_nan) || length(allow_nan) != 1L || is.na(allow_nan))
    err_call("Argument `allow_nan` must be a single non missing boolean value.")
  if (!is.numeric(min_len) || length(min_len) != 1L || !is.finite(min_len) ||
      min_len < 0 || as.integer(min_len) != min_len)
    err_call("Argument `min_len` must be a single finite positive integer or zero.")
  if (!is.numeric(max_len) || length(max_len) != 1L || is.na(max_len) ||
      is.nan(max_len) || max_len < 0 || as.integer(max_len) != max_len)
    err_call("Argument `max_len` must be a single positive integer or zero.")
  if (!is.null(len) && (!is.numeric(len) || length(len) != 1L ||
      !is.finite(len) || len < 0 || as.integer(len) != len))
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  check_atomic(
    obj,
    err_h = err_h,
    allow_null = allow_null,
    min_len = min_len,
    max_len = max_len,
    len = len,
    obj_name = obj_name
  )
  err_h <- composerr(
    paste(stringify(obj_name, str_quote = "`")),
    err_h,
    sep_1 = " "
  )
  if (!is.null(obj)) {
    id_inf <- which(is.infinite(obj))
    if (!isTRUE(allow_inf) && length(id_inf) > 0L) {
      if (max_len == 1L || len == 1L) {
        err_h("must not be `+/-Inf`, but is infinite.")
      } else {
        err_h(paste0(
          "must not have any `+/-Inf` entries, but the following entries are infinite:\n\t",
          stringify(id_inf)
        ))
      }
    }
    id_less <- which(obj < min_val)
    if (length(id_less) > 0L) {
      if (max_len == 1L || len == 1L) {
        err_h(paste0(
          "must not be small than ", stringify(min_val),
          ", but has the value ", stringify(obj)
        ))
      } else {
        err_h(paste0(
          "must not have any entries smaller than ", stringify(min_val),
          ", but the following entries are smaller:\n\t",
          stringify(id_less)
        ))
      }
    }
    id_greater <- which(obj > max_val)
    if (length(id_greater) > 0L) {
      if (max_len == 1L || len == 1L) {
        err_h(paste0(
          "must not be greater than ", stringify(max_val),
          ", but has the value ", stringify(obj)
        ))
      } else {
        err_h(paste0(
          "must not have any entries greater than ", stringify(max_val),
          ", but the following entries are greater:\n\t",
          stringify(id_greater)
        ))
      }
    }
  }
  invisible(obj)
}
