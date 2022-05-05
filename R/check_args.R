#' @include vector_manipulation.R
NULL

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
#' @export
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
  validate_composerr(err_h, err_call)
  if (!is.numeric(min_val) || length(min_val) != 1L || is.na(min_val))
    err_call("Argument `min_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.numeric(max_val) || length(max_val) != 1L || is.na(max_val))
    err_call("Argument `max_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.logical(allow_null) || length(allow_null) != 1L || is.na(allow_null))
    err_call("Argument `allow_null` must be a single non missing boolean value.")
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
      max_len < 0 || (is.finite(max_len) && as.integer(max_len) != max_len))
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
  check_vector(
    obj,
    err_h = err_h,
    allow_null = allow_null,
    min_len = min_len,
    max_len = max_len,
    len = len,
    obj_name = obj_name
  )
  err_h <- composerr(
    paste(stringify(obj_name, quote = "`")),
    err_h,
    sep_1 = " "
  )
  if (!is.null(obj)) {
    id_na <- which(is.na(obj) & !is.nan(obj))
    if (!isTRUE(allow_na) && length(id_na) > 0L) {
      if (max_len == 1L || len == 1L) {
        err_h("must not be `NA`.")
      } else {
        err_h(paste0(
          "must not have any `NA` entries, but the following entries are `NA`:\n\t",
          stringify(id_na)
        ))
      }
    }
    id_nan <- which(is.nan(obj))
    if (!isTRUE(allow_nan) && length(id_nan) > 0L) {
      if (max_len == 1L || len == 1L) {
        err_h("must not be `NaN`.")
      } else {
        err_h(paste0(
          "must not have any `NaN` entries, but the following entries are `NaN`:\n\t",
          stringify(id_nan)
        ))
      }
    }
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

#' @rdname checks
#' @export
check_integer <- function(
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
  err_call <- composerr("Error while calling `check_integer()`")
  validate_composerr(err_h, err_call)
  if (!is.numeric(min_val) || length(min_val) != 1L || is.na(min_val))
    err_call("Argument `min_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.numeric(max_val) || length(max_val) != 1L || is.na(max_val))
    err_call("Argument `max_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.logical(allow_null) || length(allow_null) != 1L || is.na(allow_null))
    err_call("Argument `allow_null` must be a single non missing boolean value.")
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
    max_len < 0 || (is.finite(max_len) && as.integer(max_len) != max_len))
    err_call("Argument `max_len` must be a single positive integer or zero.")
  if (!is.null(len) && (!is.numeric(len) || length(len) != 1L ||
    !is.finite(len) || len < 0 || as.integer(len) != len)
  )
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  check_numeric(
    obj,
    err_h = err_h,
    min_val = min_val,
    max_val = max_val,
    allow_null = allow_null,
    allow_inf = allow_inf,
    allow_na = allow_na,
    allow_nan = allow_nan,
    min_len = min_len,
    max_len = max_len,
    len = len,
    obj_name = obj_name
  )
  err_h <- composerr(
    paste(stringify(obj_name, quote = "`")),
    err_h,
    sep_1 = " "
  )
  if (!is.null(obj)) {
    id_nonint <- suppressWarnings(which(as.integer(obj) != obj))
    if (length(id_nonint) > 0L) {
      if (max_len == 1L || len == 1L) {
        err_h(paste(
          "must be an integer value, but has value:",
          stringify(format(obj, digits = 5))
        ))
      } else {
        err_h(paste0(
          "must be an integer vector, but the following entries are non integer numbers:\n\t",
          stringify(id_nonint)
        ))
      }
    }
  }
  invisible(obj)
}

#' @export
#' @rdname checks
check_vector <- function(
  obj,
  err_h = composerr(),
  allow_null = FALSE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `check_vector()`")
  validate_composerr(err_h, err_call)
  if (!is.logical(allow_null) || length(allow_null) != 1L || is.na(allow_null))
    err_call("Argument `allow_null` must be a single non missing boolean value.")
  if (!is.numeric(min_len) || length(min_len) != 1L || !is.finite(min_len) ||
      min_len < 0 || as.integer(min_len) != min_len)
    err_call("Argument `min_len` must be a single finite positive integer or zero.")
  if (!is.numeric(max_len) || length(max_len) != 1L || is.na(max_len) ||
      max_len < 0 || (is.finite(max_len) && as.integer(max_len) != max_len))
    err_call("Argument `max_len` must be a single positive integer or zero.")
  if (!is.null(len) && (!is.numeric(len) || length(len) != 1L ||
    !is.finite(len) || len < 0 || as.integer(len) != len)
  )
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # check_vector
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  err_h <- composerr(
    paste(stringify(obj_name, quote = "`")),
    err_h,
    sep_1 = " "
  )
  if (!isTRUE(allow_null) && is.null(obj)) 
    err_h("is `NULL`, but should be a vector.")
  if (!is.null(obj) && !is.atomic(obj)) 
    err_h("is not a vector (numeric, complex, character, logical or raw).")
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
    if (isTRUE(named)) {
      element_names <- names(obj)
      if (is.null(element_names))
        err_h("is an unnamed vector although it should be a named vector.")
      if (!is.null(allowed_names)) {
        err_h_names <- composerr("is a named vector, but has invalid element names", err_h)
        err_h_names <- composerr("The following vector element names are ", err_h_names, sep_1 = "")
        composerr_halt(err_h_names)
        wrong_names <- unique(element_names[!element_names %in% allowed_names])
        if (length(wrong_names) > 0)
          err_h_names(paste0(
            "not allowed:\n    ",
            stringify(wrong_names),
            "\n    Only the following list element names are allowed:\n    ",
            stringify(allowed_names)
          ))
        if (!is.null(required_names)) {
          missing_names <- unique(required_names[!required_names %in% element_names])
          if (length(missing_names) > 0)
            err_h_names(paste0(
              "missing although required:\n    ",
              stringify(missing_names),"."
            ))
        }
        if (!is.null(forbidden_names)) {
          forbidden_names <- unique(forbidden_names[forbidden_names %in% element_names])
          if (length(forbidden_names) > 0)
            err_h_names(paste0(
              "not allowed:\n    ",
              stringify(forbidden_names), "."
            ))
          
        }
        duplicate_names <- table(element_names)
        duplicate_names <- names(duplicate_names[duplicate_names > 1])
        if (length(duplicate_names) > 0)
          err_h_names(paste0(
            "used multiple times in the same list object:\n    ",
            stringify(duplicate_names), "."
          )) 
        composerr_flush(err_h_names)
      }
    } else if (isFALSE(named) && !is.null(names(obj))) {
      err_h("is a named list although it should be an unnamed list.")
    }
  }
  invisible(obj)
}

#' @export
#' @rdname checks
check_list <- function(
  obj,
  err_h = composerr(),
  allow_null = FALSE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  required_names = NULL,
  allowed_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `check_list()`")
  validate_composerr(err_h, err_call)
  if (!is.logical(allow_null) || length(allow_null) != 1L || is.na(allow_null))
    err_call("Argument `allow_null` must be a single non missing boolean value.")
  if (!is.numeric(min_len) || length(min_len) != 1L || !is.finite(min_len) ||
      min_len < 0 || as.integer(min_len) != min_len)
    err_call("Argument `min_len` must be a single finite positive integer or zero.")
  if (!is.numeric(max_len) || length(max_len) != 1L || is.na(max_len) ||
    max_len < 0 || (is.finite(max_len) && as.integer(max_len) != max_len))
    err_call("Argument `max_len` must be a single positive integer or zero.")
  if (min_len > max_len)
    err_call(paste0(
      "The value for `min_len` is not allowed to be greater than the value for ",
      "`max_len`, but `min_len` has the value ", stringify(min_len),
      " and `max_len` has the value ", stringify(max_len), "."
    ))
  if (!is.null(len) && (!is.numeric(len) || length(len) != 1L ||
    !is.finite(len) || len < 0 || as.integer(len) != len))
    err_call("Argument `len` must either be omitted or be a single positive integer or zero.")
  if (!is.null(len) && len < min_len)
    err_call(paste0(
      "The value for `min_len` is not allowed to be greater than the value for ",
      "`len`, but `min_len` has the value ", stringify(min_len),
      " and `len` has the value ", stringify(len), "."
    ))
  if (!is.null(len) && len > max_len)
    err_call(paste0(
      "The value for `max_len` is not allowed to be less than the value for ",
      "`len`, but `max_len` has the value ", stringify(max_len),
      " and `len` has the value ", stringify(len), "."
    ))
  if (!is.logical(allow_null) || length(allow_null) != 1L || is.na(allow_null))
    err_call("Argument `allow_null` must be a single non missing boolean value.")
  if (!is.null(named) && (
    !is.logical(named) || length(named) != 1L || is.na(named)))
    err_call("Argument `named` must be a single non missing boolean value or `NULL`.")
  if (!is.null(required_names) && (
    !is.character(required_names) || length(required_names) == 0L || any(is.na(required_names))))
    err_call(paste(
      "Argument `required_names` must be a character vector with non missing",
      "entries or `NULL`."
    ))
  if (!is.null(allowed_names) && (
    !is.character(allowed_names) || length(allowed_names) == 0L || any(is.na(allowed_names))))
    err_call(paste(
      "Argument `allowed_names` must be a character vector with non missing",
      "entries or `NULL`."
    ))
  if (!is.null(forbidden_names) && (
    !is.character(forbidden_names) || length(forbidden_names) == 0L || any(is.na(forbidden_names))))
    err_call(paste(
      "Argument `forbidden_names` must be a character vector with non missing",
      "entries or `NULL`."
    ))
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # check_vector
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  err_h <- composerr(
    paste(stringify(obj_name, quote = "`")),
    err_h,
    sep_1 = " "
  )
  if (!isTRUE(allow_null) && is.null(obj))
    err_h("is `NULL`, but should be a list.")
  if (!is.null(obj) && !is.list(obj))
    err_h("is not a list object.")
  if (is.list(obj)) {
    if (min_len == max_len)
      len <- min_len
    if (!is.null(len) && length(obj) != len) 
      err_h(paste0(
        "has", stringify(length(obj)), "list elements,",
        "but should have ", stringify(len), "list elements."
      ))
    if (length(obj) < min_len) 
      err_h(paste(
        "has", stringify(length(obj)), "list elements,",
        "but should have at least", stringify(min_len), "list elements."
      ))
    if (length(obj) > max_len) 
      err_h(paste0(
        "has", stringify(length(obj)), "list elements,",
        "but should have at most ", stringify(max_len), "list elements."
      ))
    if (isTRUE(named)) {
      element_names <- names(obj)
      if (is.null(element_names))
        err_h("is an unnamed list although it should be a named list.")
      if (!is.null(allowed_names)) {
        err_h_names <- composerr("is a named list, but has invalid list element names", err_h)
        err_h_names <- composerr("The following list element names are ", err_h_names, sep_1 = "")
        composerr_halt(err_h_names)
        wrong_names <- unique(element_names[!element_names %in% allowed_names])
        if (length(wrong_names) > 0)
          err_h_names(paste0(
            "not allowed:\n    ",
            stringify(wrong_names),
            "\n    Only the following list element names are allowed:\n    ",
            stringify(allowed_names)
          ))
        if (!is.null(required_names)) {
          missing_names <- unique(required_names[!required_names %in% element_names])
          if (length(missing_names) > 0)
            err_h_names(paste0(
              "missing although required:\n    ",
              stringify(missing_names),"."
            ))
        }
        if (!is.null(forbidden_names)) {
          forbidden_names <- unique(forbidden_names[forbidden_names %in% element_names])
          if (length(forbidden_names) > 0)
            err_h_names(paste0(
              "not allowed:\n    ",
              stringify(forbidden_names), "."
            ))
          
        }
        duplicate_names <- table(element_names)
        duplicate_names <- names(duplicate_names[duplicate_names > 1])
        if (length(duplicate_names) > 0)
          err_h_names(paste0(
            "used multiple times in the same list object:\n    ",
            stringify(duplicate_names), "."
          )) 
        composerr_flush(err_h_names)
      }
    } else if (isFALSE(named) && !is.null(names(obj))) {
      err_h("is a named list although it should be an unnamed list.")
    }
  }
  invisible(obj)
}
