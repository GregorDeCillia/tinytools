#' @include vector_manipulation.R
NULL

#' Useful validations with proper error handling
#'
#' @param obj The object that should be checked
#' @param err_h An error handling function created with
#'   [composerr()][composerr::composerr()]. This handler will be called, if the
#'   validation fails.
#' @param min_val A number or `+/-Inf` used for the check: `obj >= min_val`.
#' @param max_val A number or `+/-Inf` used for the check: `obj <= max_val`.
#' @param allow_null A logical value, defining if `NULL` is a allowed value for `obj`.
#' @param allow_inf A logical value, defining if `+/-Inf` are allowed values for `obj`.
#' @param allow_na A logical value, defining if `NA` is allowed for `obj`.
#' @param allow_nan A logical value, defining if `NaN` is allowed for `obj`.
#' @param allow_duplicates A logical value, defining if duplicated
#'   vector elements are allowed for `obj`.
#' @param min_len A number, defining the minimal length of the vector `obj`.
#' @param max_len A number, defining the maximal length of the vector `obj`.
#' @param len A number, defining the exact length of the vector `obj`.
#' @param named A logical value, defining if `obj` is required to be a named
#'   vector (`named = TRUE`) or if `obj` is required to be an unnamed vector
#'   (`named = FALSE`). If it does not matter wether `obj` is named or unnamed
#'   then one should set `named = NULL`.
#' @param element_names Optional character vector holding the required names
#'   for the vector elements. The order of the named elements does not matter.
#' @param allowed_names Optional character vector holding the allowed names
#'   for the vector elements.
#' @param required_names Optional character vector holding the required names
#'   for the vector elements.
#' @param forbidden_names Optional character vector holding the forbidden names
#'   for the vector elements.
#' @param obj_name An optional string holding the name of the `obj`, which
#'   should be used for referring to in the error message.
#' @return The object given in `obj`
#' @export
#' @rdname validation
validate_numeric <- function(
  obj,
  err_h = composerr(),
  min_val = -Inf,
  max_val = Inf,
  allow_null = FALSE,
  allow_inf = TRUE,
  allow_na = FALSE,
  allow_nan = FALSE,
  allow_duplicates = TRUE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  element_names = NULL,
  allowed_names = NULL,
  required_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `validate_numeric()`: ")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  call_with_similar_args(
    validate_numeric_helper,
    obj_name = obj_name,
    err_call = err_call
  )
}

#' Helper functions for the validaten functions
#'
#' @param err_call An error handling function created with
#'   [composerr()][composerr::composerr()]. This handler will be called, if the
#'   passed in configuration arguments are invalid.
#' @inheritParams validate_numeric
#' @rdname validation_helper
validate_numeric_helper <- function(
  obj,
  err_h,
  min_val,
  max_val,
  allow_null,
  allow_inf,
  allow_na,
  allow_nan,
  allow_duplicates,
  min_len,
  max_len,
  len,
  named,
  element_names,
  allowed_names,
  required_names,
  forbidden_names,
  obj_name,
  err_call
) {
  validate_composerr(err_call, composerr("Error while calling `validate_numeric_helper()`: "))
  validate_composerr(err_h, err_call)
  if (!is.numeric(min_val) || length(min_val) != 1L || is.na(min_val))
    err_call("Argument `min_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.numeric(max_val) || length(max_val) != 1L || is.na(max_val))
    err_call("Argument `max_val` must be a single finite number or `Inf` or `-Inf`.")
  if (!is.logical(allow_inf) || length(allow_inf) != 1L || is.na(allow_inf))
    err_call("Argument `allow_inf` must be a single non missing logical value.")
  if (!is.logical(allow_nan) || length(allow_nan) != 1L || is.na(allow_nan))
    err_call("Argument `allow_nan` must be a single non missing logical value.")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  validate_vector_helper(
    obj,
    err_h = err_h,
    allow_null = allow_null,
    allow_na = allow_na,
    allow_duplicates = allow_duplicates,
    min_len = min_len,
    max_len = max_len,
    len = len,
    named = named,
    element_names = element_names,
    allowed_names = allowed_names,
    required_names = required_names,
    forbidden_names = forbidden_names,
    obj_name = obj_name,
    err_call = err_call
  )
  err_h <- composerr(
    paste0(stringify(obj_name, quote = "`"), " "),
    err_h
  )
  if (!is.null(obj)) {
    if (!is.numeric(obj))
      err_h("must be a numeric vector, but it is not.")
    err_h <- composerr(
      "  - The numeric vector ",
      composerr("is invalid:\n", err_h)
    ) %>%
      composerr_halt
    id_nan <- which(is.nan(obj))
    if (!isTRUE(allow_nan) && length(id_nan) > 0L) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h("must not be `NaN`.")
      } else {
        err_h(paste0(
          "must not have any `NaN` entries,\n    but the following element indices ",
          "hold `NaN`:\n    ",
          stringify(id_nan)
        ))
      }
    }
    id_inf <- which(is.infinite(obj))
    if (!isTRUE(allow_inf) && length(id_inf) > 0L) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h("must not be `+/-Inf`, but is infinite.")
      } else {
        err_h(paste0(
          "must not have any `+/-Inf` entries,\n    but the following element indices ",
          "hold infinite values:\n    ",
          stringify(id_inf)
        ))
      }
    }
    id_less <- which(obj < min_val)
    if (length(id_less) > 0L) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h(paste0(
          "must not be small than ", stringify(min_val),
          ", but has the value ", stringify(obj)
        ))
      } else {
        err_h(paste0(
          "must not have any elements smaller than ", stringify(min_val),
          ",\n    but the following element indices hold smaller values:\n    ",
          stringify(id_less)
        ))
      }
    }
    id_greater <- which(obj > max_val)
    if (length(id_greater) > 0L) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h(paste0(
          "must not be greater than ", stringify(max_val),
          ", but has the value ", stringify(obj)
        ))
      } else {
        err_h(paste0(
          "must not have any elements greater than ", stringify(max_val),
          ",\n    but the following element indices hold greater values:\n    ",
          stringify(id_greater)
        ))
      }
    }
    composerr_flush(err_h)
  }
  invisible(obj)
}

#' @rdname validation
#' @param allowed_values An optional vector holding allowed values for the
#'   vector elements.
#' @export
validate_integer <- function(
  obj,
  err_h = composerr(),
  min_val = -Inf,
  max_val = Inf,
  allowed_values = NULL,
  allow_null = FALSE,
  allow_inf = TRUE,
  allow_na = FALSE,
  allow_nan = FALSE,
  allow_duplicates = TRUE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  element_names = NULL,
  allowed_names = NULL,
  required_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `validate_integer()`: ")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  call_with_similar_args(
    validate_integer_helper,
    obj_name = obj_name,
    err_call = err_call
  )
}

#' @inheritParams validate_integer
#' @rdname validation_helper
validate_integer_helper <- function(
  obj,
  err_h,
  min_val,
  max_val,
  allowed_values,
  allow_null,
  allow_inf,
  allow_na,
  allow_nan,
  allow_duplicates,
  min_len,
  max_len,
  len,
  named,
  element_names,
  allowed_names,
  required_names,
  forbidden_names,
  obj_name,
  err_call
) {
  validate_composerr(err_call, composerr("Error while calling `validate_integer_helper()`: "))
  validate_composerr(err_h, err_call)
  if (!is.null(allowed_values)) {
    if (!is.numeric(allowed_values) || length(allowed_values) == 0)
      err_call(paste(
        "Argument `allowed_values` must either be an integer vector",
        "or omitted."
      ))
    id_float <- which(suppressWarnings(as.integer(allowed_values)) != allowed_values)
    if (length(id_float) > 0)
      err_call(paste(
        "Argument `allowed_values` must either be an integer vector ",
        " or omitted. The following indices are floating point values:\n  ",
        stringify(id_float)
      ))
    duplicate_values <- table(allowed_values, useNA = "always")
    duplicate_values <- duplicate_values[duplicate_values > 1]
    if (length(duplicate_values) > 0)
      err_call(paste(
        "Argument `allowed_values` has the following duplicate values:\n  ",
        stringify(names(duplicate_values))
      ))  
  }
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  validate_numeric_helper(
    obj,
    err_h = err_h,
    min_val = min_val,
    max_val = max_val,
    allow_null = allow_null,
    allow_inf = allow_inf,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_duplicates = allow_duplicates,
    min_len = min_len,
    max_len = max_len,
    named = named,
    element_names = element_names,
    allowed_names = allowed_names,
    required_names = required_names,
    forbidden_names = forbidden_names,
    len = len,
    obj_name = obj_name,
    err_call = err_call
  )
  err_h <- composerr(
    paste0(stringify(obj_name, quote = "`"), " "),
    err_h
  )
  if (!is.null(obj)) {
    if (!is.numeric(obj)) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h(paste(
          c(
            "must be an integer value",
            if (isTRUE(allow_null)) "or `NULL`",
            "."
          ),
          collpase = ""
        ))
      } else {
        err_h(paste(
          c(
            "must be an integer vector",
            if (isTRUE(allow_null)) "or `NULL`",
            "."
          ),
          collpase = ""
        ))
      }
    }
    err_h <- composerr(
      "  - The numeric vector ",
      composerr("is invalid:\n", err_h)
    ) %>%
      composerr_halt
    id_nonint <- suppressWarnings(which(as.integer(obj) != obj))
    if (length(id_nonint) > 0L) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h(paste(
          "must be an integer value, but has value:",
          stringify(format(obj, digits = 5))
        ))
      } else {
        err_h(paste0(
          "must be an integer vector,\n    but the following element indices hold ",
          "non-integer numbers:\n    ",
          stringify(id_nonint)
        ))
      }
    }
    if (!is.null(allowed_values)) {
      values_wrong <- unique(obj[!obj %in% allowed_values])
      if (length(values_wrong) > 0) {
        if (max_len == 1L || isTRUE(len == 1L)) {
          err_h(paste0(
            "has the integer value ", stringify(obj),
            ",\n    but only the following values are allowed:\n  ",
            stringify(allowed_values)
          ))
        } else {
          err_h(paste0(
            "is an integer vector,\n    but contains the following not-allowed values:\n    ",
            stringify(values_wrong),
            "\n    Only the following values are allowed:\n    ",
            stringify(allowed_values)
          ))
        }
      }
    }
    composerr_flush(err_h)
  }
  invisible(obj)
}

#' @export
#' @rdname validation
validate_logical <- function(
  obj,
  err_h = composerr(),
  allow_null = FALSE,
  allow_na = FALSE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  element_names = NULL,
  allowed_names = NULL,
  required_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) { 
  err_call <- composerr("Error while calling `validate_logical()`: ")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  call_with_similar_args(
    validate_logical_helper,
    obj_name = obj_name,
    err_call = err_call
  )
}

#' @rdname validation_helper
validate_logical_helper <- function(
  obj,
  err_h,
  allow_null,
  allow_na,
  min_len,
  max_len,
  len,
  named,
  element_names,
  allowed_names,
  required_names,
  forbidden_names,
  obj_name,
  err_call
) {
  validate_composerr(err_call, composerr("Error while calling `validate_logical_helper()`: "))
  validate_composerr(err_h, err_call)
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  validate_vector_helper(
    obj,
    err_h = err_h,
    allow_null = allow_null,
    allow_na = allow_na,
    min_len = min_len,
    max_len = max_len,
    named = named,
    element_names = element_names,
    allowed_names = allowed_names,
    required_names = required_names,
    forbidden_names = forbidden_names,
    len = len,
    obj_name = obj_name,
    err_call = err_call
  )
  err_h <- composerr(
    paste0(stringify(obj_name, quote = "`"), " "),
    err_h
  )
  if (!is.null(obj) && !is.logical(obj)) {
    if (max_len == 1L || isTRUE(len == 1L)) {
      err_h(paste(
        c(
          "must be a logical value",
          if (isTRUE(allow_null)) "or `NULL`",
          "."
        ),
        collpase = ""
      ))
    } else {
      err_h(paste(
        c(
          "must be a logical vector",
          if (isTRUE(allow_null)) "or `NULL`",
          "."
        ),
        collpase = ""
      ))
    }
  }
  invisible(obj)
}

#' @rdname validation
#' @export
validate_character <- function(
  obj,
  err_h = composerr(),
  allowed_values = NULL,
  allow_null = FALSE,
  allow_na = FALSE,
  allow_duplicates = TRUE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  element_names = NULL,
  allowed_names = NULL,
  required_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `validate_character()`: ")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  call_with_similar_args(
    validate_character_helper,
    obj_name = obj_name,
    err_call = err_call
  )
}

#' @inheritParams validate_integer
#' @rdname validation_helper
validate_character_helper <- function(
  obj,
  err_h,
  allowed_values,
  allow_null,
  allow_na,
  allow_duplicates,
  min_len,
  max_len,
  len,
  named,
  element_names,
  allowed_names,
  required_names,
  forbidden_names,
  obj_name,
  err_call
) {
  validate_composerr(err_call, composerr("Error while calling `validate_integer_helper()`: "))
  validate_composerr(err_h, err_call)
  if (!is.null(allowed_values)) {
    if (!is.character(allowed_values) || length(allowed_values) == 0)
      err_call(paste(
        "Argument `allowed_values` must either be an character vector",
        "or omitted."
      ))
    duplicate_values <- table(allowed_values, useNA = "always")
    duplicate_values <- duplicate_values[duplicate_values > 1]
    if (length(duplicate_values) > 0)
      err_call(paste(
        "Argument `allowed_values` has the following duplicate values:\n  ",
        stringify(names(duplicate_values))
      ))  
  }
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  validate_vector_helper(
    obj,
    err_h = err_h,
    allow_null = allow_null,
    allow_na = allow_na,
    allow_duplicates = allow_duplicates,
    min_len = min_len,
    max_len = max_len,
    named = named,
    element_names = element_names,
    allowed_names = allowed_names,
    required_names = required_names,
    forbidden_names = forbidden_names,
    len = len,
    obj_name = obj_name,
    err_call = err_call
  )
  err_h <- composerr(
    paste0(stringify(obj_name, quote = "`"), " "),
    err_h
  )
  if (!is.null(obj)) {
    if (!is.character(obj)) {
      if (max_len == 1L || isTRUE(len == 1L)) {
        err_h(paste(
          c(
            "must be a string value",
            if (isTRUE(allow_null)) "or `NULL`",
            "."
          ),
          collpase = ""
        ))
      } else {
        err_h(paste(
          c(
            "must be a character vector",
            if (isTRUE(allow_null)) "or `NULL`",
            "."
          ),
          collpase = ""
        ))
      }
    }
    if (!is.null(allowed_values)) {
      values_wrong <- unique(obj[!obj %in% allowed_values])
      if (length(values_wrong) > 0) {
        if (max_len == 1L || isTRUE(len == 1L)) {
          err_h(paste0(
            "has the string value ", stringify(obj),
            ",\n  but only the following strings are allowed:\n  ",
            stringify(allowed_values)
          ))
        } else {
          err_h(paste0(
            "is a character vector,\n  but contains the following not-allowed strings:\n  ",
            stringify(values_wrong),
            "\n  Only the following strings are allowed:\n  ",
            stringify(allowed_values)
          ))
        }
      }
    }
  }
  invisible(obj)
}

#' @export
#' @rdname validation
validate_vector <- function(
  obj,
  err_h = composerr(),
  allow_null = FALSE,
  allow_na = FALSE,
  allow_duplicates = TRUE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  element_names = NULL,
  allowed_names = NULL,
  required_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `validate_vector()`: ")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  call_with_similar_args(
    validate_vector_helper,
    obj_name = obj_name,
    err_call = err_call
  )
}

#' @rdname validation_helper
#' @export
validate_vector_helper <- function(
  obj,
  err_h,
  allow_null,
  allow_na,
  allow_duplicates,
  min_len,
  max_len,
  len,
  named,
  element_names,
  allowed_names,
  required_names,
  forbidden_names,
  obj_name,
  err_call
) {
  validate_composerr(err_call, composerr("Error while calling `validate_vector_helper()`: "))
  validate_composerr(err_h, err_call)
  if (!is.logical(allow_null) || length(allow_null) != 1L || is.na(allow_null))
    err_call("Argument `allow_null` must be a single non missing logical value.")
  if (!is.logical(allow_na) || length(allow_na) != 1L || is.na(allow_na))
    err_call("Argument `allow_na` must be a single non missing logical value.")
  if (!is.logical(allow_duplicates) || length(allow_duplicates) != 1L || is.na(allow_duplicates))
    err_call("Argument `allow_duplicates` must be a single non missing logical value.")
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
  if (!is.null(named) && (
    !is.logical(named) || length(named) != 1L || is.na(named)
  ))
    err_call("Argument `named` must either be a single non missing logical value or omitted.")
  if (!is.null(allowed_names)) {
    if (!is.character(allowed_names))
      err_call(paste(
        "Argument `allowed_names` must either be a character vector",
        "or omitted."
      ))
    duplicate_names <- table(allowed_names)
    duplicate_names <- duplicate_names[duplicate_names > 1]
    if (length(duplicate_names) > 0)
      err_call(paste(
        "Argument `allowed_names` has the following duplicate values:\n  ",
        stringify(names(duplicate_names))
      ))  
  }
  if (!is.null(required_names)) {
    if (!is.character(required_names) || any(is.na(required_names)))
      err_call(paste(
        "Argument `required_names` must either be a character vector",
        "or omitted."
      ))
    if (!is.character(required_names) ||
        any(is.na(required_names))
    )
      err_call("Argument `required_names` is not allowed to have `NA` entries.")
    duplicate_names <- table(required_names)
    duplicate_names <- duplicate_names[duplicate_names > 1]
    if (length(duplicate_names) > 0)
      err_call(paste(
        "Argument `required_names` has the following duplicate values:\n  ",
        stringify(names(duplicate_names))
      ))  
  }
  if (!is.null(forbidden_names)) {
    if (!is.character(allowed_names))
      err_call(paste(
        "Argument `allowed_names` must either be a character vector",
        "or omitted."
      ))
    duplicate_names <- table(allowed_names)
    duplicate_names <- duplicate_names[duplicate_names > 1]
    if (length(duplicate_names) > 0)
      err_call(paste(
        "Argument `allowed_names` has the following duplicate values:\n  ",
        stringify(names(duplicate_names))
      )) 
  }
  if (!is.null(element_names)) {
    if (!is.character(element_names))
      err_call(paste(
        "Argument `element_names` must either be a character vector",
        "or omitted."
      ))
    duplicate_names <- table(element_names)
    duplicate_names <- duplicate_names[duplicate_names > 1]
    if (length(duplicate_names) > 0)
      err_call(paste(
        "Argument `element_names` has the following duplicate values:\n  ",
        stringify(names(duplicate_names))
      ))  
    if (!is.null(forbidden_names)) {
      wrong_names <- forbidden_names[forbidden_names %in% element_names]
      if (length(wrong_names) > 0) {
        err_h_names(paste0(
          "The following element names given in `element_names` are also ",
          "listed in `forbidden_names`:\n  ",
          stringify(wrong_names),
          "\nPlease either use `forbidden_names` or `element_names`, but not both!"
        ))
      }
    }
    if (!is.null(allowed_names)) {
      wrong_names <- element_names[!element_names %in% allowed_names]
      if (length(wrong_names) > 0) {
        err_h_names(paste0(
          "The following element names given in `element_names` are not listed ",
          "in `allowed_names`:\n  ",
          stringify(wrong_names),
          "\nPlease either use `allowed_names` or `element_names`, but not both!"
        ))
      }
    }
    if (!is.null(required_names)) {
      wrong_names <- required_names[!required_names %in% element_names]
      if (length(wrong_names) > 0) {
        err_h_names(paste0(
          "The following element names given in `required_names` are not listed ",
          "in `element_names`:\n  ",
          stringify(wrong_names),
          "\nPlease either use `required_names` or `element_names`, but not both!"
        ))
      }
    }
  }
  if (isFALSE(named) && !is.null(allowed_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `allowed_names` must be",
      "`NULL`."
    ))
  if (isFALSE(named) && !is.null(required_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `required_names` must be",
      "`NULL`."
    ))
  if (isFALSE(named) && !is.null(forbidden_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `forbidden_names` must be",
      "`NULL`."
    ))
  if (isFALSE(named) && !is.null(element_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `element_names` must be",
      "`NULL`."
    ))
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # validate_vector
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  err_h <- composerr(
    paste0(stringify(obj_name, quote = "`"), " "),
    err_h
  )
  if (!isTRUE(allow_null) && is.null(obj)) 
    err_h("is `NULL`, but should be a vector.")
  if (!is.null(obj) && !is.atomic(obj)) 
    err_h("is not a vector (numeric, complex, character, logical or raw).")
  if (!is.null(obj) && is.atomic(obj)) {
    if (!is.null(len) && length(obj) != len) 
      err_h(paste0(
        "has length ", stringify(length(obj)),
        ", but should have length ", stringify(len), "."
      ))
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
    if (!is.null(allowed_names) || !is.null(required_names) ||
      !is.null(forbidden_names) || !is.null(element_names)
    )
      named <- TRUE
    e_names <- names(obj)
    if (isTRUE(named) && is.null(e_names))
      err_h("is an unnamed vector although it should be a named vector.")
    if (isFALSE(named) && !is.null(e_names))
      err_h("is a named vector although it should be an unnamed vector.")
    if (isTRUE(named)) {
      err_h_names <- composerr(
        "is a named vector, but has invalid element names:\n",
        err_h
      ) %>%
        composerr("  - The following vector element names are ", .) %>%
        composerr_halt
      if (!is.null(element_names)) {
        allowed_names <- element_names
        required_names <- element_names
      }
      if (!is.null(allowed_names)) {
        allowed_names[is.na(allowed_names)] <- ""
        wrong_names <- unique(e_names[!e_names %in% allowed_names])
        if (length(wrong_names) > 0)
          err_h_names(paste0(
            "not allowed:\n    ",
            stringify(wrong_names),
            "\n    Only the following element names are allowed:\n    ",
            stringify(allowed_names)
          ))
      }
      if (!is.null(required_names)) {
        missing_names <- unique(required_names[!required_names %in% e_names])
        if (length(missing_names) > 0)
          err_h_names(paste0(
            "missing although required:\n    ",
            stringify(missing_names),"."
          ))
      }
      if (!is.null(forbidden_names)) {
        forbidden_names[is.na(forbidden_names)] <- ""
        forbidden_names <- unique(forbidden_names[forbidden_names %in% e_names])
        if (length(forbidden_names) > 0)
          err_h_names(paste0(
            "not allowed:\n    ",
            stringify(forbidden_names), "."
          ))
        
      }
      if (isTRUE(named)) {
        duplicate_names <- table(e_names)
        duplicate_names <- names(duplicate_names[duplicate_names > 1])
        if (length(duplicate_names) > 0)
          err_h_names(paste0(
            "used for multiple elements:\n    ",
            stringify(duplicate_names), "."
          ))
      }
      composerr_flush(err_h_names)
    }
    err_h <- composerr(
      "  - The vector ",
      composerr("is invalid:\n", err_h)
    ) %>%
      composerr_halt
    if (isFALSE(allow_na)) {
      id_na <- which(is.na(obj))
      if (length(id_na) > 0L) {
        if (max_len == 1 || isTRUE(len == 1)) {
          err_h("is `NA`, although `NA` is not an allowed values.")
        } else {
          err_h(paste0(
            "must not contain `NA` values,",
            "\n    but has some missing values at the following indices:\n    ",
            stringify(id_na)
          ))
        }
      }
    }
    if (isFALSE(allow_duplicates)) {
      duplicate_values <- table(obj)
      duplicate_values <- duplicate_values[duplicate_values > 1]
      if (length(duplicate_values) > 0)
        err_h(paste(
          "The vector has the following duplicate values:\n    ",
          stringify(names(duplicate_values))
        ))
    }
    composerr_flush(err_h)
  }
  invisible(obj)
}

#' @export
#' @rdname validation
validate_list <- function(
  obj,
  err_h,
  allow_null = FALSE,
  min_len = 0L,
  max_len = Inf,
  len = NULL,
  named = NULL,
  element_names = NULL,
  allowed_names = NULL,
  required_names = NULL,
  forbidden_names = NULL,
  obj_name = NULL
) {
  err_call <- composerr("Error while calling `validate_list()`: ")
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # begin check
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  call_with_similar_args(
    validate_list_helper,
    obj_name = obj_name,
    err_call = err_call
  )
}

validate_list_helper <- function(
  obj,
  err_h,
  allow_null,
  min_len,
  max_len,
  len,
  named,
  element_names,
  allowed_names,
  required_names,
  forbidden_names,
  obj_name,
  err_call
) {
  validate_composerr(err_call, composerr("Error while calling `validate_list_helper()`: "))
  validate_composerr(err_h, err_call)
  validate_logical(allow_null, err_call, len = 1)
  validate_integer(min_len, err_call, len = 1, min_val = 0L)
  validate_integer(max_len, err_call, len = 1, min_val = 0L, allow_inf = TRUE)
  if (min_len > max_len)
    err_call(paste0(
      "The value for `min_len` is not allowed to be greater than the value for ",
      "`max_len`, but `min_len` has the value ", stringify(min_len),
      " and `max_len` has the value ", stringify(max_len), "."
    ))
  validate_integer(len, err_call, len = 1, min_val = 0L, allow_null = TRUE)
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
  validate_logical(named, err_call, len = 1, allow_null = TRUE)
  validate_character(
    allowed_names, err_call,
    allow_null = TRUE, allow_duplicates = FALSE, allow_na = TRUE
  )
  validate_character(
    required_names, err_call,
    allow_null = TRUE, allow_duplicates = FALSE
  )
  validate_character(
    forbidden_names, err_call,
    allow_null = TRUE, allow_duplicates = FALSE
  )
  validate_character(
    element_names, err_call,
    allow_null = TRUE, allow_duplicates = FALSE
  )
  if (!is.null(element_names)) {
    if (!is.null(forbidden_names)) {
      wrong_names <- forbidden_names[forbidden_names %in% element_names]
      if (length(wrong_names) > 0) {
        err_h_names(paste0(
          "The following element names given in `element_names` are also ",
          "listed in `forbidden_names`:\n  ",
          stringify(wrong_names),
          "\nPlease either use `forbidden_names` or `element_names`, but not both!"
        ))
      }
    }
    if (!is.null(allowed_names)) {
      wrong_names <- element_names[!element_names %in% allowed_names]
      if (length(wrong_names) > 0) {
        err_h_names(paste0(
          "The following element names given in `element_names` are not listed ",
          "in `allowed_names`:\n  ",
          stringify(wrong_names),
          "\nPlease either use `allowed_names` or `element_names`, but not both!"
        ))
      }
    }
    if (!is.null(required_names)) {
      wrong_names <- required_names[!required_names %in% element_names]
      if (length(wrong_names) > 0) {
        err_h_names(paste0(
          "The following element names given in `required_names` are not listed ",
          "in `element_names`:\n  ",
          stringify(wrong_names),
          "\nPlease either use `required_names` or `element_names`, but not both!"
        ))
      }
    }
  }
  if (isFALSE(named) && !is.null(allowed_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `allowed_names` must be",
      "`NULL`."
    ))
  if (isFALSE(named) && !is.null(required_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `required_names` must be",
      "`NULL`."
    ))
  if (isFALSE(named) && !is.null(forbidden_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `forbidden_names` must be",
      "`NULL`."
    ))
  if (isFALSE(named) && !is.null(element_names))
    err_call(paste(
      "If argument `named` is set to `FALSE`, then `element_names` must be",
      "`NULL`."
    ))
  if (!is.null(obj_name) && (!is.character(obj_name) || length(obj_name) != 1L || is.na(obj_name)))
    err_call("Argument `obj_name` must either be omitted or a string.")
  # validate_list
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(obj))
  }
  err_h <- composerr(
    paste0(stringify(obj_name, quote = "`"), " "),
    err_h
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
    
    if (!is.null(allowed_names) || !is.null(required_names) ||
        !is.null(forbidden_names) || !is.null(element_names)
    )
      named <- TRUE
    e_names <- names(obj)
    if (isTRUE(named) && is.null(e_names))
      err_h("is an unnamed list although it should be a named list.")
    if (isFALSE(named) && !is.null(e_names))
      err_h("is a named list although it should be an unnamed list.")
    if (isTRUE(named)) {
      err_h_names <- composerr(
        "is a named list, but has invalid element names:\n",
        err_h
      ) %>%
        composerr("  - The following list element names are ", .) %>%
        composerr_halt
      if (!is.null(element_names)) {
        allowed_names <- element_names
        required_names <- element_names
      }
      if (!is.null(allowed_names)) {
        allowed_names[is.na(allowed_names)] <- ""
        wrong_names <- unique(e_names[!e_names %in% allowed_names])
        if (length(wrong_names) > 0)
          err_h_names(paste0(
            "not allowed:\n    ",
            stringify(wrong_names),
            "\n    Only the following element names are allowed:\n    ",
            stringify(allowed_names)
          ))
      }
      if (!is.null(required_names)) {
        missing_names <- unique(required_names[!required_names %in% e_names])
        if (length(missing_names) > 0)
          err_h_names(paste0(
            "missing although required:\n    ",
            stringify(missing_names),"."
          ))
      }
      if (!is.null(forbidden_names)) {
        forbidden_names[is.na(forbidden_names)] <- ""
        forbidden_names <- unique(forbidden_names[forbidden_names %in% e_names])
        if (length(forbidden_names) > 0)
          err_h_names(paste0(
            "not allowed:\n    ",
            stringify(forbidden_names), "."
          ))
        
      }
      if (isTRUE(named)) {
        duplicate_names <- table(e_names)
        duplicate_names <- names(duplicate_names[duplicate_names > 1])
        if (length(duplicate_names) > 0)
          err_h_names(paste0(
            "used for multiple elements:\n    ",
            stringify(duplicate_names), "."
          ))
      }
      composerr_flush(err_h_names)
    }
  }
  invisible(obj)
}
