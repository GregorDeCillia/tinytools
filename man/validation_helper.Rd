% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{validate_numeric_helper}
\alias{validate_numeric_helper}
\alias{validate_integer_helper}
\alias{validate_logical_helper}
\alias{validate_character_helper}
\alias{validate_vector_helper}
\title{Helper functions for the validaten functions}
\usage{
validate_numeric_helper(
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
)

validate_integer_helper(
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
)

validate_logical_helper(
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
)

validate_character_helper(
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
)

validate_vector_helper(
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
)
}
\arguments{
\item{obj}{The object that should be checked}

\item{err_h}{An error handling function created with
\link[composerr:composerr]{composerr()}. This handler will be called, if the
validation fails.}

\item{min_val}{A number or \verb{+/-Inf} used for the check: \code{obj >= min_val}.}

\item{max_val}{A number or \verb{+/-Inf} used for the check: \code{obj <= max_val}.}

\item{allow_null}{A logical value, defining if \code{NULL} is a allowed value for \code{obj}.}

\item{allow_inf}{A logical value, defining if \verb{+/-Inf} are allowed values for \code{obj}.}

\item{allow_na}{A logical value, defining if \code{NA} is allowed for \code{obj}.}

\item{allow_nan}{A logical value, defining if \code{NaN} is allowed for \code{obj}.}

\item{allow_duplicates}{A logical value, defining if duplicated
vector elements are allowed for \code{obj}.}

\item{min_len}{A number, defining the minimal length of the vector \code{obj}.}

\item{max_len}{A number, defining the maximal length of the vector \code{obj}.}

\item{len}{A number, defining the exact length of the vector \code{obj}.}

\item{named}{A logical value, defining if \code{obj} is required to be a named
vector (\code{named = TRUE}) or if \code{obj} is required to be an unnamed vector
(\code{named = FALSE}). If it does not matter wether \code{obj} is named or unnamed
then one should set \code{named = NULL}.}

\item{element_names}{Optional character vector holding the required names
for the vector elements. The order of the named elements does not matter.}

\item{allowed_names}{Optional character vector holding the allowed names
for the vector elements.}

\item{required_names}{Optional character vector holding the required names
for the vector elements.}

\item{forbidden_names}{Optional character vector holding the forbidden names
for the vector elements.}

\item{obj_name}{An optional string holding the name of the \code{obj}, which
should be used for referring to in the error message.}

\item{err_call}{An error handling function created with
\link[composerr:composerr]{composerr()}. This handler will be called, if the
passed in configuration arguments are invalid.}

\item{allowed_values}{An optional vector holding allowed values for the
vector elements.}
}
\description{
Helper functions for the validaten functions
}
