#' @include vector_manipulation.R
NULL

#' Calculate and add difference-variables for count variables in data.frames
#' 
#' Often we have data.frames which have counting variables like
#' - `n_geb`: number of born children
#' - `n_gest`: number of died people
#' - `n_pop`: number of people living in Tyrol
#' - etc.
#' 
#' For this variables we often want to calculate the variables:
#' - `d_*` (e.g. `d_geb`): The absolute difference (growth) compared to the previous year.
#' - `da_*` (e.g. `da_geb`): The relative difference (relative growth) compared to the previous year.
#' 
#' For this purpose we can have:
#' - an ordering prior to the calcuation: often `order = jahr`
#' - an optional grouping prior to the calculation (e.g. by `geschl` or some other grouping var)
#' 
#' The following functions can do this calculations:
#' - `create_d_vars()`: Calculate `d_*` and `da_*` for a set of given variables (mostly of type `n_*`)
#' - `create_d_vars_all()`: Calculate `d_*` and `da_*` for all variables except of a few given variables.
#' 
#' @param x A data.frame holding the data
#' @param n_var A character vector holding the names of the counting variables
#'   for which the difference variables should be created.
#' @param order_var An optional string holding the name of the variable, which
#'   is used for ordering the data.frame. If omitted, the
#'   pre-sorting is omitted.
#' @param grouping_var An optional string holding the name of the grouping variable.
#'   This variable is used for grouping the data.frame. If omitted, the
#'   pre-grouping is omitted.
#' @return A modified data.frame, holding the calculated variables.
#' @rdname create_d_vars
#' @export
create_d_vars <- function(
  x,
  n_var,
  order_var = NULL,
  grouping_var = NULL
) {
  create_d_vars_helper(
    x = x,
    n_var = n_var,
    order_var = order_var,
    grouping_var = grouping_var,
    err_h = composerr("Error while calling 'create_d_vars()': ")
  )
}

#' helper function for [create_d_vars()]
#' 
#' @inheritParams create_d_vars
#' @param err_h Error handling function.
create_d_vars_helper <- function(
  x,
  n_var,
  order_var = NULL,
  grouping_var = NULL,
  err_h = composerr("Error while calling 'create_d_vars_helper()': ")
) {
  all_vars <- c(n_var, order_var, grouping_var)
  missing_vars <- all_vars[!all_vars %in% names(x)]
  if (length(missing_vars) > 0)
    paste0(
      "The following variables could not be found:\n",
      stringify(missing_vars, before = "\t", collapse = "\n", new_line = TRUE)
    ) %>%
    err_h
  if (!is.null(grouping_var)) {
    x <- x %>%
      group_by(!!sym(grouping_var))
  }
  if (!is.null(order_var))
    x <- x %>%
    arrange(!!sym(order_var))
  var_list <- n_var %>%
    lapply(function(n_var) {
      list(
        n_var = n_var,
        d_var = gsub("^(n_)?(.*)", "d_\\2", n_var),
        da_var = gsub("^(n_)?(.*)", "da_\\2", n_var)
      )
    })
  lapply(
    var_list,
    function(v) {
      x <<- x %>%
        mutate(!!v$d_var := c(NA, diff(!!sym(v$n_var)))) %>%
        mutate(!!v$da_var := c(NA, diff(!!sym(v$n_var)))/shift_vec(!!sym(v$n_var)))
    }
  ) %>% invisible
  if (!is.null(grouping_var))
    x <- x %>% ungroup
  x
}

#' @param except_vars A character vector holding variables, which should
#'   not be used for longitudinal calculation.
#' @rdname create_d_vars
#' @export
create_d_vars_all <- function(
  x,
  order_var = NULL,
  grouping_var = NULL,
  except_vars = NULL
) {
  create_d_vars_helper(
    x = x,
    n_var = setdiff(
      names(x),
      unique(c(except_vars, order_var, grouping_var))
    ),
    order_var = order_var,
    grouping_var = grouping_var,
    err_h = composerr("Error while calling 'create_d_vars_all()': ")
  )
}