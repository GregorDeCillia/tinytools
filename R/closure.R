#' @include import.R
NULL

#' Restrict function environment
#' 
#' Build a new function with an optimal scope. Normally the entire
#' environment tree (including the entire ancestry) is kept in memory,
#' but with `restrict_fn_env()` you get a function that has only
#' a copies of all needed variables and the scope ancestry holds only the
#' necessary environments (loaded packages when calling `restrict_fn_env()`).
#' @param fn A function whose parent scope should be restricted to the
#'   set of variables given in `vars` and the loaded packages.
#' @param vars An optional character vector holding the names of the variables
#'   which should be copied to the new function scope.
#' @param lookup_env The environment holding the variables given in `vars`.
#'   The default is the environment where the function `fn` was defined.
#' @return A new function with a small scope containing the variables given 
#'   in `vars`.
#' @export
restrict_fn_env <- function(fn, vars = NULL, lookup_env = environment(fn)) {
  new_env <- new.env(parent = parent.env(.GlobalEnv))
  for(v in vars) {
    assign(
      v,
      get(v, envir = lookup_env),
      envir = new_env
    )
  }
  environment(fn) <- new_env
  fn
}

#' Eval code in closure without scoping problems
#' 
#' @param lookup_env The environment holding the variables given in `vars`.
#'   The default is the calling environment.
#' @param expr The expression, which should be evaluated inside of the
#'   closure.
#' @inheritParams restrict_fn_env
#' @export
eval_closure <- function(
  expr,
  vars = NULL,
  lookup_env = parent.frame()
) {
  new_env <- new.env(parent = parent.env(.GlobalEnv))
  for(v in vars) {
    assign(
      v,
      get(v, envir = lookup_env),
      envir = new_env
    )
  }
  assign("expr", substitute(expr), envir = new_env)
  local(
    tryCatch(
      eval(expr),
      error = function(e) stop(
        "Error while calling `eval_closure()`: ",
        "The following expression could not be evaluated:\n  '",
        deparse(expr),
        "'\n",
        e,
        "Please pass the expression directly to `eval_closure()` and ensure ",
        "that all needed variables are specified in `vars`."
      )
    ),
    envir = new_env
  )
}
