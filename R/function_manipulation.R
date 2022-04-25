#' @include import.R
#' @include list_manipulation.R
NULL

#' Restrict function environment
#' 
#' Build a new function with an optimal scope. Normally the entire
#' environment tree (including the entire ancestry) is kept in memory,
#' but with `restrict_fn_env()` you get a function that has only
#' a copies of all needed variables and the scope ancestry holds only the
#' global environment.
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
  new_env <- new.env(parent = .GlobalEnv)
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
  new_env <- new.env(parent = .GlobalEnv)
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
      ),
      finally = rm(expr)
    ),
    envir = new_env
  )
}

#' List all passed in function arguments
#' 
#' This functions returns a named list holding the argument values of the
#' current function call.
#' This includes:
#'   - arguments assigned by name (e.g. `foo(x = 1)`)
#'   - arguments assigned by position (e.g. `foo(1)`)
#'   - arguments for which the default value was overwritten (e.g. `foo(x = 1)` with `foo <- function(x = 99)`)
#'   - arguments for which the default value was used (e.g. `foo()` with `foo <- function(x = 99)`
#'   - named and unnamed arguments inside of a three dots elipsis
#' @param n The number ob frames to go back. The default value `n = 0L` means,
#'   that the given arguments of current function should be returned.
#' @return A named list holding the values of all call arguments
#' @export
get_call_args <- function(n = 0L) {
  plyr::defaults(
    # all passed in arguments from the call
    as.list(match.call(
      definition = sys.function(sys.parent(n + 1L)),
      call = sys.call(sys.parent(1L + n)),
      envir = parent.frame(2L + n)
    )) %>%
      {
        .[2:length(.)]
      },
    # possible defaults values of the function
    rlang::fn_fmls(fn = rlang::caller_fn(n = n + 1L)) %>%
      as.list %>%
      {
        .[names(.) != "..."]
      } %>%
      {
        .[lapply(., rlang::is_missing) %>% unlist %>% `!`]
      }
  )
}

#' Call a function with similar arguments as current function call
#' 
#' This function allows can be called from within a function call 
#' (e.g. `foo(x = 1, y = 2)`) in order to call another function
#' (e.g. `baz(x = 1, y = 2, z = 3`) with using the same (or
#' almost the same) arguments as in the current function call
#' (e.g. by calling `call_fn_with_similar_args(baz, z = 3)` inside of `foo()`).
#' @param fn The function that should be called.
#' @param ... Additional function parameters, which should be added to the 
#'   arguments of the current function call.
#' @param skip An optional character vector holding names of
#'   arguments of the current function call which should **not be used** for the
#'   call of `fn`.
#' @param n The number ob frames to go back. The default value `n = 0L` means,
#'   that the arguments of the current function call should be used.
#' @return The return value of the called function given in `fn`.
#' @export
call_with_similar_args <- function(fn, ..., skip = NULL, n = 0L) {
  err_h <- composerr("Error while calling `call_fn_with_similar_args()`")
  if (!is.function(fn))
    err_h("Argument `fn` must be a function.")
  if ((!is.null(skip) && !is.character(skip)) || (is.character(skip) && any(is.na(skip)))) {
    err_h("Argument `skip` must either be `NULL` or a character vector without `NA` entries.")
  }
  if (!is.numeric(n) || length(n) != 1 || !is.finit(n) || n < 0L || as.integer(n) != n)
    err_h("The argument `n` must be a finite positive integer.")
  args <- get_call_args(n = n + 1L)
  wrong_names <- skip[!skip %in% names(args)]
  if (length(wrong_names) > 0L)
    paste0(
      "The following variables passed in argument `skip`",
      "are no arguments of the current function call:\n\t",
      stringify(wrong_names),
      "\nOnly the following are available:\n\t",
      stringify(names(args)),
      "\n"
    ) %>%
    err_h
  args <- args[!names(args) %in% skip] %>%
    (plyr::defaults)(
      list(...),
      .
    )
  tryCatch(
    do.call(
      fn,
      args = args
    ),
    error = function(e) paste0("Could not evaluate the function given in argument `fn`: ", e) %>%
      err_h
  )
}
