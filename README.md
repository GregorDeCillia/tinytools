
# tinytools

<!-- badges: start -->

[![GitHub last
commit](https://img.shields.io/github/last-commit/a-maldet/tinytools.svg?logo=github)](https://github.com/a-maldet/tinytools/commits/master)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/a-maldet/tinytools.svg?logo=github)](https://github.com/a-maldet/tinytools)
<!-- badges: end -->

``` r
library(tinytools)
```

This package contains a set of various small general purpose functions
that may be helpful for your daily R coding problems.

# Installation

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/tinytools', build_opts = NULL)
```

# Contained functions

## Vector manipulation

  - `stringify()`: Turn a vector into a single string. Useful for
    printing vector data in error or warning messages messages.
  - `shift_vec()`: Shift the entries of a vector to the left or to the
    right.
  - `format_abs()`: Turn a numeric vector into a character vector
    formatted as absolute numbers. Useful for printing numerical data in
    `rmd` or `rnw`-reports.
  - `format_rel()`: Same as `format_abs()`, but for printing decimal
    numbers with a specific number of digits after the decimal mark.

## Date manipulation

  - `calc_date()`: Calculate a vector holding date objects from a day,
    month and year number.
  - `calc_difftime_days()`: Calculate a diff vector holding the number
    of days between two vectors with date objects as entries.
  - `calc_difftime_years()`: Calculate the diff vector holding the
    number of years (also including fractional years) between two
    vectors with date objects as entries. This function also takes leap
    years into account and can be used for calculating the age of a
    person. Only for persons born on the 29th of february there is an
    exception, and their birth is mapped to the 1st of march in order to
    be able to compute their age.
  - `is_leap_year()`: Returns a logical answer to the question if a
    specific year is a leap year.
  - `date_2_year()`: Extracts a numeric vector holding year numbers from
    a vector holding date objects.

## Calculate difference measures

  - `create_d_vars()`: Calculate absolute and relative difference
    vectors for one or multiple data.frame variables. E.g. if we have
    timevariable like a `year` a measurement variable like the
    population count `n_pop`. Then function `create_d_vars()` can be
    used in order to calculate the following variables:
      - `d_pop`: Holding the absolute yearly growth of the population
        `n_pop`.
      - `da_pop`: Holding the relative yearly growth of the population
        `n_pop`
  - `create_d_vars_all()`: Like `create_d_vars()`, but using all columns
    except of specific set, defined by the user.

## List manipulation

  - `lappli()`: Same as `lapply()`, but also passing the variables `.i`
    and `.n` holding the item index and the item name to the given
    function `function(x, ..., .i, .n) {...}`.
  - `sappli()`: Same as `sapply()`, but also passing the variables `.i`
    and `.n` holding the item index and the item name to the given
    function `function(x, ..., .i, .n) {...}`.
  - `add_list_items()`: A function that adds new list entries to a given
    list object.

## Advanced R coding

  - `init_text_builder()`: Initialize a `text_builder`. This is a tool
    for incrementally adding text blocks. One use case can be creating
    large LaTeX code snippets in R.
  - `set_fn_defaults()`: Set the defaults for a given function. Can be
    useful for customizing various functions.
  - `get_defaults()`: Return all function arguments including their
    default values for a given function.

## Validation

These functions help validating objects a user passed into your
functions (useful for package development). These functions allow to
specify various requirements on the objects and automatically throw
meaningful error messages, in case of failure. These functions should be
used together with the
[composerr](https://github.com/a-maldet/composerr)) package. The
following validations are available:

  - `validate_vector()`: Check if an object is atomic
  - `validate_logical()`: Check if an object is a logical vector
  - `validate_character()`: Check if an object is a character vector
  - `validate_numeric()`: Check if an object is a numeric vector
  - `validate_integer()`: Check if an object is an integer vector
  - `validate_list()`: Check if an object is a list

## Minor console tools

  - `ttable()`: Same as `table()`, but setting `useNA = "always"`.
  - `parse_copied_range()`: With this function you can parse a pasted
    range (Excel, HTML etc.) into a data.frame. This function is useful
    when you want to quickly access some structured data without being
    bothered by file access code.
