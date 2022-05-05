#' @include vector_manipulation.R
NULL

#' Calculate a date vector
#' 
#' @param day A numeric vector holding the day number (1 - 31) of the month
#' @param month A numeric vector holding the month numbers (1 - 12)
#' @param year A numeric vector holding the year numbers (>= 2000)
#' @export
calc_date <- function(day, month, year) {
  as.Date(paste0(day, ".", month, ".", year), format = "%d.%m.%Y")
}

#' Calculate time difference between two dates
#' 
#' The following functions can be used to calculate the time difference between
#' two points of time:
#' - `calc_difftime_days()`: Calculates the number of days between two dates
#'   (e.g. `365` for `1.1.2019` and `1.1.2020`)
#' - `calc_difftime_years()`: Calculates the number of years (including fractional years) between
#'   to dates. This function also takes leap years into account.
#'   (e.g.: `1.000` for `1.3.2019` and `1.3.2020`)
#' @param start A date vector holding the start dates of the time interval.
#' @param end A date vector holding the end dates of the time interval. 
#'   Must be of the same length as `start`.
#' @rdname calc_difftime
#' @export
calc_difftime_days <- function(start, end) {
  calc_difftime_days_helper(
    start,
    end,
    err_h = composerr("Error while calling 'calc_difftime_days()': ")
  )
}

#' @rdname calc_difftime
#' @export
calc_difftime_years <- function(start, end) {
  days <- calc_difftime_days_helper(
    start,
    end,
    err_h = composerr("Error while calling 'calc_difftime_years()': ")
  )
  unlist(lapply(
    1:length(start),
    function(i) {
      if (is.na(start[i]) || is.na(end[i]))
        return(NA)
      start_year <- date_2_year(start[i])
      end_year <- date_2_year(end[i])
      years_inbetween <- start_year:(end_year + 1)
      if (calc_date(28, 2, start_year) < start[i]) {
        years_inbetween <- years_inbetween[2:length(years_inbetween)]
      }
      days_inbetween <- rep(365, length(years_inbetween))
      days_inbetween[is_leap_year(years_inbetween)] <- 366
      cumsum_days_inbetween <- cumsum(days_inbetween)
      years <- sum(cumsum_days_inbetween <= days[i])
      years + (days[i] - sum(cumsum_days_inbetween[years]))/days_inbetween[years+1]
    }
  ))
}

#' Helper function for calculating the time difference in days
#' 
#' @param err_h Error handling function.
#' @inheritParams calc_difftime_days
calc_difftime_days_helper <- function(
  start, end, err_h
) {
  len <- length(start)
  if (len != length(end))
    err_h("Arguments 'start' and 'end' must have the same length.")
  as.numeric(unlist(lapply(
    1:len,
    function(i) difftime(end[i], start[i], units = "days")
  )))
}

#' Leap year checker
#' 
#' @param x A numeric vector holding the year numbers
#' @return A logical vector, where each entry displays if the year in the
#'   corresponding `x` entry was a leap year.
#' @export
is_leap_year <- function(x) {
  mod(x, 4) == 0 & (mod(x, 100) != 0 || mod(x, 400) == 0)
}

#' Extract year number from date
#' 
#' @param date A date vector
#' @return The year number vector
#' @export
date_2_year <- function(date) {
  as.numeric(format(date, "%Y"))
}