#' Manage API calls in last 10 seconds
#'
#' Manages list of all API calls made in the last 10 seconds. If no tracker is
#' provided, a new one is returned. If an already existing tracker is passed as
#' an argument to the function, the function will add a new call to it, then
#' remove all items with a timestamp > 10 seconds from current system time.
#'
#' The SCB API limits calls to 10 per 10 seconds (rolling). If this number is
#' exceeded, \code{\link[httr]{GET}} returns a 429 response. We could use
#' \code{\link[base]{Sys.sleep}} with time set to 10 seconds when this occurs,
#' to reset the counter, but this would be inefficient: since the limit is
#' rolling, we should only wait until the oldest call is > 10 seconds in the
#' past, and then attempt a new query. This is what this function facilitates.
#'
#' @param tracker Currently tracked calls
#' @return data.frame containing currently tracked calls
update_call_tracker <- function(tracker = NULL) {

  if (is.null(tracker)) {

    # No tracker set up, create new
    tracker <- data.frame(timestamp = Sys.time(),
                          stringsAsFactors = FALSE)

  } else {

    # Add to current tracker
    tracker <- rbind(tracker, data.frame(timestamp = Sys.time(),
                                         stringsAsFactors = FALSE))

    # Delete records older than 10 seconds
    tracker <- tracker[difftime(Sys.time(), tracker$timestamp) < 10, , drop = FALSE]

  }

  # Return new tracker
  return(tracker)

}
#' Extract pertinent information from table variables metadata
#'
#' Calling \code{\link{scb_list}} with a table ID as argument will return a
#' 2-element list containing metadata; the title of the table, and a list of
#' variables and field values present in the table. This function takes the
#' variable portion of the metadata and extracts the most useful information
#' from it, which is then stored in a data.frame for easy access.
#'
#' @param table_vars From \code{\link{scb_list}} call to table ID (select
#'   $variables)
#' @return data.frame containing variable and value descriptions, as well as
#'   start and end of date range for data
interpret_table_variables <- function(table_vars) {

  # Create output container
  extract <- data.frame(variable_descriptions = NA, value_descriptions = NA,
                        date_range_start = NA, date_range_end = NA, stringsAsFactors = FALSE)
  time_index <- NA

  # Find time component; if none, keep NA
  # Relies on fact that only one variable can be time
  # Replace with listing of years with data (no months)
  if ("time" %in% names(table_vars)) {

    # Store time index
    time_index <- which(table_vars$time)

    # Store time values
    time_values <- table_vars$values[time_index][[1]]

    # Convert based on known formats
    time_values <- lapply(X = time_values, FUN = convert_time_to_year)

    # Store min and max
    time_start <- min(unlist(time_values))
    time_end <- max(unlist(time_values))

  }

  # Find text description of variables, excluding time
  if ("text" %in% names(table_vars)) {

    if (is.na(time_index)) {

      variable_descriptions <- list(table_vars$text)

    } else {

      variable_descriptions <- list(table_vars$text[-time_index])

    }

  }

  # Find text description of variable values, excluding time
  if ("valueTexts" %in% names(table_vars)) {

    if (is.na(time_index)) {

      value_descriptions <- list(table_vars$valueTexts)

    } else {

      value_descriptions <- list(table_vars$valueTexts[-time_index])

    }

  }

  extract$variable_descriptions <- variable_descriptions
  extract$value_descriptions <- value_descriptions
  extract$date_range_start <- time_start
  extract$date_range_end <- time_end

  return(extract)

}
#' Convert string time to numeric year
#'
#' \href{SCB:s}{www.scb.se} database stores time data in a few different
#' formats, many of which has no standard conversion to numeric or time objects.
#' This function uses the known formats to convert time data to a numeric year,
#' to enable easy comparisons. Months and quarters are ignored, as they are not
#' present in all tables.
#'
#' The formats currently supported are listed below: \itemize{\item "2018" for
#' year 2018 \item "2018M01" for year 2018, January \item "2018m01" for year
#' 2018, January \item "2018K1" for year 2018, first quarter \item "2018k1" for
#' year 2018, first quarter \item "2018-2020" for years 2018, 2019, and 2020
#' \item "2018/2019" for years 2018 and 2019 \item "2018H1" for year 2018, first
#' half \item "2018h1" for year 2018, first half \item "2018/19" for years 2018
#' and 2019}
#'
#' @param time_value The time value returned from API query
#' @return Integer value of year, or NA if unable to convert
convert_time_to_year <- function(time_value) {

  if (grepl(pattern = "^\\d{4}$", x = time_value)) {

    # Format is of type 2018 for 2018
    year <- as.numeric(time_value)

  } else if (grepl(pattern = "^\\d{4}M\\d{2}$", x = time_value, ignore.case = TRUE) |
             grepl(pattern = "^\\d{4}K\\d{1}$", x = time_value, ignore.case = TRUE) |
             grepl(pattern = "^\\d{4}H\\d{1}$", x = time_value, ignore.case = TRUE)) {

    # Format is of type 2018M01 for January 2018 or
    # Format is of type 2018K1 for first quarter 2018
    # Format is of type 2018H1 for first half 2018
    year <- as.numeric(stringr::str_match(string = time_value,
                                          pattern = "^\\d{4}"))

  } else if (grepl(pattern = "^\\d{4}-\\d{4}$", x = time_value) |
             grepl(pattern = "^\\d{4}/\\d{4}$", x = time_value)) {

    # Format is a range of years, e.g. 2015-2018 or 2018/2019
    year_start <- as.numeric(stringr::str_match(string = time_value, pattern = "^\\d{4}"))
    year_end <- as.numeric(stringr::str_match(string = time_value, pattern = "\\d{4}$"))
    year <- seq.int(from = year_start, to = year_end, by = 1)

  } else if (grepl(pattern = "^\\d{4}/\\d{2}$", x = time_value)) {
    
    # Format is of type 2018/19
    year_start <- as.numeric(stringr::str_match(string = time_value, pattern = "^\\d{4}"))
    
    # Second year is first two digits of first year + last two digits in
    # time_value string
    year_end <- as.numeric(paste0(stringr::str_match(string = year_start, pattern = "^\\d{2}"), 
                                  stringr::str_match(string = time_value, pattern = "\\d{2}$")))
    year <- seq.int(from = year_start, to = year_end, by = 1)
    
  } else {

    # Unrecognised format - give warning and return NA
    warning(paste0("Unable to convert time format ", time_value, ", will show as NA."))
    year <- NA

  }

  return(year)

}
