# Utility functions - do not export

#' Keep track of API calls
#'
#' Manages list of all API calls made in the last 10 seconds. If no tracker is
#' provided, a new one is returned. If an already existing tracker is passed as
#' an argument to the function, the function will add a new call to it, then
#' remove all items with a timestamp > 10 seconds from current system time.
#'
#' The SCB API limits calls to 10 per 10 seconds (rolling). If this number is
#' exceeded, httr::GET returns a 429 response. We could use Sys.sleep(10) when
#' this occurs to reset the counter, but this would be inefficient: since the
#' limit is rolling, we should only wait until the oldest call is > 10 seconds
#' in the past, and then attempt a new query. This is what this function
#' facilitates.
#'
#' @param tracker Currently tracked calls
#' @return Data frame containing currently tracked calls
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
#' Convert metadata to intelligible data frame
#'
#' Superceded by interpret_table_variables(), which provides more efficient
#' storage of the pertinent data contained in the metadata. Kept here in case a
#' new use for it is discovered, but will likely be removed in a future update.
#'
#' @param id ID of table
#' @param metadata Metadata returned from scb_list() call to table ID
#' @return Data table containing id, name, variables, and values for table
interpet_table_metadata <- function(id, metadata) {

  # Store table title
  table_name <- metadata$title

  # Convert variables to data.table to make unlisting easy
  table_vars <- data.frame(metadata$variables, stringsAsFactors = FALSE)

  # Check existence of fields - add dummy if not
  if (!"code" %in% names(table_vars)) {table_vars$code <- NA}
  if (!"text" %in% names(table_vars)) {table_vars$text <- NA}
  if (!"values" %in% names(table_vars)) {table_vars$values <- NA}
  if (!"valueTexts" %in% names(table_vars)) {table_vars$valueTexts <- NA}
  if (!"elimination" %in% names(table_vars)) {table_vars$elimination <- NA}
  if (!"time" %in% names(table_vars)) {table_vars$time <- NA}

  # Unlist fields
  table_vars <- tidyr::unnest(table_vars)

  # Bind together
  table_joined <- cbind(id, table_name, table_vars)

  # Return
  return(table_joined)

}
#' Extract pertinent information from table metadata
#'
#' Calling scb_list() with a table ID as argument will return a 2-element list
#' containing metadata; the title of the table, and a list of variables and
#' field values present in the table. This function takes the variable portion
#' of the metadata and extracts the most useful information from it, which is
#' then stored in a data.frame for easy access.
#'
#' @param table_vars From scb_list() call to table ID (select $variables)
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
    time_values <- table_vars$values[time_index]

    # Assume ordered list for now
    time_start <- time_values[[1]][1]
    time_end <- time_values[[1]][length(time_values[[1]])]

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
#' Try scb_list() and catch 429 response
#'
#' This is a wrapper for scb_list() to enable it to call repeatedly in case of a
#' 429 (too many requests) response is returned. Since the SCB API has a rolling
#' limit of 10 call per 10 seconds, this situation will frequently occur. In
#' case the response is neither a data.frame or list, the function will throw an
#' error.
#'
#' There is potential to improve this, in combination with scb_list(), by having
#' the latter return the status code along with the data. 
#'
#' @param lang Language
#' @param database_id Database
#' @param id ID to query
#' @param call_tracker Current call tracker
#' @return Valid response from scb_list()
try_scb_list <- function(lang, database_id, id, call_tracker) {

  # Call scb_list: if 429 response, wait for cache to clear then continue
  counter <- 0 # If this reaches 50000, exit: prevents infinite loops
  while (TRUE) {

    call_tracker <- update_call_tracker(call_tracker)
    cur_dir <- scb_list(lang = lang,
                        database_id = database_id,
                        id = id)

    if (!is.data.frame(cur_dir) & !is.list(cur_dir)) {

      counter <- counter + 1
      if (cur_dir == "Unexpected status code from GET: 429") {

        # Wait for call_tracker to clear
        time_to_sleep <- difftime(Sys.time(), call_tracker[which.min(call_tracker$timestamp), ])
        Sys.sleep(time_to_sleep)

      } else {

        stop("Unknown error in scb_list() call in add_directory_to_cache()")

      }

    } else {

      counter <- 0
      break

    }
    
    if (counter > 49999) {stop("Loop failed to exit in try_scb_list()")}

  }

  return(cur_dir)

}
