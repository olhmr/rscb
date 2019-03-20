# Utility functions - do not export

#' Keep track of API calls
#'
#' Manages list of all API calls made in the last 10 seconds. If no tracker is
#' provided, a new one is returned. If an already existing tracker is passed as
#' an arugment to the function, the function will add a new call to it, then
#' remove all items with a timestamp > 10 seconds from current time.
#'
#' The SCB API limits calls to 10 per 10 seconds (rolling). If this number is
#' exceeded, httr::GET returns a 429 response. We could use Sys.sleep(10) when
#' this occurs to reset the counter, but this would be inefficient: since the
#' limit is rolling, we should only wait until the oldest call is > 10 seconds
#' in the past, and then attempt a new query. This is what this function
#' facilitates.
#'
#' Ideally, we would make this function less general (such as making updating
#' the tracked call list optional), and handle the validation step outside of
#' the function, but this will do for now.
#'
#' @param tracker The current tracking data
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
