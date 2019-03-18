# Utility functions - do not export

#' Keep track of API calls
#'
#' Manages SCB limit of 10 calls per 10 seconds to ensure smooth running of
#' functions reliant on API calls.
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
