#' List available tables / metadata for level
#'
#' By default, will return a list of all highest-level directories, in English,
#' in the ssb database. If additional levels are provided, all subdirectories to
#' the specified path will be shown.
#'
#' The database_id parameter should normally not be touched; at the moment of
#' this writing, only the ssd database can be queried. The levels parameter
#' should be provided as a path, e.g. "AM/AM0101/AM0101A", where each element of
#' the path refers to an ID. The IDs can most easily be determined by
#' sequentially searching through the different levels, starting with the one
#' provided by the default call, where levels = NULL.
#'
#' The function uses the httr package to submit the API request, and jsonlite to
#' parse the response, which is then returned. If an individual table is
#' specified, the returned data will contain metadata for that table, rather
#' than a directory list.
#'
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
#' @param levels Path to search in database; requires database_id to be defined
#' @return A data frame containing the requested directory or a list containing
#'   metadata for the specified table
#' @examples
#' scb_list()
#' scb_list(levels = "AM/AM0101/AM0101A")
#' scb_list(lang = "sv", levels = "LE")
#' @export
scb_list <- function(lang = "en", database_id = "ssd", levels = NULL) {

  # Validate language input
  if (!grepl(pattern = "^en$|^se$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

  # Create request url
  api_url <- paste0("http://api.scb.se/OV0104/v1/doris/", lang, "/", database_id, "/", levels)

  # GET and parse response
  response <- httr::GET(url = api_url)
  parsed <- jsonlite::parse_json(json = response, simplifyVector = TRUE)

  # Return parsed response
  return(parsed)

}
#' Search for directory or table in database
#'
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
#' @param search_term Text to search for in directory and table names
#' @export
scb_search <- function(lang = "en", database_id = "ssd", search_term) {

  # Validate language input
  if (!grepl(pattern = "^en$|^se$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

}
#' Create directory and table list for quick searches
#'
#' Creates a dataset that can be saved in /data and used for quick searches.
#' Needed since the SCB API limits requests to maximum 10 per 10 seconds, which
#' makes ad hoc searching slow.
#'
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
scb_create_cached_data <- function(lang = "en", database_id = "ssd") {

  # Validate language input
  if (!grepl(pattern = "^en$|^se$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

  # Iterate through database using scb_list, adding each database / table name
  # to the dataset as we go, then save to /data

}
