#' List available tables / metadata for level
#'
#' By default, will return a list of all highest-level directories, in English,
#' in the ssb database. If additional ids are provided, all subdirectories to
#' the specified path will be shown.
#'
#' The database_id parameter should normally not be touched; at the moment of
#' this writing, only the ssd database can be queried. The ids parameter
#' should be provided as a path, e.g. "AM/AM0101/AM0101A", where each element of
#' the path refers to an ID. The IDs can most easily be determined by
#' sequentially searching through the different id, starting with the one
#' provided by the default call, where id = NULL.
#'
#' The function uses the httr package to submit the API request, and jsonlite to
#' parse the response, which is then returned. If an individual table is
#' specified, the returned data will contain metadata for that table, rather
#' than a directory list.
#'
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
#' @param id Path to search in database; requires database_id to be defined
#' @return A data frame containing the requested directory, a list containing
#'   metadata for the specified table, or, if status code from GET is not 200,
#'   the status code from the GET call
#' @examples
#' scb_list()
#' scb_list(id = "AM/AM0101/AM0101A")
#' scb_list(lang = "sv", id = "LE")
#' @export
scb_list <- function(lang = "en", database_id = "ssd", id = NULL) {

  # Validate language input
  if (!grepl(pattern = "^en$|^sv$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

  # Create request url
  api_url <- paste0("http://api.scb.se/OV0104/v1/doris/", lang, "/", database_id, "/", id)

  # GET response and check status
  response <- httr::GET(url = api_url)
  status <- response$status
  if (status == 200) {

    # Status good - parse and return response
    parsed <- jsonlite::parse_json(json = response, simplifyVector = TRUE)
    return(parsed)

  } else {

    # Status bad - return status code
    return(paste0("Unexpected status code from GET: ", status))

  }

}
#' Search for directory or table in database
#'
#' Currently only cached search implemented, and thus requires cached_directory,
#' created by scb_create_cache(), as an argument. This will be amended in the
#' future to provide search through uncached directory
#'
#' @param search_term Regex to search for in directory and table names
#' @param cached_directory Created by scb_create_cache
#' @param ignore_case Instruction passed to grepl
#' @param directory_or_table "any", "l" (directory), or "t" (table)
#' @export
scb_search <- function(search_term, cached_directory = NULL,
                       ignore_case = FALSE, directory_or_table = "any") {

  # Check for cached directory
  if (is.null(cached_directory)) {

    load("data/scb_directory_cache.rda")
    cached_directory <- scb_directory_cache

  }

  # Filter according to directory_or_table
  if (directory_or_table == "any") {

    filtered <- cached_directory

  } else {

    filtered <- cached_directory[cached_directory$type == directory_or_table, ]

  }

  # Search
  results <- filtered[grepl(pattern = search_term,
                            x = cached_directory$text,
                            ignore.case = ignore_case), ]

  # Return
  return(results)

}
