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
#' @return A data frame containing the requested directory, a list containing
#'   metadata for the specified table, or, if status code from GET is not 200,
#'   the status code from the GET call
#' @examples
#' scb_list()
#' scb_list(levels = "AM/AM0101/AM0101A")
#' scb_list(lang = "sv", levels = "LE")
#' @export
scb_list <- function(lang = "en", database_id = "ssd", levels = NULL) {

  # Validate language input
  if (!grepl(pattern = "^en$|^sv$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

  # Create request url
  api_url <- paste0("http://api.scb.se/OV0104/v1/doris/", lang, "/", database_id, "/", levels)

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
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
#' @param search_term Text to search for in directory and table names
#' @export
scb_search <- function(lang = "en", database_id = "ssd", search_term) {

  # Validate language input
  if (!grepl(pattern = "^en$|^sv$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

}
#' Create directory and table list for quick searches
#'
#' Creates a dataset that can be saved in /data and used for quick searches.
#' Needed since API requests make searching slow, and the SCB API limits
#' requests to maximum 10 per 10 seconds, which making it even slower.
#'
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
#' @export
scb_create_cache <- function(lang = "en", database_id = "ssd") {

  # Validate language input
  if (!grepl(pattern = "^en$|^sv$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

  # Set up container
  cache <- data.frame(id = character(), level = numeric(), text = character(), stringsAsFactors = FALSE)

  # Initial list is highest directory in database
  cur_dir_list <- scb_list()
  cache <- rbind(cache, data.frame(id = cur_dir_list$id,
                                   level = 1,
                                   text = cur_dir_list$text, stringsAsFactors = FALSE))

  # Iterate through database using scb_list, adding each database / table name
  # to the dataset as we go, then save to /data This should be redone as a
  # recursive function, and should add better control over how many requests are
  # submitted in each 10 second window to maximise performance, as well as
  # checking for levels dynamically (not all paths have the same number of
  # levels)
  for (cur_id_1 in cache[cache$level == 1, ]$id) {

    cur_dir_list <- scb_list(lang = lang,
                             database_id = database_id,
                             levels = cur_id_1)
    cache <- rbind(cache, data.frame(id = paste0(cur_id_1, "/", cur_dir_list$id),
                                     level = 2,
                                     text = cur_dir_list$text, stringsAsFactors = FALSE))

    for (cur_id_2 in cache[cache$level == 2, ]$id) {

      cur_dir_list <- scb_list(lang = lang,
                               database_id = database_id,
                               levels = paste0(cur_id_1, "/", cur_id_2))
      if (!is.data.frame(cur_dir_list)) {

        if (cur_dir_list == "Unexpected status code from GET: 429") {

          # Too many requests (SCB limits to 10 per 10 seconds)
          # Wait and retry
          Sys.sleep(10)
          cur_dir_list <- scb_list(lang = lang,
                                   database_id = database_id,
                                   levels = paste0(cur_id_1, "/", cur_id_2))

        } else {

          stop(cur_dir_list)

        }

      }
      cache <- rbind(cache, data.frame(id = paste0(cur_id_1, "/", cur_id_2, "/", cur_dir_list$id),
                                       level = 3,
                                       text = cur_dir_list$text, stringsAsFactors = FALSE))

    }

  }

  return(cache)

}
