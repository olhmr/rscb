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
#' @return data.frame showing ID, type, depth, and text description of each
#'   directory, subdirectory, and table in the database, to be saved in /data
#' @export
scb_create_cache <- function(lang = "en", database_id = "ssd") {

  # Validate language input
  if (!grepl(pattern = "^en$|^sv$", x = lang)) {
    stop("The lang parameter must be either \"en\" (English) or \"sv\" (Swedish)")
  }

  # Set up container
  cache <- data.frame(id = character(), level = numeric(), text = character(), stringsAsFactors = FALSE)

  # Initial list is highest directory in database
  call_tracker <- update_call_tracker()
  cur_dir_list <- scb_list()
  cache <- rbind(cache, data.frame(id = cur_dir_list$id,
                                   type = cur_dir_list$type,
                                   depth = 1,
                                   text = cur_dir_list$text, stringsAsFactors = FALSE))

  # Recursively iterate through directories and subdirectories For each item in
  # cur_dir_list, check type: if l, then it is a directory and we call the
  # add_to_cache function on it (and then repeat the procedure for any
  # subdirectories, etc.). If t, then it is a table and we skip to the next in
  # the list.
  for (i in 1:dim(cur_dir_list)[1]) {

    call_tracker <- update_call_tracker(call_tracker)
    cache <- add_to_cache(cache, lang, database_id,
                          levels = cur_dir_list[i, ]$id,
                          depth = 1, call_tracker)

  }

  return(cache)

}
#' Add scb_list call to cache
#'
#' Given a list of highest level directories from scb_create_cache(), the
#' function goes through each directory and subdirectory in turn, cataloging
#' each level and storing it in memory, which is then returned.
#'
#' @param cache Current cache
#' @param lang Language: should be inherited
#' @param database_id Database to search: should be inherited
#' @param levels Path for querying with scb_list()
#' @param depth How deep in the subdirectories we are
#' @param call_tracker Current call_tracker instance: should be created in
#'   scb_create_cache()
#' @return data.frame showing ID, type, depth, and text description of each
#'   directory, subdirectory, and table in the database
add_to_cache <- function(cache, lang, database_id, levels, depth, call_tracker) {

  # Call scb_list: if 429 response, wait for cache to clear then continue
  while (TRUE) {

    call_tracker <- update_call_tracker(call_tracker)
    cur_dir_list <- scb_list(lang = lang,
                             database_id = database_id,
                             levels = levels)

    if (!is.data.frame(cur_dir_list)) {

      if (cur_dir_list == "Unexpected status code from GET: 429") {

        # Wait for call_tracker to clear
        Sys.sleep(difftime(Sys.time(), call_tracker[which.max(call_tracker$timestamp), ]))

      } else {

        stop("Unknown error in scb_list() call in add_to_cache()")

      }

    } else {

      break

    }

  }

  # Add to cache
  cache <- rbind(cache, data.frame(id = paste0(levels, "/", cur_dir_list$id),
                                   depth = depth + 1,
                                   type = cur_dir_list$type,
                                   text = cur_dir_list$text,
                                   stringsAsFactors = FALSE))

  # Call add_to_cache() on all subdirectories
  for (i in 1:dim(cur_dir_list)[1]) {

    if (cur_dir_list[i, ]$type == "l") {

      cache <- add_to_cache(cache, lang, database_id,
                            levels = paste0(levels, "/", cur_dir_list[i, ]$id),
                            depth, call_tracker)

    }

  }

  return(cache)

}
