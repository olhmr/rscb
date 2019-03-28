#' Create cache of database
#'
#' Creates a local and condensed copy of the given database at \url{www.scb.se},
#' for the given language, with the option of starting at a particular ID path
#' to only cache a subset of the data.
#'
#' The function is only tested with the "ssd" database, as that is the only one
#' that can be queried through this package and the API as of March 2019. The
#' output is condensed to reduce file size - the full structure can be seen in
#' \code{\link{scb_cache}} or by loading "data/scb_cache.rda".
#'
#' The function recursively calls \code{\link{scb_list}} through
#' \code{\link[rscb]{add_directory_to_cache}}. For each directory, the ID path
#' and text description is stored. For tables, in addition to the ID path and
#' text description, the variables, values, and date range present in the table
#' is also stored.
#'
#' @param lang Supported languages: "en" English
#' @param database_id Supported databases: "ssd"
#' @param initial_id From where to start caching: default top level
#' @return Data.frame containing id, depth, type, name, variable and value
#'   descriptions, and date range for each directory, subdirectory, and table in
#'   the database
#' @examples
#' \dontrun{
#' scb_cache <- scb_create_cache(lang = "en")
#' scb_AM0101_cache <- scb_create_cache(lang = "en", initial_id = "AM/AM0101")
#' }
#' @export
scb_create_cache <- function(lang = "en", database_id = "ssd", initial_id = "") {

  # Set up container
  cache <- data.frame(id = character(),
                      depth = numeric(),
                      type = character(),
                      name = character(),
                      var_desc = character(),
                      val_desc = character(),
                      date_start = character(),
                      date_end = character(),
                      stringsAsFactors = FALSE)

  # Initialise
  call_tracker <- update_call_tracker()
  cur_dir <- try_scb_list(lang = lang, database_id = database_id,
                          id = initial_id, call_tracker = call_tracker)
  cache <- rbind(cache, data.frame(id = cur_dir$id, depth = 1, type = cur_dir$type,
                                   name = cur_dir$text, var_desc = NA, val_desc = NA,
                                   date_start = NA, date_end = NA,
                                   stringsAsFactors = FALSE))

  # Loop through top level directory specified by initial_id
  # If subdirectory, call recursive caching function on it
  # If table, store variables
  for (i in 1:nrow(cur_dir)) {

    if (cur_dir[i, ]$type == "l") {

      cache <- add_directory_to_cache(cache = cache, lang = lang, database_id = database_id,
                                      id = paste0(initial_id, "/", cur_dir[i, ]$id),
                                      depth = 1, call_tracker = call_tracker)

    } else if (cur_dir[i, ]$type == "t") {

      # Get variables
      vars <- try_scb_list(lang = lang, database_id = database_id,
                       id = paste0(initial_id, "/", cur_dir[i, ]$id),
                       call_tracker = call_tracker)$variables

      # Store in cache
      vars_interpreted <- interpret_table_variables(vars)
      cache[cache$id == cur_dir[i, ]$id, ]$var_desc <- vars_interpreted$variable_descriptions
      cache[cache$id == cur_dir[i, ]$id, ]$val_desc <- vars_interpreted$value_descriptions
      cache[cache$id == cur_dir[i, ]$id, ]$date_start <- vars_interpreted$date_range_start
      cache[cache$id == cur_dir[i, ]$id, ]$date_end <- vars_interpreted$date_end_start

    }

  }

  # Return
  return(cache)

}
#' Add \code{\link{scb_list}} call to cache
#'
#' This function makes up the main body of \code{\link{scb_create_cache}}. It works by
#' recursively calling \code{\link{scb_list}}, each time adding the directory list and any
#' table metadata as appropriate.
#'
#' For more information, see \code{\link{scb_create_cache}}.
#'
#' @param cache Current cache
#' @param lang Language: from \code{\link{scb_create_cache}}
#' @param database_id Database to search: from \code{\link{scb_create_cache}}
#' @param id Path for querying with \code{\link{scb_list}}
#' @param depth Current depth: 1 = top level directory
#' @param call_tracker Current call_tracker instance
#' @return data.frame showing ID, depth, type, and name of each directory,
#'   subdirectory, and table, with tables also including information on
#'   variables contained
add_directory_to_cache <- function(cache, lang, database_id, id, depth, call_tracker) {

  # Call scb_list: if 429 response, wait for cache to clear then continue
  cur_dir <- try_scb_list(lang = lang, database_id = database_id,
                          id = id, call_tracker = call_tracker)

  # Create dummy cache to bind
  tmp_cache <- data.frame(id = paste0(id, "/", cur_dir$id),
                          depth = depth + 1, type = cur_dir$type,
                          name = cur_dir$text, var_desc = NA,
                          val_desc = NA, date_start = NA,
                          date_end = NA, stringsAsFactors = FALSE)

  # Add to cache
  cache <- rbind(cache, tmp_cache)

  # Call add_directory_to_cache() on all subdirectories, and add variable data
  # for all tables
  for (i in 1:nrow(tmp_cache)) {

    if (tmp_cache[i, ]$type == "l") {

      cache <- add_directory_to_cache(cache = cache, lang = lang, database_id = database_id,
                                      id = tmp_cache[i, ]$id,
                                      depth = depth, call_tracker = call_tracker)

    } else if (tmp_cache[i, ]$type == "t") {

      # Get variables
      vars <- try_scb_list(lang = lang, database_id = database_id,
                           id = tmp_cache[i, ]$id, call_tracker = call_tracker)$variables

      # Store in cache
      vars_interpreted <- interpret_table_variables(vars)
      cache[cache$id == tmp_cache[i, ]$id, ]$var_desc <- vars_interpreted$variable_descriptions
      cache[cache$id == tmp_cache[i, ]$id, ]$val_desc <- vars_interpreted$value_descriptions
      cache[cache$id == tmp_cache[i, ]$id, ]$date_start <- vars_interpreted$date_range_start
      cache[cache$id == tmp_cache[i, ]$id, ]$date_end <- vars_interpreted$date_range_end

    } else {

      stop("Unknown type in add_directory_to_cache()")

    }

  }

  return(cache)

}
