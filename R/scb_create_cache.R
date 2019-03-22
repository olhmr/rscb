# Functions to cache SCB database

#' New function for caching
#'
#' Caches both directories and tables into one, and does so more efficiently.
#' @param lang Supported languages: "en" English
#' @param database_id Supported databases: "ssd"
#' @param initial_id From where to start caching: default top level
#' @return Cached database containing directory structure and table metadata
#' @export
scb_create_cache <- function(lang = "en", database_id = "ssd", initial_id = "") {

  # Set up container
  cache <- data.frame(id = character(),
                      depth = numeric(),
                      type = character(),
                      name = character(),
                      var_codes = list(),
                      var_names = list(),
                      var_values = list(),
                      var_value_names = list(),
                      var_elims = list(),
                      var_times = list(),
                      stringsAsFactors = FALSE)

  # Initialise
  call_tracker <- update_call_tracker()
  cur_dir <- scb_list(lang = lang, database_id = database_id, id = initial_id)
  cache <- rbind(cache, data.frame(id = cur_dir$id, depth = 1, type = cur_dir$type,
                                   name = cur_dir$text, var_codes = NA, var_names = NA,
                                   var_values = NA, var_value_names = NA,
                                   var_elims = NA, var_times = NA,
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
      vars <- scb_list(lang = lang, database_id = database_id,
                       id = paste0(initial_id, "/", cur_dir[i, ]$id))$variables

      # Store in cache
      # Must be a more elegant way to do this
      if ("code" %in% names(vars)) {cache[cache$id == cur_dir[i, ]$id, ]$var_codes <- list(vars$code)}
      if ("text" %in% names(vars)) {cache[cache$id == cur_dir[i, ]$id, ]$var_names <- list(vars$text)}
      if ("values" %in% names(vars)) {cache[cache$id == cur_dir[i, ]$id, ]$var_values <- list(vars$values)}
      if ("valueTexts" %in% names(vars)) {cache[cache$id == cur_dir[i, ]$id, ]$var_value_names <- list(vars$valueTexts)}
      if ("elimination" %in% names(vars)) {cache[cache$id == cur_dir[i, ]$id, ]$var_elims <- list(vars$elimination)}
      if ("time" %in% names(vars)) {cache[cache$id == cur_dir[i, ]$id, ]$var_times <- list(vars$time)}

    }

  }

  # Return
  return(cache)

}
#' Add scb_list() call to cache
#'
#' @param cache Current cache
#' @param lang Language: should be inherited
#' @param database_id Database to search: should be inherited
#' @param id Path for querying with scb_list()
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
                          name = cur_dir$text, var_codes = NA,
                          var_names = NA, var_values = NA,
                          var_value_names = NA, var_elims = NA,
                          var_times = NA, stringsAsFactors = FALSE)

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
      # Must be a more elegant way to do this
      if ("code" %in% names(vars)) {cache[cache$id == tmp_cache[i, ]$id, ]$var_codes <- list(vars$code)}
      if ("text" %in% names(vars)) {cache[cache$id == tmp_cache[i, ]$id, ]$var_names <- list(vars$text)}
      if ("values" %in% names(vars)) {cache[cache$id == tmp_cache[i, ]$id, ]$var_values <- list(vars$values)}
      if ("valueTexts" %in% names(vars)) {cache[cache$id == tmp_cache[i, ]$id, ]$var_value_names <- list(vars$valueTexts)}
      if ("elimination" %in% names(vars)) {cache[cache$id == tmp_cache[i, ]$id, ]$var_elims <- list(vars$elimination)}
      if ("time" %in% names(vars)) {cache[cache$id == tmp_cache[i, ]$id, ]$var_times <- list(vars$time)}

    } else {

      stop("Unknown type in add_directory_to_cache()")

    }

  }

  return(cache)

}
