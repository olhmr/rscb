# Functions to cache SCB database

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

  scb_directory_cache <- scb_create_directory_cache(lang = lang, database_id = database_id)
  scb_table_cache <- scb_create_table_cache(lang = lang, database_id = database_id,
                                            cached_directory = scb_directory_cache)

}
#' Map directory and store to flat data frame
#'
#' Map out the directory structure in the given database for the given language.
#' This can then be saved in /data and used for quick directory searches. This
#' can then be used as input in table caching function.
#'
#' @param lang "en" English or "sv" Swedish
#' @param database_id Database to search
#' @return data.frame showing ID, type, depth, and text description of each
#'   directory, subdirectory, and table in the database, to be saved in /data
#' @export
scb_create_directory_cache <- function(lang = "en", database_id = "ssd") {

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
  # add_directory_to_cache function on it (and then repeat the procedure for any
  # subdirectories, etc.). If t, then it is a table and we skip to the next in
  # the list.
  for (i in 1:dim(cur_dir_list)[1]) {

    call_tracker <- update_call_tracker(call_tracker)
    cache <- add_directory_to_cache(cache, lang, database_id,
                          id = cur_dir_list[i, ]$id,
                          depth = 1, call_tracker)

  }

  return(cache)

}
#' Store metadata on tables
#'
#' @param lang "en" (English) or "sv" (Swedish)
#' @param database_id "ssd" for standard database
#' @param cached_directory Created by scb_create_directory_cache: leave blank to load from /data
#' @return data.table showing ID, name, and metadata for each table in database
#' @export
scb_create_table_cache <- function(lang = "en", database_id = "ssd",
                                   cached_directory = NULL) {

  # If no cache provided, use default saved to /data
  if (is.null(cached_directory)) {

    load("data/scb_directory_cache.rda")
    cached_directory <- scb_directory_cache

  }

  # Set up container
  cache <- data.frame(id = character(), table_name = character(), code = character(),
                      text = character(), values = character(), valueTexts = character(),
                      elimination = logical(), time = logical())

  # Initialise call_tracker
  call_tracker <- update_call_tracker() # Slightly inefficient

  # Loop through directory
  progress_bar <- utils::txtProgressBar(min = 0, max = nrow(cached_directory), initial = 0)
  for (i in 1:nrow(cached_directory)) {

    if (cached_directory[i, ]$type == "t") {

      # Call scb_list: if 429 response, wait for cache to clear then continue
      while (TRUE) {

        call_tracker <- update_call_tracker(call_tracker)
        cur_metadata <- scb_list(lang = lang,
                                 database_id = database_id,
                                 id = cached_directory[i, ]$id)

        if (!is.list(cur_metadata)) {

          if (cur_metadata == "Unexpected status code from GET: 429") {

            # Wait for call_tracker to clear
            time_to_sleep <- difftime(Sys.time(), call_tracker[which.min(call_tracker$timestamp), ])
            Sys.sleep(time_to_sleep)

          } else if (cur_metadata == "Unexpected status code from GET: 404") {

            # Table not found, probably because of out of date directory
            # Create fake metadata file with NAs, then break
            cur_metadata <- data.frame(id = cached_directory[i, ]$id,
                                       table_name = cached_directory[i, ]$text,
                                       code = NA, text = NA, values = NA, valueTexts = NA,
                                       elimination = NA, time = NA)
            break

          } else {

            stop("Unknown error in scb_list() call in add_directory_to_cache()")

          }

        } else {

          # Interpet and exit
          cur_metadata <- interpet_table_metadata(id = cached_directory[i, ]$id,
                                                  metadata = cur_metadata)
          break

        }

      }

      utils::setTxtProgressBar(progress_bar, i)
      cache <- rbind(cache, cur_metadata)

    }

  }

  return(cache)

}
#' Add scb_list call to directory cache
#'
#' Given a list of highest level directories from scb_create_cache(), the
#' function goes through each directory and subdirectory in turn, cataloging
#' each level and storing it in memory, which is then returned.
#'
#' @param cache Current cache
#' @param lang Language: should be inherited
#' @param database_id Database to search: should be inherited
#' @param id Path for querying with scb_list()
#' @param depth How deep in the subdirectories we are
#' @param call_tracker Current call_tracker instance: should be created in
#'   scb_create_cache()
#' @return data.frame showing ID, type, depth, and text description of each
#'   directory, subdirectory, and table in the database
add_directory_to_cache <- function(cache, lang, database_id, id, depth, call_tracker) {

  # Call scb_list: if 429 response, wait for cache to clear then continue
  while (TRUE) {

    call_tracker <- update_call_tracker(call_tracker)
    cur_dir_list <- scb_list(lang = lang,
                             database_id = database_id,
                             id = id)

    if (!is.data.frame(cur_dir_list)) {

      if (cur_dir_list == "Unexpected status code from GET: 429") {

        # Wait for call_tracker to clear
        time_to_sleep <- difftime(Sys.time(), call_tracker[which.min(call_tracker$timestamp), ])
        Sys.sleep(time_to_sleep)

      } else {

        stop("Unknown error in scb_list() call in add_directory_to_cache()")

      }

    } else {

      break

    }

  }

  # Add to cache
  cache <- rbind(cache, data.frame(id = paste0(id, "/", cur_dir_list$id),
                                   depth = depth + 1,
                                   type = cur_dir_list$type,
                                   text = cur_dir_list$text,
                                   stringsAsFactors = FALSE))

  # Call add_directory_to_cache() on all subdirectories
  for (i in 1:dim(cur_dir_list)[1]) {

    if (cur_dir_list[i, ]$type == "l") {

      cache <- add_directory_to_cache(cache, lang, database_id,
                            id = paste0(id, "/", cur_dir_list[i, ]$id),
                            depth, call_tracker)

    }

  }

  return(cache)

}
