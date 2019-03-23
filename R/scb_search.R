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
#' @param search_id Directory / table ID path
#' @param search_type Directory "l" or table "t"
#' @param search_name Text in directory or table name
#' @param search_var_desc Text in variable descriptions: table only
#' @param search_val_desc Text in value descriptions: table only
#' @param search_year Year for which there is data: table only
#' @param cached_database Created by scb_create_cache
#' @param ignore_case Instruction passed to grepl
#' @export
scb_search <- function(search_id = NULL, search_type = NULL, search_name = NULL,
                       search_var_desc = NULL, search_val_desc = NULL,
                       search_year = NULL,
                       cached_database = NULL,
                       ignore_case = FALSE) {

  # Warn for illogical arguments
  if (!is.null(search_type)) {

    if (search_type != "t" & (!is.null(search_var_desc) | !is.null(search_val_desc)
                              | !is.null(search_year))) {

      warning("Only tables will be returned when variables, values, and years are searched for.")
      search_type <- "t"

    }

  } else if (!is.null(search_var_desc) | !is.null(search_val_desc)
             | !is.null(search_year)) {

    warning("Only tables will be returned when variables, values, and years are searched for.")
    search_type <- "t"

  }


  # Check for cached directory
  if (is.null(cached_database)) {

    load("data/scb_cache.rda")
    cached_database <- scb_cache

  }

  # Filter according to search terms; start with type
  output <- cached_database
  if (!is.null(search_type)) {output <- output[output$type == search_type, ]}
  if (!is.null(search_id)) {output <- output[grepl(pattern = search_id, x = output$id, ignore.case = ignore_case), ]}
  if (!is.null(search_name)) {output <- output[grepl(pattern = search_name, x = output$name, ignore.case = ignore_case), ]}
  if (!is.null(search_var_desc)) {output <- output[grepl(pattern = search_var_desc, x = output$var_desc, ignore.case = ignore_case), ]}
  if (!is.null(search_val_desc)) {ouput <- output[grepl(pattern = search_val_desc, x = output$val_desc, ignore.case = ignore_case), ]}
  # Make special handling of years

  # Return
  return(output)

}
