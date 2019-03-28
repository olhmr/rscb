#' List directory structure or table metadata
#'
#' By default, will return a list of all highest-level directories, in English,
#' in the ssd database. If an ID path is provided, either all subdirectories to
#' the specified path will be shown, or, if the ID path refers to a table, the
#' table metadata.
#'
#' The database_id argument should normally not be touched; "ssd" is the main
#' database for national statistics and, as of March 2019, the only one
#' accessible via the API and this package. To see if any other databases are
#' available, one can set the database_id argument to "": i.e., call
#' scb_list(database_id = ""). The ID is provided as a path, e.g.
#' "AM/AM0101/AM0101A", where each element of the path refers to either a
#' directory, a subdirectory, or a table ID. The IDs can most easily be
#' determined by either sequentially interrogating the database (starting with
#' no ID argument), or by looking through the cached data in scb_cache.
#'
#' The function uses the
#' \href{https://cran.r-project.org/web/packages/httr/index.html}{httr} package
#' to submit the API request, and
#' \href{https://cran.r-project.org/web/packages/jsonlite/index.html}{jsonlite}
#' to parse the response, which it then returns. If the ID path refers to a
#' specific table, the returned data will contain all metadata available for
#' that table, rather than a directory list.
#'
#' @param lang "en" for English, "sv" for Swedish
#' @param database_id Database to search
#' @param id Path to search in database
#' @return A data.frame containing the requested directory, a list containing
#'   metadata for the specified table, or, if status code from GET is not 200,
#'   the status code from the GET call.
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
#' Search for directory or table in database cache
#'
#' Uses data.frame subsetting along with grepl to return all rows in
#' \code{\link{scb_cache}} that matches the provided arguments. Arguments that
#' are skipped are ignored. The function uses data/scb_cache.rda by default, but
#' this can be overriden via the cached_database argument.
#'
#' The subsetting starts with the type argument, to quickly separate databases
#' and tables; this is done as a simple data.frame subsetting operation,
#' matching either "l" or "t". The function then goes through the rest of the
#' arguments, and uses \code{\link[base]{grepl}} to match the string or regex
#' provided in the relevant column.
#'
#' If type is not specified but any search term other than ID is, the function
#' will default to table search as the other search variables are only present
#' in tables. In this case, a warning is provided.
#'
#' This function is subject to change as the caching function is improved.
#'
#' @param search_id Directory or table ID path
#' @param search_type "l" for directory, "t" for table
#' @param search_name Regex for directory or table name
#' @param search_var_desc Rexeg for variable descriptions: table only
#' @param search_val_desc Regex for value descriptions: table only
#' @param search_year Year for which there is data: table only
#' @param cached_database See \code{\link{scb_cache}}
#' @param ignore_case Is regex case sensitive - passed to grepl
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
