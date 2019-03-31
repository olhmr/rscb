#' Query a table in the SCB Database
#'
#' This is essentially a wrapper for \code{\link[httr]{POST}}, with some more
#' intuitive and user-friendly handling of arguments and output. The ...
#' arguments should be named lists of the form list(code =
#' variable_code_to_filter_on, filter = type_of_filter_to_use, values =
#' values_to_filter_on). These are converted to JSON with the
#' \href{https://cran.r-project.org/web/packages/jsonlite/index.html}{jsonlite}
#' package, and submitted as the body in \code{\link[httr]{POST}}.
#'
#' The types of filter that can be used are: \itemize{ \item item - filter to
#' values matching those provided \item all - wildcard selection, e.g. \* for
#' all values, 01\* for all values starting with 01 \item top - filter to the
#' top x records, where x is provided in values \item agg - aggregate values
#' \item vs - comparison versus other variable }
#'
#' For more information, see \url{https://www.scb.se/api/}.
#'
#' @param table_id ID path of table to query
#' @param lang Language "en" English or "sv" Swedish
#' @param database_id ID of database; currently only "ssd"
#' @param ... Arguments to query table with, each like: list(code =
#'   code_to_query, filter = filter_type, values = c(values_to_filter))
#' @return data.frame containing response from \code{\link[httr]{POST}} query
#' @examples
#' \dontrun{
#' scb_query(
#'   table_id = "AM/AM0101/AM0101A/LonArb07Privat",
#'   list(code = "Overtidstillagg", filter = "item", values = "10"),
#'   list(code = "Tid", filter = "top", values = "5"),
#'   list(code = "SNI2007", filter = "item", values = c("B", "C")))
#' }
#' @export
scb_query <- function(table_id, ..., lang = "en", database_id = "ssd") {

  # Create request url
  api_url <- paste0("http://api.scb.se/OV0104/v1/doris/", lang, "/", database_id, "/", table_id)

  # Unpack arguments list
  args <- list(...)

  # Construct containers
  main_frame <- data.frame()
  sel_frame <- data.frame()

  # Iterate through argument list
  if (length(args) > 0) {

    for (i in 1:length(args)) {

      # Main data.frame
      main_frame <- rbind(main_frame, data.frame(code = args[[i]]$code, stringsAsFactors = FALSE))

      # Selection data.frame
      tmp_sel <- data.frame(filter = args[[i]]$filter, stringsAsFactors = FALSE)
      tmp_sel$values <- list(args[[i]]$values)
      sel_frame <- rbind(sel_frame, tmp_sel)

    }

  }

  # Join up
  main_frame$selection <- sel_frame

  # Add response
  response <- list(format = "csv")
  query_list <- list(query = main_frame, response = response)

  # Convert to JSON
  query_json <- jsonlite::toJSON(query_list)

  # Correct response; API does not recognise square brackets in response format
  query_json <- stringr::str_replace(string = query_json,
                                     pattern = "\"response\":\\{\"format\":\\[\"csv\"\\]\\}",
                                     replacement = "\"response\":\\{\"format\":\"csv\"\\}")

  # Send query
  response <- httr::POST(url = api_url, body = query_json)

  # Check and format response
  if (response$status_code == 200) {

    output <- httr::content(response)

  } else {

    output <- paste0("Unexpected status code from POST: ", response$status_code)

  }

  # Return results
  return(output)

}




