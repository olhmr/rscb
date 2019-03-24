#' Query a table
#'
#' @param table_id ID path of table to query
#' @param lang Language "en" English or "sv" Swedish
#' @param database_id ID of database; currently only "ssd"
#' @param ... Arguments to query table with, each like: list(code = code_to_query, filter = filter_type, values = c(values_to_filter))
#' @return data.frame containing response from POST query
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
  for (i in 1:length(args)) {

    # Main data.frame
    main_frame <- rbind(main_frame, data.frame(code = args[[i]]$code, stringsAsFactors = FALSE))

    # Selection data.frame
    tmp_sel <- data.frame(filter = args[[i]]$filter, stringsAsFactors = FALSE)
    tmp_sel$values <- list(args[[i]]$values)
    sel_frame <- rbind(sel_frame, tmp_sel)

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

  # Return results
  return(response)

}




