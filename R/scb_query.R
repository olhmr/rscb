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

  # Construct data frame
  body_from_args <- data.frame(code = character(),
                               selection.filter = character(),
                               selection.values = character(),
                               stringsAsFactors = FALSE)
  for (i in 1:length(args)) {

    tmp_frame <- data.frame(code = args[[i]]$code, selection.filter = args[[i]]$filter,
                            stringsAsFactors = FALSE)
    tmp_frame$selection.values <- list(args[[i]]$values) # Needed to ensure stored as vector
    body_from_args <- rbind(body_from_args, tmp_frame)

  }

  # Add response
  response <- list(format = "json")
  query_list <- list(query = body_from_args, response = response)

  # Convert to JSON
  query_json <- jsonlite::toJSON(query_list)

  # Correct response; API does not recognise square brackets in response format
  query_json <- stringr::str_replace(string = query_json,
                                     pattern = "\"response\":\\{\"format\":\\[\"json\"\\]\\}",
                                     replacement = "\"response\":\\{\"format\":\"json\"\\}")

  # Send query
  response <- httr::POST(url = api_url, body = query_json)

  # Check and format response

  # Return results


  # Create query string
  # Get arguments from user
  # Push to list / data frame structure (see from)
  # Correct response with str_replace from stringr
  # Send to server
  # Dummy
  # query_string <- "{ \"query\": [{\"code\":\"Fodelseland\", \"selection\":{\"filter\":\"item\", \"values\":[\"010\",\"020\"]}},
  #                              {\"code\":\"Alder\", \"selection\":{\"filter\":\"all\", \"values\":[\"*\"]}},
  #                              {\"code\":\"Tid\", \"selection\":{ \"filter\":\"top\", \"values\":[\"3\"]}}],
  #   \"response\": {\"format\":\"csv\"} }"

  # "{ \"query\": [{\"code\":\"Fodelseland\", \"selection\":{\"filter\":\"item\", \"values\":[\"010\",\"020\"]}},
  #          {\"code\":\"Tid\", \"selection\":{ \"filter\":\"top\", \"values\":[\"3\"]}}],
  # \"response\": {\"format\":\"csv\"} }"

  # Replace response as incorrectly formatted when using jsonlite::toJSON
  # str_replace(to, "\"response\":\\{\"format\":\\[\"csv\"\\]\\}", "\"response\":\\{\"format\":\"csv\"\\}")

  # Send query
  # response <- httr::POST(url = api_url, body = query_string)

  # Format response

  # Return
  return(response)

}




