% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scb_query.R
\name{scb_query}
\alias{scb_query}
\title{Query a table in the SCB Database}
\usage{
scb_query(table_id, ..., lang = "en", database_id = "ssd")
}
\arguments{
\item{table_id}{ID path of table to query}

\item{...}{Arguments to query table with, each like: list(code =
code_to_query, filter = filter_type, values = c(values_to_filter))}

\item{lang}{Language "en" English or "sv" Swedish}

\item{database_id}{ID of database; currently only "ssd"}
}
\value{
list containing status code and data.frame with response from
  \code{\link[httr]{POST}} query
}
\description{
This is essentially a wrapper for \code{\link[httr]{POST}}, with some more
intuitive and user-friendly handling of arguments and output. The ...
arguments should be named lists of the form list(code =
variable_code_to_filter_on, filter = type_of_filter_to_use, values =
values_to_filter_on). These are converted to JSON with the
\href{https://cran.r-project.org/web/packages/jsonlite/index.html}{jsonlite}
package, and submitted as the body in \code{\link[httr]{POST}}.
}
\details{
The types of filter that can be used are: \itemize{ \item item - filter to
values matching those provided \item all - wildcard selection, e.g. \* for
all values, 01\* for all values starting with 01 \item top - filter to the
top x records, where x is provided in values \item agg - aggregate values
\item vs - comparison versus other variable }

For more information, see \url{https://www.scb.se/api/}.
}
\examples{
\dontrun{
scb_query(
  table_id = "AM/AM0101/AM0101A/LonArb07Privat",
  list(code = "Overtidstillagg", filter = "item", values = "10"),
  list(code = "Tid", filter = "top", values = "5"),
  list(code = "SNI2007", filter = "item", values = c("B", "C")))
}
}
