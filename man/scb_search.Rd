% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scb_search.R
\name{scb_search}
\alias{scb_search}
\title{Search for directory or table in database cache}
\usage{
scb_search(search_id = NULL, search_type = NULL, search_name = NULL,
  search_var_desc = NULL, search_val_desc = NULL, search_year = NULL,
  cached_database = NULL, ignore_case = FALSE)
}
\arguments{
\item{search_id}{Directory or table ID path}

\item{search_type}{"l" for directory, "t" for table}

\item{search_name}{Regex for directory or table name}

\item{search_var_desc}{Rexeg for variable descriptions: table only}

\item{search_val_desc}{Regex for value descriptions: table only}

\item{search_year}{Year for which there is data: table only}

\item{cached_database}{See \code{\link{scb_cache}}}

\item{ignore_case}{Is regex case sensitive - passed to grepl}
}
\description{
Uses data.frame subsetting along with grepl to return all rows in
\code{\link{scb_cache}} that matches the provided arguments. Arguments that
are skipped are ignored. The function uses data/scb_cache.rda by default, but
this can be overriden via the cached_database argument.
}
\details{
The subsetting starts with the type argument, to quickly separate databases
and tables; this is done as a simple data.frame subsetting operation,
matching either "l" or "t". The function then goes through the rest of the
arguments, and uses \code{\link[base]{grepl}} to match the string or regex
provided in the relevant column.

If type is not specified but any search term other than ID is, the function
will default to table search as the other search variables are only present
in tables. In this case, a warning is provided.

This function is subject to change as the caching function is improved.
}
\examples{
\dontrun{
scb_search(search_id = "AM/AM0101")
scb_search(search_type = "t", search_name = "LFS", ignore_case = TRUE)
scb_search(search_type = "t", search_var_desc = "overtime")
}
}
