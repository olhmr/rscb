#' Cached view of ssd directories
#'
#' Data frame containing information on all directories, subdirectories, and
#' tables in the ssd database (the main database for national statistics, and
#' currently the only one accessible via API).
#'
#' @format A data frame with x rows and 10 columns:
#' \describe{
#'   \item{id}{id to use to query directory or table}
#'   \item{depth}{1 = highest level directory, incremented for each subdirectory}
#'   \item{type}{l for directory, t for table},
#'   \item{name}{Information on database / table}
#'   \item{var_codes}{codes in table}
#'   \item{var_names}{names of codes in table}
#'   \item{var_values}{values in table}
#'   \item{var_value_names}{names of values in table}
#'   \item{var_elims}{indicates whether variable can be elimated}
#'   \item{var_times}{indicates whether variable is a valid time dimension}
#' }
#' @source rscb::scb_create_directory_cache()
"scb_cache"
