#' Cached view of ssd directories
#'
#' Data frame containing information on all directories, subdirectories, and
#' tables in the ssd database (the main database for national statistics, and
#' currently the only one accessible via API).
#'
#' @format A data frame with x rows and 10 columns:
#' \describe{
#'   \item{id}{ID to use to query directory or table}
#'   \item{depth}{1 = highest level directory, incremented for each subdirectory}
#'   \item{type}{l for directory, t for table}
#'   \item{name}{Name of directory / table}
#'   \item{var_desc}{Variables in table}
#'   \item{val_desc}{Values in table}
#'   \item{date_start}{First time entry in table}
#'   \item{date_end}{Last time entry in table}
#' }
#' @source rscb::scb_create_directory_cache()
"scb_cache"
