#' Cached view of ssd directories
#'
#' Data frame containing information on all directories, subdirectories, and
#' tables in the ssd database (the main database for national statistics, and
#' currently the only one accessible via API).
#' 
#' @format A data frame with ca 2797 rows and 4 columns:
#' \describe{
#'   \item{id}{id to use to query directory or table}
#'   \item{type}{l for directory, t for table},
#'   \item{depth}{1 = highest level directory, incremented for each subdirectory}
#'   \item{text}{Information on database / table}
#' }
#' @source rscb::scb_create_cache()
"scb_directory_cache"