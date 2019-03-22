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
#' @source rscb::scb_create_directory_cache()
"scb_directory_cache"
#' Cached view of ssd tables
#'
#' Data frame containing metadata for all tables in ssd database (the main
#' database for national statistics, and currently the only one accessible via
#' API).
#'
#' @format A data frame with x rows and 8 columns:
#' \describe{
#'   \item{id}{id to use to query table}
#'   \item{table_name}{name of table}
#'   \item{code}{variable code}
#'   \item{text}{description of variable}
#'   \item{elimination}{aggregation behaviour}
#'   \item{time}{indicates variable contains time data}
#'   \item{values}{possible values for variable}
#'   \item{valueTexts}{description of value}
#' }
#' @source rscb::scb_create_table_cache()
"scb_table_cache"
