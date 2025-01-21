#' Check if file is recent
#'
#' Check if it has been updated for more than x days
#'
#' @param path_file character variable
#'
#' @param margin_days numeric variable
#'
#' @return string
#'
#' @export
check_if_update <- function(path_file, margin_days) {
  if (!file.exists(path_file)) {
    stop("File NOT FOUND.")
  }

  file_mod_time <- lubridate::ymd_hms(file.mtime(path_file))
  threshold_date <- lubridate::ymd(Sys.Date()) - lubridate::days(margin_days)

  if (file_mod_time < threshold_date) {
    warning("File is outdated: CHECK IF UPDATED.")
  } else {
    message("File is up to date.")
  }
}
