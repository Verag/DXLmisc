#' Check id file is recent
#'
#' Check if it has been updated for more than x days
#'
#' @param path_file character variable
#'
#' @param margin_days numeric variable
#'
#' @return string
#'
#' @examples
#'
#' @export
check_if_update <- function(path_file, margin_days) {

  if (file.exists(path_file)) {
    if (ymd_hms(file.mtime(path_file)) < ymd(Sys.Date()) - days(margin_days)) {
      warning("a última atualização do ficheiro foi numa data superior ao thresold. necessário confirmar se se mantém atualizado.")
    } else {
      message("o ficheiro especificado foi encontrado em sistema e encontra-se atualizado mediante o critério definido.")
    }
  } else {
    stop("o ficheiro não foi encontrado com sucesso mediante o caminho especificado.")
  }
}
