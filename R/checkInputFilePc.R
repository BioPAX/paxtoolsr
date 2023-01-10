#' Check input files from PC
#'
#' @param inputFile Path to file
#'
#' @details Program will terminate if checks are invalid
#'
#' @return No value is returned
#'
#' @concept paxtoolsr
#' @noRd
checkInputFilePc <- function(inputFile) {
  if(!file.exists(inputFile)) {
    stop("ERROR: inputFile not file.")
  }

  if(file.size(inputFile) > 2e9) {
    stop("ERROR: A maximum file size limit of 1GB has been placed on files being read. Reading larger files with this function may be very slow. Please contact package author for workarounds.")
  }
}
