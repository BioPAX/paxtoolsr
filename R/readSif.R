#' Read in a binary SIF file
#'
#' @param inputFile an inputFile
#' @return a data.frame with the interactions in the binary SIF format
#'
#' @examples
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#'
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom readr read_tsv cols
readSif <- function(inputFile) {
  checkInputFilePc(inputFile)

  results <- read_tsv(inputFile, 
                      progress = TRUE, 
                      col_names = c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"), 
                      col_types = cols(.default = "c"))
  
  results <- as.data.frame(results)
  #colnames(results) <- c("PARTICIPANT_A",  "INTERACTION_TYPE", "PARTICIPANT_B")

  return(results)
}
