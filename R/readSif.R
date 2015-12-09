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
#' @importFrom data.table fread
readSif <- function(inputFile) {
    results <- fread(inputFile, sep="\t", header=TRUE, stringsAsFactors=FALSE)
    results <- as.data.frame(results)
    colnames(results) <- c("PARTICIPANT_A",  "INTERACTION_TYPE", "PARTICIPANT_B")
    
    return(results)
}
