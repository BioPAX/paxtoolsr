#' Read SBGN files as XML documents 
#' 
#' @param inputFile an inputFile
#' @return an XMLInternalDocument
#' 
#' @examples 
#' results <- readSbgn(system.file("extdata", "test_sbgn.xml", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readSbgn <- function(inputFile) {
    results <- xmlTreeParse(inputFile, useInternalNodes=TRUE)
    return(results)
}
