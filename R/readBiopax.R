#' Read BioPAX files as XML documents 
#' 
#' @param inputFile an inputFile
#' @return an XMLInternalDocument
#' 
#' @examples 
#' results <- readBiopax(system.file("extdata", "biopax3-short-metabolic-pathway.owl", 
#'   package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readBiopax <- function(inputFile) {
    results <- xmlTreeParse(inputFile, useInternalNodes=TRUE)
    return(results)
}
