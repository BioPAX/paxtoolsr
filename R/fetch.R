#' Fetch a set of IDs from a BioPAX OWL file
#'
#' This function will create a subsetted object with specified URIs. 
#'
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string with the name of the output BioPAX OWL file
#' @param idList a vector of IDs from the BioPAX OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details Only entities in the input BioPAX file will be used in the fetch.
#' IDs used must be URIs for the entities of interest. Additional properties 
#' such as cross-references for fetched entities will be included in the output. 
#' 
#' @examples 
#' outFile <- tempfile()
#' ids <- c("http://identifiers.org/uniprot/P36894", 
#'          "http://identifiers.org/uniprot/Q13873")
#' results <- fetch(system.file("extdata", "REACT_12034-3.owl", package="paxtoolsr"), 
#'                  outFile, ids)
#' 
#' @concept paxtoolsr
#' @export
fetch <- function(inputFile, outputFile=NULL, idList) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    idList <- paste(idList, collapse=",")
    
    command <- "fetch"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    idListJStr <- .jnew("java/lang/String", idList)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, inputJStr, idListJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}
