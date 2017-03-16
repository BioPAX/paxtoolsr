#' Get the neighbors of a set of IDs in a BioPAX file
#' 
#' This function retrieves a set of neighbors for a set of IDs in a BioPAX file.
#' 
#' @param inputFile a string with the name of the input BioPAX OWL file
#' @param outputFile a string with the name of the output BioPAX OWL file
#' @param idList a vector of IDs from the BioPAX OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details Only entities in the input BioPAX file will be searched for neighbors.
#' IDs used must be URIs for the entities of interest. 
#' 
#' @examples
#' outFile <- tempfile()
#' results <- getNeighbors(system.file("extdata", 
#'   "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr"), 
#'   outFile, 
#'   c("HTTP://WWW.REACTOME.ORG/BIOPAX/48887#PROTEIN2360_1_9606", 
#'     "HTTP://WWW.REACTOME.ORG/BIOPAX/48887#PROTEIN1631_1_9606")) 
#' 
#' @concept paxtoolsr
#' @export
getNeighbors <- function(inputFile, outputFile=NULL, idList) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    idList <- paste(idList, collapse=",")
    
    command <- "getNeighbors"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    idListJStr <- .jnew("java/lang/String", idList)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, inputJStr, idListJStr, outputJStr) 
    #DEBUG 
    #cat("ARGSLIST:", commandJStr, "\n")
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}
