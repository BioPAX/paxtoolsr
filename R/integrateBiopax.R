#' Integrate two BioPAX OWL files (DEPRECATED)
#' 
#' This function merges two BioPAX OWL files 
#' 
#' @param inputFile1 a string of the name of the input BioPAX OWL file 
#' @param inputFile2 a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output integrated BioPAX 
#'   OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details This method is deprecated. Use mergeBiopax instead. 
#' 
#' @examples
#' outFile <- tempfile()
#' results <- integrateBiopax(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
#'   outFile) 
#' 
#' @concept paxtoolsr
#' @export
#' @seealso \code{\link{mergeBiopax}}
integrateBiopax <- function(inputFile1, inputFile2, outputFile=NULL) {
    inputFile1 <- checkInputFile(inputFile1)
    inputFile2 <- checkInputFile(inputFile2)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "integrate"
    commandJStr <- .jnew("java/lang/String", command)
    file1JStr <- .jnew("java/lang/String", inputFile1)
    file2JStr <- .jnew("java/lang/String", inputFile2)
    
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, file1JStr, file2JStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}
