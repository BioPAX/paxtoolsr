#' Read PSIMI file
#' 
#' This function reads in a PSIMI file. 
#' 
#' @param inputFile a string of the name of the input PSIMI file
#' @param outputFile a string of the name of the output BioPAX OWL file
#' @param bpLevelArg a string representing the BioPAX level for the output file
#'   (default: NULL)
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details The Proteomics Standard Initiative (PSIMI) format is described at 
#' https://code.google.com/p/psimi/wiki/PsimiTabFormat
#' 
#' @examples
#' outFile <- tempfile()
#' results <- fromPsimi(system.file("extdata", "10523676-compact.xml", package="paxtoolsr"), 
#'                                  outFile, 
#'                                  "3")
#'                                  
#' @concept paxtoolsr
#' @export
fromPsimi <- function(inputFile, outputFile=NULL, bpLevelArg=3) {
    command <- "fromPsimi"
    
    outputFile <- checkOutputFile(outputFile)
    bpLevelArg <- as.character(bpLevelArg)
    
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    bpLevelArgJStr <- .jnew("java/lang/String", bpLevelArg)
    
    argsList <- list(commandJStr, bpLevelArgJStr, inputJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
    
}
