#' Convert a PSIMI or older BioPAX OWL file to BioPAX Level 3
#' 
#' This file will convert PSIMI or older BioPAX objects to BioPAX Level 3
#' 
#' @param inputFile a string of the name of the input file
#' @param outputFile a string of the name of the output BioPAX OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @examples
#' inputFile <- system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr")
#' outFile <- tempfile()
#' results <- toLevel3(inputFile, outFile) 
#' 
#' @concept paxtoolsr
#' @export
toLevel3 <- function(inputFile, outputFile=NULL) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "toLevel3" 
    
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, inputJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    #results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    results <- readBiopax(inputFile)

    return(results) 
}
