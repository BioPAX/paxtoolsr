#' Merges two BioPAX OWL files
#' 
#' This function merges two BioPAX OWL files 
#' 
#' @param inputFile1 a string of the name of the input BioPAX OWL file 
#' @param inputFile2 a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output merged BioPAX 
#'   OWL file (Optional)
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details Only entities that share IDs will be merged. No additional merging
#'   occurs on cross-references. Merging may result in warning messages caused 
#'   as a result of redundant actions being checked against by the Java library; 
#'   these messages may be ignored. 
#' 
#' @examples    
#' outFile <- tempfile()
#' results <- mergeBiopax(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'                        package="paxtoolsr"), 
#'                        system.file("extdata", "dna_replication.owl", 
#'                        package="paxtoolsr"), 
#'                        outFile) 
#' 
#' @concept paxtoolsr
#' @export
mergeBiopax <- function(inputFile1, inputFile2, outputFile=NULL) {
    inputFile1 <- checkInputFile(inputFile1)
    inputFile2 <- checkInputFile(inputFile2)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "merge"
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
