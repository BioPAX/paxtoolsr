#' Convert a BioPAX OWL file to SBGNML
#' 
#' This function will convert a BioPAX OWL file into the Systems Biology Graphical
#' Notation (SBGN) Markup Language (SBGNML) XML representation 
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output SBGNML file
#' @return an XMLInternalDocument representing a SBGNML file
#'
#' @details Objects in the SBGNML format are laid out using a Compound Spring 
#' Embedder (CoSE) layout
#'
#' @references \url{http://www.cs.bilkent.edu.tr/~ivis/layout/cose-animated-demo/cose.html}
#'
#' @examples 
#' outFile <- tempfile()
#' results <- toSBGN(system.file("extdata", "biopax3-short-metabolic-pathway.owl", 
#'   package="paxtoolsr"), 
#'   outFile) 
#' 
#' @concept paxtoolsr
#' @export
toSBGN <- function(inputFile, outputFile=NULL) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "toSBGN"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, inputJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}
