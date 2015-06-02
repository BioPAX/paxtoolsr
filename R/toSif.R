#' Convert a BioPAX OWL file to SIF
#' 
#' Convert a BioPAX OWL file to a binary SIF file 
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output SIF file (Optional)
#' @return a 3-column data.frame where the columns are named: PARTICIPANT_A, 
#'   INTERACTION_TYPE, PARTICIPANT_B
#'   
#' @details Information on SIF conversion is provided on the Pathway Commons 
#'   site: \url{http://www.pathwaycommons.org/pc2/}
#'  
#' @examples   
#' outFile <- tempfile()
#' results <- toSif(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   outFile) 
#'   
#' @concept paxtoolsr
#' @export
toSif <- function(inputFile, outputFile=NULL) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    #DEBUG 
    #cat("OUTPUTFILE: ", outputFile)
    
    command <- "toSif"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, inputJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- read.table(outputFile, sep="\t", as.is=TRUE, quote="")
    colnames(results) <- c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B")
    
    return(results)        
}
