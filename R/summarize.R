#' Summarize a BioPAX file
#' 
#' This function provides a summary of BioPAX classes.
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file 
#' @return list with BioPAX class counts 
#' 
#' @details BioPAX classes are defined by the BioPAX specification: 
#'   \url{http://www.biopax.org/}
#' 
#' @examples
#' summary <- summarize(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#' package="paxtoolsr")) 
#' 
#' @concept paxtoolsr
#' @export
summarize <- function(inputFile) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- tempfile()
    
    command <- "summarize"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    argsList <- list(commandJStr, inputJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    # Make a vector with each line text as a vector entry
    lines <- readLines(outputFile)
    
    results <- list()
    
    for(line in lines) {        
        
        # Makes sure a line matches
        if(grepl("^[A-Za-z]+\\s=\\s\\d+", line)) {
            
            # Removes any characters at the end of the line that probably have parentheses
            tmp <- gsub("^([A-Za-z]+)\\s=\\s(\\d+).*", "\\1=\\2", line) 
            
            # Produces a vector with two entries 
            tmp2 <- strsplit(tmp, "=")[[1]]
            results[[tmp2[1]]] <- tmp2[2]
        }
    }
    
    return(results)
}
