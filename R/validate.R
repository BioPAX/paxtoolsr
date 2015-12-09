#' Validate BioPAX files 
#' 
#' This function validates BioPAX files for errors.
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output file containing 
#'   validation results
#' @param type a string denoting the type of output: xml (default), html, biopax 
#' @param autoFix a boolean that determines if the input file should be 
#'   fixed automatically. Errors that can be automatically fixed include 
#'   generating displayName properties from names, inferring organism, and 
#'   inferring dataSource
#' @param onlyErrors a boolean of whether to only display errors
#' @param maxErrors a integer denoting the number of errors to return 
#' @param notStrict a boolean of whether to be strict in validation (default: FALSE)
#' 
#' @return an XMLInternalDocument is returned if type is set to "xml" otherwise
#'   the location of the outputfile is returned.  
#' 
#' @details See the publication by Rodchenkov, et al. for information on the 
#'   BioPAX validator. See \url{http://biopax.baderlab.org/validator} for 
#'   additional information on validator. 
#'   See \url{http://biopax.baderlab.org/validator/errorTypes.html} for 
#'   information on error types. 
#' 
#' @references Rodchenkov I, Demir E, Sander C, Bader GD. The BioPAX Validator, 
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/23918249}
#' 
#' @examples
#' outFile <- tempfile()
#' rawDoc <- validate(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), onlyErrors=TRUE) 
#' 
#' @concept paxtoolsr
#' @export
validate <- function(inputFile, outputFile=NULL, 
                     type=c("xml", "html", "biopax"),
                     autoFix=FALSE, onlyErrors=FALSE, maxErrors=NULL, 
                     notStrict=FALSE) {
    #DEBUG 
    #inputFile <- files[i]
    #outputfile <- NULL
    #type <- NULL
    #autoFix <- FALSE
    #onlyErrors <- FALSE
    #maxErrors <- NULL
    #notStrict <- TRUE
    
    inputFile <- checkInputFile(inputFile)
    type <- match.arg(type)
    
    command <- "validate"
    commandJStr <- .jnew("java/lang/String", command)
    
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    outputFile <- checkOutputFile(outputFile)
    
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    if(is.null(type)) {
        outputTypeJStr <- .jnew("java/lang/String", "xml")
    } else {
        outputTypeJStr <- .jnew("java/lang/String", type)       
    }
    
    argsList <- list(commandJStr, inputJStr, outputJStr, outputTypeJStr)
    
    if(autoFix) {
        autoFixJStr <- .jnew("java/lang/String", "auto-fix")
        argsList <- append(argsList, autoFixJStr)
    }
    
    if(onlyErrors) {
        onlyErrorsJStr <- .jnew("java/lang/String", "only-errors")
        argsList <- append(argsList, onlyErrorsJStr)
    }
    
    if(!is.null(maxErrors)) {
        maxErrorsJStr <- .jnew("java/lang/String", paste("maxerrors=", 
                                                         maxErrors, sep=""))
        argsList <- append(argsList, maxErrorsJStr)
    }
    
    if(notStrict) {
        nonStrictJStr <- .jnew("java/lang/String", "notstrict")
        argsList <- append(argsList, nonStrictJStr)     
    }
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    if(type == "xml") { 
        results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
        return(results)
    } else {
        return(outputFile) 
    }
}
