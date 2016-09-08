#' Process Pathway Commons request in various formats
#' 
#' @param content a string, content to be processed
#' @param format a string, the type of format
#' 
#' @return an R object using one of the read* methods provided in this package 
#'   corresponding to the format
#' 
#' @examples 
#' fileName <- system.file("extdata", "test_biopax.owl", package="paxtoolsr")
#' content <- readChar(fileName, file.info(fileName)$size)
#' results <- processPcRequest(content, "BIOPAX")
#' 
#' @seealso \code{\link{pcFormats}}
#' 
#' @concept paxtoolsr
#' @export 
#' 
#' @importFrom rjson fromJSON
processPcRequest <- function(content, format, ...) {
    if(format == "JSON") {     
        results <- fromJSON(content)
        return(results)
    } else if(format == "XML") {
        results <- xmlTreeParse(content, useInternalNodes=TRUE)
        return(results)
    }
    
    filename <- tempfile() 
    write(content, file=filename) 
    stopifnot(file.info(filename)$size > 0)
    
    #DEBUG 
    #cat("FILENAME: ", filename, "\n") 
    
    if(format == "EXTENDED_BINARY_SIF") {
        results <- readSifnx(filename, ...) 
    } else if(format == "BINARY_SIF") {
        results <- readSif(filename, ...)           
    } else if(format == "BIOPAX") {     
        results <- readBiopax(filename, ...)
    } else if(format == "SBGN") {     
        results <- readSbgn(filename, ...)
    } else if(format == "GSEA") {     
        results <- readGmt(filename, ...)
    } else {
        results <- content
    }
    
    return(results)
}
