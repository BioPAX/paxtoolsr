#' Utility method; create temporary file if necessary
#' 
#' @param file a string or XMLInternalDocument
#' @return location of file 
#' 
#' @concept paxtoolsr
#' @noRd
checkInputFile <- function(file) {
    if(typeof(file) == "externalptr") {
        tmp <- tempfile()
        saveXML(file, tmp)
        
        file <- tmp
    }
    
    return(file)
}
