#' Utility method; create temporary file if necessary
#' 
#' @param file a string
#' @return location of file 
#' 
#' @concept paxtoolsr
#' @noRd
checkOutputFile <- function(file) {
    if(is.null(file)) {
        file <- tempfile() 
    }    
    
    return(file)
}
