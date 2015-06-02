#' Acceptable Pathway Commons Formats 
#' 
#' A simple function to see valid options 
#' 
#' @return acceptable Pathway Commons formats 
#' 
#' @details See references.
#' 
#' @references Output Formats Description: \url{http://www.pathwaycommons.org/pc2/help/formats.html}
#' 
#' @examples
#' pcFormats()
#' 
#' @concept paxtoolsr
#' @export 
pcFormats <- function() {
    pcFormats <- c("BINARY_SIF", "BIOPAX", "EXTENDED_BINARY_SIF", "GSEA", "SBGN") 
    
    return(pcFormats)
}
