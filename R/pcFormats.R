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
    #pcFormats <- c("BINARY_SIF", "BIOPAX", "EXTENDED_BINARY_SIF", "GSEA", "SBGN") 
    pcFormats <- list("BIOPAX"="BioPAX Level 3 RDF/XML Format",
                      "GSEA"="Gene Set Expression Analysis Format",
                      "JSONLD"="JSON-LD format",
                      "SBGN"="Systems Biology Graphical Notation Format",
                      "SIF"="Simple Binary Interaction Format",
                      "TXT"="Extended SIF")
    
    return(pcFormats)
}
