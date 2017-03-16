#' Get Pathway Commons BioPAX elements
#' 
#' This command retrieves full pathway information for a set of elements such as
#' pathway, interaction or physical entity given the RDF IDs. 
#' 
#' @param uri a vector that includes valid/existing BioPAX element's URI (RDF
#'   ID; for utility classes that were "normalized", such as entity refereneces
#'   and controlled vocabularies, it is usually a Idntifiers.org URL. Multiple
#'   IDs are allowed per query, for example,
#'   c("http://identifiers.org/uniprot/Q06609",
#'   "http://identifiers.org/uniprot/Q549Z0") See also about MIRIAM and
#'   Identifiers.org in details. 
#' @param format output format (Default: BIOPAX). Valid options can be found using 
#'   \code{\link{pcFormats}}
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @param ... additional arguments to read* methods that handle data from Pathway Commons
#' 
#' @return a XMLInternalDocument object
#' 
#' @details Get commands only retrieve the BioPAX elements that are directly
#'   mapped to the ID. Use the "traverse query to traverse BioPAX graph and
#'   obtain child/owner elements.
#'   
#'   Information on MIRIAM and Identifiers.org 
#'   \url{http://www.pathwaycommons.org/pc2/#miriam}
#' 
#' @seealso \code{\link{pcFormats}}
#' 
#' @examples 
#' uri <- "http://identifiers.org/uniprot/O14503"
#' #results <- getPc(uri)
#' 
#' uri <- c("http://identifiers.org/uniprot/O14503", "http://identifiers.org/uniprot/Q9P2X7")
#' #results <- getPc(uri, verbose=TRUE)
#' 
#' @concept paxtoolsr
#' @export
getPc <- function(uri, format="BIOPAX", verbose=FALSE, ...) {
    uris <- paste(paste0("uri=", uri), collapse="&")
    
    baseUrl <- paste0(getPcUrl(), "get?")
    url <- paste(baseUrl, uris, sep="") 
    
    stopifnot(format %in% pcFormats())
    
    if(!is.null(format)) {
        url <- paste0(url, "&format=", format)
    }
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, format, ...)
    return(results) 
}
