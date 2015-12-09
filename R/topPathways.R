#' Retrieve top pathways
#' 
#' This command returns all "top" pathways.
#' 
#' @param datasource filter by data source (same as for 'search').
#' @param organism organism filter (same as for 'search').
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return a data.frame with the following columns:
#'   \itemize{ 
#'     \item uri URI ID for the pathway
#'     \item biopaxClass the type of BioPAX object
#'     \item name a human readable name
#'     \item dataSource the dataSource for the pathway 
#'     \item organism an organism identifier 
#'     \item pathway URI ID for the pathway
#'   }
#'   
#' @details Pathways that are neither 'controlled' nor 'pathwayComponent' of 
#'   another process.
#' 
#' @examples
#' datasource <- "panther"
#' #results <- topPathways(datasource=datasource)
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom plyr ldply
topPathways <- function(datasource=NULL, organism=NULL, verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "top_pathways?")
    url <- baseUrl
    
    if(!is.null(datasource)) {
        url <- paste(url, "&datasource=", datasource, sep="")
    }
    
    if(!is.null(organism)) {
        url <- paste(url, "&organism=", organism, sep="")
    }
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    
    #DEBUG
    #return(results)
    
    resultsDf <- ldply(xmlToList(results), data.frame, stringsAsFactors=FALSE)
    resultsDf <- resultsDf[,c("uri", "biopaxClass", "name", 
                              "dataSource", "organism")]
    
    # Remove NAs
    resultsDf <- resultsDf[which(!is.na(resultsDf[,"uri"])),]
    
    return(resultsDf) 
}
