#' Retrieve top pathways
#' 
#' This command returns all "top" pathways.
#' 
#' @param q [Optional] a keyword, name, external identifier, or a Lucene query string, like in 'search', but the default is '*' (match all).
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
#' #results <- topPathways(datasource="panther")
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom plyr ldply
#' @importFrom httr build_url parse_url
topPathways <- function(q=NULL, datasource=NULL, organism=NULL, verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "top_pathways")
    
    queryList <- list()

    if(!is.null(q)) {
        queryList[["q"]] <- q
    }
    
    if(!is.null(datasource)) {
        queryList[["datasource"]] <- datasource
    }
    
    if(!is.null(organism)) {
        queryList[["organism"]] <- organism
    }
    
    tmpUrl <- parse_url(baseUrl)
    tmpUrl$query <- queryList
    url <- build_url(tmpUrl)
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    
    #DEBUG
    #str(results)
    #return(results)
    
    resultsDf <- ldply(xmlToList(results), data.frame, stringsAsFactors=FALSE)
    
    if("organism" %in% colnames(resultsDf)) {
        resultsDf <- resultsDf[,c("uri", "biopaxClass", "name", 
                                  "dataSource", "organism")]        
    } else {
        resultsDf <- resultsDf[,c("uri", "biopaxClass", "name", 
                                  "dataSource")]    
    }

    # Remove NAs
    resultsDf <- resultsDf[which(!is.na(resultsDf[,"uri"])),]
    
    return(resultsDf) 
}
