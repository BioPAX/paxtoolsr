#' Get Pathway Commons BioPAX elements
#' 
#' This function will retrieve a set of BioPAX elements given a graph query match. 
#' 
#' @param kind graph query. Valid options can be found using \code{\link{pcGraphQueries}}
#'   See Details for information on graph queries. 
#' @param source source object's URI/ID. Multiple source URIs/IDs are allowed
#'   per query, for example c("http://identifiers.org/uniprot/Q06609", 
#'   "http://identifiers.org/uniprot/Q549Z0")
#'   See a note about MIRIAM and Identifiers.org in details
#' @param target [Required for PATHSFROMTO graph query] target URI/ID. Multiple
#'   target URIs are allowed per query; for example c("http://identifiers.org/uniprot/Q06609", 
#'   "http://identifiers.org/uniprot/Q549Z0")
#'   See a note about MIRIAM and Identifiers.org in details
#' @param direction [Optional, for NEIGHBORHOOD and COMMONSTREAM algorithms] -
#'   graph search direction. Valid options: \code{\link{pcDirections}}.
#' @param limit graph query search distance limit (default: 1).
#' @param format output format. Valid options: \code{\link{pcFormats}}
#' @param datasource datasource filter (same as for 'search').
#' @param organism organism filter (same as for 'search').
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return depending on the the output format a different object may be returned. 
#'   \code{\link{pcFormats}}
#' 
#' @seealso \code{\link{pcFormats}, \link{pcDirections}}
#' 
#' @examples
#' source <- "http://identifiers.org/uniprot/O14503"
#' #results <- graphPc(source=source, kind="neighborhood", format="TXT")
#' 
#' @concept paxtoolsr
#' @export 
graphPc <- function(kind, source, target=NULL, direction=NULL, limit=NULL, 
                    format=NULL, datasource=NULL, organism=NULL, 
                    verbose=FALSE) {
    
    baseUrl <- paste0(getPcUrl(), "graph")
    kindList <- list(kind=kind)
    
    sourceList <- NULL
    if(!is.null(source)) {
        # Put into the correct format
        #sources <- paste(paste0("source=", source), collapse="&")
        #url <- paste(url, "&", sources, sep="")
          
        sourceList <- lapply(source, function(x) { x })
        names(sourceList) <- rep("source", length(sourceList))
    }
    
    #DEBUG 
    #cat("TARGET: ", target, "\n")
    #cat("KIND: ", kind, "\n")
    
    targetList <- NULL
    if(kind == "PATHSFROMTO") {
        if(!is.null(target)) {
            #targets <- paste(paste0("target=", target), collapse="&")
            #url <- paste(url, "&", targets, sep="")
            
            targetList <- lapply(target, function(x) { x })
            names(targetList) <- rep("source", length(targetList))
        } else {
            stop("target must be set if kind is PATHSFROMTO")
        }
    }
    
    directionList <- NULL
    if(!is.null(direction)) {
      direction <- toupper(direction)     
      stopifnot(direction %in% pcDirections())
      
      #url <- paste(url, "&direction=", direction, sep="")
      directionList <- list(direction=direction)
    }
    
    limitList <- NULL
    if(!is.null(limit)) {
      #url <- paste(url, "&limit=", limit, sep="")
      limitList <- list(limit=limit)
    }
    
    # Pre-process arguments 
    ## Convert to uppercase to avoid issues in if statements
    format <- toupper(format)
    stopifnot(format %in% names(pcFormats()))
    formatList <- NULL
    if(!is.null(format)) {
      #url <- paste(url, "&format=", format, sep="")
      formatList <- list(format=format)
    }
    
    datasourceList <- NULL
    if(!is.null(datasource)) {
      # Put into the correct format
      #datasources <- paste(paste0("datasource=", datasource), collapse="&")
      #url <- paste(url, "&", datasources, sep="")
        
      datasourceList <- lapply(datasource, function(x) { x })
      names(datasourceList) <- rep("datasource", length(datasourceList))
    }
    
    organismList <- NULL
    if(!is.null(organism)) {
      # url <- paste(url, "&organism=", organism, sep="")
      organismList <- list(organism=organism)
    }
    
    queryList <- c(kindList, sourceList, targetList, directionList, limitList, 
               formatList, datasourceList, organismList)
    
    tmpUrl <- parse_url(baseUrl)
    tmpUrl$query <- queryList
    url <- build_url(tmpUrl)
    
    tmp <- getPcRequest(url, verbose)
    if(tmp == "") {
      stop("ERROR: Result was empty")
    }
    
    results <- processPcRequest(tmp, format)
    return(results)
}
