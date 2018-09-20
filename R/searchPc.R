#' Search Pathway Commons
#' 
#' This command provides a text search using the Lucene query syntax. 
#' 
#' @param q a keyword, name, external identifier, or a Lucene query string.
#' @param page an integer giving the search result page number (N>=0, default:
#'   0)
#' @param datasource a vector that is a filter by data source (use names or URIs
#'   of pathway data sources or of any existing Provenance object). If multiple
#'   data source values are specified, a union of hits from specified sources is
#'   returned. For example, datasource as c("reactome", "pid") returns hits
#'   associated with Reactome or PID.
#' @param organism a vector that is an organism filter. The organism can be
#'   specified either by official name, e.g. "homo sapiens" or by NCBI taxonomy
#'   id, e.g. "9606". Similar to data sources, if multiple organisms are
#'   declared a union of all hits from specified organisms is returned. For
#'   example organism as c("9606", "10016") returns results for both human and
#'   mice. Only humans, "9606" is officially supported.
#' @param type BioPAX class filter. See Details.
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return an XMLInternalDocument with results
#' 
#' @details Indexed fields were selected based on most common searches. Some of 
#' these fields are direct BioPAX properties, others are composite
#' relationships. All index fields are (case-sensitive):comment, ecnumber,
#' keyword, name, pathway, term, xrefdb, xrefid, dataSource, and organism. The
#' pathway field maps to all participants of pathways that contain the
#' keyword(s) in any of its text fields. This field is transitive in the sense
#' that participants of all sub-pathways are also returned. Finally, keyword is
#' a transitive aggregate field that includes all searchable keywords of that
#' element and its child elements - e.g. a complex would be returned by a
#' keyword search if one of its members has a match. Keyword is the default
#' field type. All searches can also be filtered by data source and organism. It
#' is also possible to restrict the domain class using the 'type' parameter.
#' This query can be used standalone or to retrieve starting points for graph
#' searches. Search strings are case insensitive unless put inside quotes.
#' 
#' BioPAX classes can be found at \url{http://www.pathwaycommons.org/pc2/#biopax_types}
#' 
#' @examples
#' query <- "Q06609"
#' #results <- searchPc(query)
#' 
#' query <- "glycolysis"
#' #results <- searchPc(query, type="Pathway")
#' 
#' @concept paxtoolsr
#' @export
searchPc <- function(q, page=0, datasource=NULL, organism=NULL, type=NULL, 
                     verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "search.xml")
    
    qList <- list(q=q)
    pageList <- list(page=page)
    
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
      #organisms <- paste(paste0("organism=", organism), collapse="&")
      #url <- paste(url, "&", organisms, sep="")
      
      organismList <- lapply(organism, function(x) { x })
      names(organismList) <- rep("organism", length(organismList))
    }
    
    typeList <- NULL
    if(!is.null(type)) {
      #url <- paste(url, "&type=", type, sep="")
      typeList <- list(type=type)
    }
    
    queryList <- c(qList, pageList, datasourceList, organismList, typeList)
    
    tmpUrl <- parse_url(baseUrl)
    tmpUrl$query <- queryList
    url <- build_url(tmpUrl)
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    
    return(results)
}
