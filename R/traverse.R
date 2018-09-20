#' Access Pathway Commons using XPath-type expressions 
#' 
#' This command provides XPath-like access to the Pathway Commons. 
#' 
#' @param uri a BioPAX element URI - specified similarly to the 'GET' command
#'   above). Multiple IDs are allowed (uri=...&uri=...&uri=...).
#' @param path a BioPAX propery path in the form of
#'   property1[:type1]/property2[:type2]; see properties, inverse properties,
#'   Paxtools, org.biopax.paxtools.controller.PathAccessor.
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return an XMLInternalDocument with results
#' 
#' @details With traverse users can explicitly state the paths they would like
#'   to access. The format of the path query is in the form: [Initial 
#'   Class]/[property1]:[classRestriction(optional)]/[property2]... A "*" sign 
#'   after the property instructs path accessor to transitively traverse that 
#'   property. For example, the following path accessor will traverse through
#'   all physical entity components within a complex: 
#'   "Complex/component*/entityReference/xref:UnificationXref" The following
#'   will list display names of all participants of interactions, which are
#'   components (pathwayComponent) of a pathway (note: pathwayOrder property,
#'   where same or other interactions can be reached, is not considered here): 
#'   "Pathway/pathwayComponent:Interaction/participant*/displayName" The
#'   optional parameter classRestriction allows to restrict/filter the returned
#'   property values to a certain subclass of the range of that property. In the
#'   first example above, this is used to get only the Unification Xrefs. Path
#'   accessors can use all the official BioPAX properties as well as additional
#'   derived classes and parameters in paxtools such as inverse parameters and
#'   interfaces that represent anonymous union classes in OWL. (See Paxtools
#'   documentation for more details).
#' 
#' @examples
#' uri <- "http://identifiers.org/uniprot/P38398"
#' #results <- traverse(uri=uri, path="ProteinReference/organism/displayName")
#' 
#' @references Paxtools Documentation: \url{http://www.biopax.org/m2site/}
#' 
#' @concept paxtoolsr
#' @export   
#' @importFrom utils URLencode
traverse <- function(uri, path, verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "traverse")
    
    #uri <- unname(sapply(uri, URLencode, reserved=TRUE))
    #path <- URLencode(path, reserved = TRUE)

    uriList <- NULL
    if(!is.null(uri)) {
        # Put into the correct format
        #uris <- paste(paste0("uri=", uri), collapse="&")
        #url <- paste(baseUrl, uris, sep="")
        
        uriList <- lapply(uri, function(x) { x })
        names(uriList) <- rep("uri", length(uriList))
    }
    
    pathList <- list(path=path)
    
    queryList <- c(uriList, pathList)
    
    tmpUrl <- parse_url(baseUrl)
    tmpUrl$query <- queryList
    url <- build_url(tmpUrl)
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    return(results) 
}
