#' Get base Pathway Commons URL
#' 
#' @return a string with base Pathway Commons URL
#' 
#' @details paxtoolsr will support versions Pathway Commons 5 and later. Old 
#' versions of the webservice will not be not be operational. Users can parse
#' older BioPAX outputs as an alternative. 
#' 
#' @examples 
#' url <- getPcUrl()
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom httr url_success
getPcUrl <- function() {
    url <- NULL
    
    #baseUrl <- "http://www.pathwaycommons.org/pc2"
    baseUrl <- "http://purl.org/pc2/"
    
    curUrl <- paste0(baseUrl, getOption("pc.version"), "/")
    tmpVersion <- as.numeric(getOption("pc.version")) + 1 
    nextUrl <- paste0(baseUrl, tmpVersion, "/")
    
    if(url_success(curUrl)) {
        url <- curUrl
    } 
    
    if(url_success(nextUrl)) {
        url <- nextUrl
    }     
    
    if(is.null(url)) {
        stop(paste("ERROR: Pathway Commons webservice cannot be reached. URLs tried:", curUrl, nextUrl))
    }
    
    return(url)
}
