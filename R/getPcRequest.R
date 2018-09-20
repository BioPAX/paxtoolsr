#' Get a Pathway Commons Webservice Request 
#' 
#' @param url Pathway Commons webservice request URL 
#' @param verbose a boolean whether to display debugging information
#' 
#' @return request results 
#' 
#' @concept paxtoolsr
#' @keywords internal
#' @noRd
#' @importFrom httr HEAD GET content accept
getPcRequest <- function(url, verbose) {
    url <- URLencode(url)
    
    if(verbose) {
        cat("URL: ", url, "\n") 
    }
    
    #     tmp <- tryCatch(getURLContent(url, .opts=list(followlocation=TRUE)),
    #                     error=function(e) {
    #                         #DEBUG
    #                         #cat("X", e$message, "\n")
    #                         
    #                         code <- substr(e$message, 1, 3)
    #                         
    #                         # Make sure the code is numeric
    #                         if(grepl("^\\d+$", code)) {
    #                             message <- getErrorMessage(code)                                            
    #                         } else {
    #                             code <- NA
    #                             message <- e$message
    #                         }
    #                         
    #                         result <- paste("ERROR: Code:", code, "Message:", message)
    #                         result
    #                     })
    #    
    #    statusCode <- url.exists(url, .opts=list(followlocation=TRUE), .header=TRUE)["status"]
    statusCode <- ""
    
    maxTries <- 2 
    counter <- 0
    
    # Retry a couple of times after a few seconds
    while(statusCode != "200" && counter <= maxTries) {
        statusCode <- HEAD(url)$status   
        counter <- counter + 1
        Sys.sleep(3)
    }
    
    # Check HTTP status code; 200 is success 
    if(statusCode == "200") {
        #tmp <- getURLContent(url, .opts=list(followlocation=TRUE))
        
        #Set preference order of accept types
        tmp <- content(GET(url, 
                           accept("text/xml,text/plain,application/json"),
                           add_headers("Cache-Control"="nocache")), 
                           as="text")
    } else {
        # Make sure the statusCode is numeric
        if(grepl("^\\d+$", statusCode)) {
            message <- getErrorMessage(statusCode)                                            
        } else {
            statusCode <- NA
            message <- NA
        }
        
        tmp <- paste("ERROR: Code:", statusCode, "Message:", message)
    }
    
    if(grepl("^ERROR", tmp)) {
        stop(paste(tmp, "(PC Webservice Error)"))
    }
    
    return(tmp)
}
