#' Get Error Message for a Pathway Commons Error 
#' 
#' @param code a three digit numerical error code
#' @return an error message for the code 
#' 
#' @examples 
#' results <- getErrorMessage("452")
#' 
#' @concept paxtoolsr
#' @export
getErrorMessage <- function(code) {
    codes <- c("452", "460", "500", "503")
    messages <- c("Bad Request (illegal or no arguments)", 
                  "No Results Found", 
                  "Internal Server Error", 
                  "Server is temporarily unavailable due to regular maintenance")
    
    errors <- data.frame(codes=codes, messages=messages, stringsAsFactors=FALSE)
    
    message <- errors$messages[errors$codes==code]
    
    if(length(message)==1) {
        return(message)     
    } else {
        return("Unknown Error")
    }
}
