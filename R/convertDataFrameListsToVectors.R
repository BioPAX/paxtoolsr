#' Convert columns with list in data.frame to vector
#' 
#' @param df a data.frame
#' @param delimiter a delimiter to concatenate (DEFAULT: ;)
#' 
#' @results a data.frame without list columns 
#' 
#' @note Lists as columns are useful programmatically, but cause issue in writing 
#'   output to text-based files
#'   
#' @concept paxtoolsr
#' @export
convertDataFrameListsToVectors <- function(df, delimiter=";") {
    listCols <- sapply(1:ncol(df), function(x, y) {
        is.list(y[,x])
    }, df)
    listCols <- which(listCols)
    
    for(i in listCols) {
        tmp <- sapply(df[,i], function(x) {
            paste(x, collapse = delimiter)
        })
        df[,i] <- tmp
    }
    
    return(df)
}
