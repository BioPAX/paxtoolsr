#' List files in cache directory 
#' 
#' @return a vector of the files in the cache directory
#' 
#' @examples 
#' getCacheFiles()
#' 
#' @concept paxtoolsr
#' @export
getCacheFiles <- function() {
    return(dir(Sys.getenv("PAXTOOLSR_CACHE")))
}
