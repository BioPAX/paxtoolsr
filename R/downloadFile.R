#' Check Cache and Download File 
#' 
#' @param baseUrl a string, entire download URL except filename
#' @param fileName a string, the filename of file to be downloaded 
#' @param destDir a string, the path where a file should be saved 
#' @param cacheEnv a string, the environment variable that points to the specific cache
#' @param verbose show debugging information
#' 
#' @return a boolean TRUE if the file was downloaded or already exists, FALSE otherwise
#' 
#' @details 
#' Description of file formats: http://www.pathwaycommons.org/pc2/formats
#' 
#' @examples 
#' downloadFile("http://google.com/", fileName="index.html", destDir=tempdir())
#' 
#' @concept paxtoolsr
#' @seealso \code{\link{readSif}, \link{readBiopax}, \link{readSbgn}, \link{readSifnx}, \link{readGmt}}
#' @export
#' 
#' @importFrom httr HEAD GET http_status write_disk progress add_headers http_date
downloadFile <- function(baseUrl, fileName, destDir=NULL, cacheEnv="PAXTOOLSR_CACHE", verbose=FALSE) {
    url <- URLencode(paste0(baseUrl, fileName))
    fileIdx <- NULL
    
    if(!is.null(destDir)) {
        filePath <- file.path(destDir, fileName)
    } else {
        cacheMapPath <- file.path(Sys.getenv(cacheEnv), "cacheMap.txt")
        cacheMap <- read.table(cacheMapPath, sep="\t", header=TRUE, stringsAsFactors=FALSE)
        filePath <- file.path(Sys.getenv(cacheEnv), fileName)
        
        fileIdx <- which(cacheMap[,"fileName"] == fileName)
    }
    
    if(verbose) {
        cat("filePath: ", filePath, "\n")
    }
    
    if(length(fileIdx) == 0) {
        headResp <- HEAD(url, add_headers("If-Modified-Since"=""))    
    } else {
        headResp <- HEAD(url, add_headers("If-Modified-Since"=cacheMap[fileIdx,"retrievedDate"]))   
    }

    httpStatus <- http_status(headResp)
    
    if(grepl("success", httpStatus$category, ignore.case=TRUE)) {
        getResp <- GET(url=url, write_disk(filePath, overwrite=TRUE), progress())
        
        if(is.null(destDir)) {
            # Current date
            retrievedDate <- http_date(as.POSIXlt(Sys.time(), "GMT"))
            
            if(length(fileIdx) == 0) {
                cacheMap <- rbind(cacheMap, data.frame(fileName=fileName, retrievedDate=retrievedDate, url=url))
            } else {
                cacheMap[fileIdx,"fileName"] <- fileName
                cacheMap[fileIdx,"retrievedDate"] <- retrievedDate
                cacheMap[fileIdx,"url"] <- url
            }
            
            write.table(cacheMap, file=cacheMapPath, quote=FALSE, sep="\t", row.names=FALSE)            
        }
    }
    
    if(length(fileIdx) != 0 || grepl("success", httpStatus$category, ignore.case=TRUE)) {
        return(TRUE)
    } else {
        if(verbose) {
            cat("fileIdx: ", fileIdx, "\n")   
            cat("httpStatus$category: ", httpStatus$category, "\n")   
        }
        
        return(FALSE)
    }
}
