#' Download Pathway Commons files (uses menu and cache)
#' 
#' @param selectedFileName a string, a name of a file to skip the the interactive selection
#' @param destDir a string, the destination directory for the file to be 
#'   downloaded (Default: NULL). If NULL, then file will be downloaded to cache
#'   directory file.path(Sys.getenv("HOME"), ".paxtoolsRCache")
#'   
#' @return an R object using one of the read* methods provided in this package 
#'   corresponding to the file downloaded 
#'   
#' @examples 
#' \dontrun{
#'   downloadPc2(tempdir())
#' }
#'   
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom R.utils gunzip
downloadPc2 <- function(selectedFileName=NULL, destDir=NULL) {
    baseUrl <- "http://www.pathwaycommons.org"
    downloadsSubDir <- "/pc2/downloads/"
    
    # Parse webpage
    doc <- htmlParse(paste0(baseUrl, downloadsSubDir)) 
    
    # Extract links
    links <- xpathSApply(doc, "//a/@href")
    
    # Process links; get only gzipped files
    idx <- grepl(".gz", links)
    tmp <- strsplit(links[idx], ";")
    tmp2 <- lapply(tmp, function(x) { x[1] }) 
    tmp3 <- unname(unlist(tmp2))        
    
    filenames <- gsub(downloadsSubDir, "", tmp3)
    
    # Construct URLs
    tmp3 <- paste0(baseUrl, downloadsSubDir, filenames)
    
    # Show menu if user does not specify a file
    if(is.null(selectedFileName)) {
        selectedFileName <- select.list(filenames, graphics=FALSE)
    }
    
    if(is.null(destDir)) {
        stopifnot(Sys.getenv("PAXTOOLSR_CACHE") != "")
        selectedFilePath <- file.path(Sys.getenv("PAXTOOLSR_CACHE"), selectedFileName)
    } else {
        selectedFilePath <- file.path(destDir, selectedFileName)
    }
    
    # Download file if it does not exist
    if(!file.exists(selectedFilePath)) {
        # NOTE: File not found if not first URL encoded
        tmpUrl <- paste0(baseUrl, downloadsSubDir)
        downloadResult <- downloadFile(baseUrl=tmpUrl, fileName=selectedFileName, destDir=destDir)
        
        #DEBUG
        #str(tmpUrl)
        #str(selectedFileName)
        #str(destDir)
        
        stopifnot(downloadResult)
    }
    
    tmpFile <- gunzip(selectedFilePath, remove=FALSE, temporary=TRUE, skip=TRUE)
    
    # Parse GMT 
    if(grepl("GSEA", selectedFileName)) {
        results <- readGmt(tmpFile)
        return(results)
    }      
    
    # Parse EXTENDED_BINARY_SIF
    if(grepl("EXTENDED_BINARY_SIF", selectedFileName)) {
        results <- readSifnx(tmpFile)
        return(results)
    }      
    
    # Parse BINARY_SIF
    if(grepl("BINARY_SIF", selectedFileName)) {
        results <- readSif(tmpFile)
        return(results)
    }  
    
    # Parse BIOPAX
    if(grepl("BIOPAX", selectedFileName)) {
        results <- readBiopax(tmpFile)
        return(results)
    }
}
