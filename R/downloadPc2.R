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
downloadPc2 <- function(selectedFileName=NULL, destDir=NULL, version=NULL) {
    if(!is.null(version)) {
        baseUrl <- Sys.getenv("PC_ARCHIVE_URL")
        
        doc <- htmlParse(baseUrl) 
        links <- xpathSApply(doc, "//a/@href")
        
        idx <- grepl(paste0("^v", version), links)
        
        downloadsSubDir <- unname(links[idx])
    } else {
        baseUrl <- Sys.getenv("PC_URL")
        #baseUrl <- "http://www.pathwaycommons.org/pc2/"
        downloadsSubDir <- "downloads/"
    }
    
    url <- paste0(baseUrl, downloadsSubDir)
    
    # Parse webpage
    doc <- htmlParse(url) 
    
    # Extract links
    links <- xpathSApply(doc, "//a/@href")    
    
    # Process links; get only gzipped files
    idx <- grepl(".gz", links)
    tmp <- strsplit(links[idx], ";")
    tmp2 <- lapply(tmp, function(x) { x[1] }) 
    tmp3 <- unname(unlist(tmp2))        
    
    #filenames <- gsub(downloadsSubDir, "", tmp3)
    # Remove any existing starting slash
    #filenames <- gsub("/", "", filenames)
    
    tmp4 <- lapply(strsplit(tmp3, "/"), function(url) {
        url[length(url)]
    })
    
    filenames <- unlist(tmp4)
    
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
        
        if(!downloadResult) {
            stop("ERROR: File was not found.") 
        }
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
