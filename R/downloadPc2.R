#' Download Pathway Commons files (uses menu and cache)
#' 
#' @param selectedFileName a string, a name of a file to skip the the interactive selection
#' @param destDir a string, the destination directory for the file to be 
#'   downloaded (Default: NULL). If NULL, then file will be downloaded to cache
#'   directory file.path(Sys.getenv("HOME"), ".paxtoolsRCache")
#' @param returnNames return a vector of names matching the given regular expression
#' @param version a version number for a previous version of Pathway Commons data; 
#'   versions 3 and above. Parameter set as version="8". Available versions "http://www.pathwaycommons.org/archives/PC2/"
#' @param verbose a flag to display debugging information (Default: FALSE)  
#' @param ... additional parameters to send to corresponding read* methods
#'   
#' @return an R object using one of the read* methods provided in this package 
#'   corresponding to the file downloaded 
#'   
#' @examples 
#' \dontrun{
#'   downloadPc2(version="8") 
#'   downloadPc2(version="8", returnNames="ext.*sif")
#'   downloadPc2("PathwayCommons.8.inoh.GSEA.hgnc.gmt.gz", version="8", verbose=TRUE)
#' }
#'   
#' @aliases downloadPc  
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom R.utils gunzip
#' @importFrom utils select.list
downloadPc2 <- function(selectedFileName=NULL, destDir=NULL, returnNames=NULL, version, verbose=FALSE, ...) {
    if(is.null(destDir)) {
        stopifnot(Sys.getenv("PAXTOOLSR_CACHE") != "")
        destDir <- Sys.getenv("PAXTOOLSR_CACHE")
    }
    
    selectedFilePath <- file.path(destDir, selectedFileName)
    
    # Download file if it does not exist
    if(identical(selectedFilePath, character(0)) || !file.exists(selectedFilePath) || !is.null(returnNames)) {
        if(!is.null(version)) {
            baseUrl <- Sys.getenv("PC_ARCHIVE_URL")
            
            doc <- htmlParse(baseUrl) 
            links <- xpathSApply(doc, "//a/@href")
            
            if(version == "current") {
                idx <- grepl(paste0("^current"), links)
            } else {
                idx <- grepl(paste0("^v", version), links)
            }
            
            downloadsSubDir <- unname(links[idx])
        } else {
            baseUrl <- Sys.getenv("PC_URL")
            #baseUrl <- "http://www.pathwaycommons.org/pc2/"
            downloadsSubDir <- "downloads/"
        }
        
        url <- paste0(baseUrl, downloadsSubDir)
        
        if(verbose) {
            cat("URL: ", url, "\n")
        }
        
        # Parse webpage
        doc <- htmlParse(url) 
        
        # Extract links
        links <- xpathSApply(doc, "//a/@href")    
        
        # Process links; get only gzipped files
        idx <- grepl(".gz", links)
        tmp <- strsplit(links[idx], "/")
        tmp2 <- lapply(tmp, function(x) { x[length(x)] }) 
        
        tmp3 <- unname(unlist(tmp2))  
        #tmp3 <- strsplit(tmp3, ";")
        tmp3 <- lapply(tmp3, function(x) { x[1] }) 
        
        #filenames <- gsub(downloadsSubDir, "", tmp3)
        # Remove any existing starting slash
        #filenames <- gsub("/", "", filenames)
        
        #tmp4 <- lapply(strsplit(tmp3, "/"), function(url) {
        #    url[length(url)]
        #})
        
        filenames <- unlist(tmp3)
        
        if(!is.null(returnNames)) {
            idx <- grepl(returnNames, filenames, ignore.case=TRUE)
            return(filenames[idx])
        }
        
        # Construct URLs
        tmp3 <- paste0(baseUrl, downloadsSubDir, filenames)
        
        # Show menu if user does not specify a file
        if(is.null(selectedFileName)) {
            selectedFileName <- select.list(filenames, graphics=FALSE)
        }
        
        # NOTE: File not found if not first URL encoded
        tmpUrl <- paste0(baseUrl, downloadsSubDir)
        
        if(verbose) {
            cat("baseUrl: ", tmpUrl, "\n")
            cat("fileName: ", selectedFileName, "\n")
            cat("destDir: ", destDir, "\n")
        }
        
        downloadResult <- paxtoolsr::downloadFile(baseUrl=tmpUrl, fileName=selectedFileName, destDir=destDir, verbose=verbose)
        
        if(!downloadResult) {
            stop("ERROR: File was not found.") 
        }
        
        selectedFilePath <- file.path(destDir, selectedFileName)
    }
    
    if(verbose) {
        cat("selectedFilePath: ", selectedFilePath, "\n")
    }
    
    tmpFile <- R.utils::gunzip(selectedFilePath, remove=FALSE, temporary=TRUE, skip=TRUE)
    
    # READ FUNCTIONS ----
    ## Parse GMT 
    if(grepl("GSEA", selectedFileName) || grepl("gmt", selectedFileName)) {
        results <- readGmt(tmpFile, ...)
        return(results)
    }      
    
    ## Parse EXTENDED_BINARY_SIF (TXT)
    if(grepl("TXT", selectedFileName) || grepl("txt", selectedFileName) || grepl("EXTENDED_BINARY_SIF", selectedFileName)) {
        results <- readSifnx(tmpFile, ...)
        return(results)
    }      
    
    ## Parse BINARY_SIF (SIF)
    if(grepl("SIF", selectedFileName) || grepl("BINARY_SIF", selectedFileName) || grepl("sif", selectedFileName)) {
        results <- readSif(tmpFile, ...)
        return(results)
    }  
    
    ## Parse BIOPAX
    if(grepl("BIOPAX", selectedFileName)) {
        results <- readBiopax(tmpFile, ...)
        return(results)
    }
}
