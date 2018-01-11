#' Read in Pathway Commons Pathways Information 
#' 
#' @param inputFile an inputFile; if NULL then retrieve the current pathways.txt; see details (default: NULL)
#' 
#' @return a data.table 
#' 
#' @details This file is generally found as pathways.txt.gz (e.g. 
#' http://www.pathwaycommons.org/archives/PC2/current/pathways.txt.gz)
#' 
#' @examples 
#' results <- readPcPathwaysInfo(system.file("extdata", "pathways.txt.gz", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readPcPathwaysInfo <- function(inputFile=NULL) {
    if(is.null(inputFile)) {
        url <- "http://www.pathwaycommons.org/archives/PC2/current/"
        fileName <- "pathways.txt.gz"
        
        downloadFile(url, fileName)
        
        inputFile <- file.path(Sys.getenv("PAXTOOLSR_CACHE"), fileName)
    } 
    
    if(!file.exists(inputFile)) {
        stop("ERROR: inputFile was not found")
    } 
    
    pathwayChildrenFile <- tempfile("pathwayChildren", fileext=".txt")
    pathwayInfoFile <- tempfile("pathwayInfo", fileext=".txt")
    
    # Open file connections
    pathwayChildrenCon <- file(pathwayChildrenFile, "w")
    pathwayInfoCon <- file(pathwayInfoFile, "w")
    
    if(grepl(".gz$", inputFile)) {
        con <- gzfile(inputFile)
    } else {
        con <- file(inputFile)
    }
    
    newLineFlag <- FALSE
    
    # Read single lines
    lineTmp <- readLines(con, warn=FALSE)
    
    # Skip first line because it is empty
    for (i in 1:length(lineTmp)) {
        line <- lineTmp[i]
        
        if(grepl("^$", line)) {
            newLineFlag <- TRUE
            next
        }
        
        if(!newLineFlag) {
            writeLines(line, pathwayChildrenCon)
        } else {
            writeLines(line, pathwayInfoCon)        
        }
    }
    
    close(pathwayChildrenCon)
    close(pathwayInfoCon)
    close(con)
    
    pathwayChildren <- read.table(pathwayChildrenFile, header=TRUE, sep="\t", quote="", 
                        stringsAsFactors=FALSE, fill=TRUE)
    pathwayInfo <- read.table(pathwayInfoFile, header=TRUE, sep="\t", quote="", 
                        stringsAsFactors=FALSE, fill=TRUE)        
    
    results <- merge(pathwayChildren, pathwayInfo, by=c("PATHWAY_URI", "DISPLAY_NAME"))

    results <- setDT(results)
    results$DIRECT_SUB_PATHWAY_URIS <- strsplit(results$DIRECT_SUB_PATHWAY_URIS, ";")
    results$ALL_SUB_PATHWAY_URIS <- strsplit(results$ALL_SUB_PATHWAY_URIS, ";")
    results$ALL_NAMES <- strsplit(results$ALL_NAMES, ";")
    
    results$DATASOURCE <- tolower(results$DATASOURCE)
    
    return(results)
}
