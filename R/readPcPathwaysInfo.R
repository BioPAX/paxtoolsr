#' Read in Pathway Commons Pathways Information 
#' 
#' @param inputFile an inputFile; if NULL then retrieve the current pathways.txt; see details (default: NULL)
#' @param version a version number for a previous version of Pathway Commons data; 
#'   versions 3 and above. Parameter set as version="8". Available versions "http://www.pathwaycommons.org/archives/PC2/"
#' 
#' @return a data.frame
#' 
#' @details This file is generally found as pathways.txt.gz (e.g. 
#' http://www.pathwaycommons.org/archives/PC2/current/pathways.txt.gz)
#' 
#' @examples 
#' results <- readPcPathwaysInfo(system.file("extdata", "pathways.txt.gz", package="paxtoolsr"), version="8")
#' 
#' @concept paxtoolsr
#' @export
readPcPathwaysInfo <- function(inputFile=NULL, version=NULL) {
    if(is.null(inputFile) && is.null(version)) {
        stop("ERROR: Either inputFile or version must be specified")
    }
    
    if(is.null(inputFile) && !is.null(version)) {
        url <- paste0("http://www.pathwaycommons.org/archives/PC2/v", version, "/")
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
    
    tmpResults <- merge(pathwayChildren, pathwayInfo, by=c("PATHWAY_URI", "DISPLAY_NAME"))

    tmpResults$DIRECT_SUB_PATHWAY_URIS <- I(strsplit(tmpResults$DIRECT_SUB_PATHWAY_URIS, ";"))
    tmpResults$ALL_SUB_PATHWAY_URIS <- I(strsplit(tmpResults$ALL_SUB_PATHWAY_URIS, ";"))
    tmpResults$ALL_NAMES <- I(strsplit(tmpResults$ALL_NAMES, ";"))
    
    tmpResults$DATASOURCE <- tolower(tmpResults$DATASOURCE)
    
    # Add a column that has all the sub-pathway names 
    pathwayNames <- list() 
    
    for(i in 1:nrow(tmpResults)) {
        t1 <- tmpResults[i, "ALL_SUB_PATHWAY_URIS"][[1]]
        
        subPathwayNames <- NULL 
        
        for(j in 1:length(t1)) {
            idx <- which(tmpResults$PATHWAY_URI == t1[j])
            subPathwayNames <- c(subPathwayNames, tmpResults$DISPLAY_NAME[idx])
        }  
        
        pathwayNames[[i]] <- subPathwayNames
    }
    
    results <- data.frame(tmpResults, ALL_SUB_PATHWAY_NAMES=I(pathwayNames), stringsAsFactors = FALSE)
    
    return(results)
}
