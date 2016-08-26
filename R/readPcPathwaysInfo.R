#' Read in Pathway Commons Pathways Information 
#' 
#' @param inputFile an inputFile
#' 
#' @return a list with two items: 1) pathwayChildren and 2) pathwayInfo
#' 
#' @details This file is generally found as pathways.txt.gz (e.g. 
#' http://www.pathwaycommons.org/archives/PC2/current/pathways.txt.gz)
#' 
#' @examples 
#' #results <- readPcPathwaysInfo(INPUTFILE)
#' 
#' @concept paxtoolsr
#' @export
readPcPathwaysInfo <- function(inputFile) {
    if(!file.exists(inputFile)) {
        stop("ERROR: inputFile not file.")
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
    for (i in 2:length(lineTmp)) {
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

    results <- list(pathwayInfo=pathwayInfo, 
                    pathwayChildren=pathwayChildren)
    
    return(results)    
}
