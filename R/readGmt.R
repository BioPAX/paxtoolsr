#' Read in gene sets from GMT files 
#'
#' This function will read in gene sets in the GMT format into a named list. 
#' 
#' @param inputFile an inputFile
#' @param removePrefix Pathway Commons genesets are prefixed with a NCBI organism taxonomy number (e.g. 9606 for humans); this is a boolean whether to remove the prefix (default: FALSE)
#' @param returnInfo a boolean whether to return information on genesets; these results are returned a list of two items: 1) basic GMT results and 2) datasource, organism, and id type information for each gene set (default: FALSE)
#' 
#' @return a named list where each entry corresponds to a gene set or a list described in the returnInfo parameter
#' 
#' @examples 
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"))
#' results <- readGmt(system.file("extdata", "test_gsea_1.gmt", package="paxtoolsr"))
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"), removePrefix=TRUE)
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"), returnInfo=TRUE)
#' 
#' @concept paxtoolsr
#' @export
readGmt <- function(inputFile, removePrefix=FALSE, returnInfo=FALSE) {
    if(!file.exists(inputFile)) {
        stop("ERROR: inputFile not file.")
    }
    
    fileContents <- readLines(inputFile)
    
    # NOTE: Removing empty strings is necessary
    fileContents <- fileContents[fileContents != ""]
    
    tmpResults <- sapply(fileContents, function(x) { 
      tmp <- unlist(strsplit(x, "\t", fixed = TRUE)) 
    })
    
    if(class(tmpResults) == "matrix") {
        t2 <- tmpResults
        tmpResults <- list() 
        tmpResults[[t2[1]]] <- as.vector(t2)
    }
    
    names(tmpResults) <- sapply(tmpResults, function(x) {
        if(removePrefix) {
            t1 <- strsplit(x, ": ")[[1]]
            t2 <- paste(t1[2:length(t1)], collapse=": ")
            results <- trimws(t2)
        } else {
            results <- x[1]
        }
        
        return(results)
    })
    
    if(returnInfo) {
        results <- lapply(tmpResults, function(x) {
            t1 <- trimws(strsplit(x[2], ";")[[1]])
            tmp <- strsplit(t1, ": ")
            
            dataSource <- tmp[[1]][2]
            organism <- tmp[[2]][2]
            idType <- tmp[[3]][2]
            geneSet <- x[-(1:2)]
            
            results <- list(geneSet=geneSet, dataSource=dataSource, organism=organism, idType=idType)
            
            return(results)
        })
    } else {
        # Remove the first two entries, i.e. the name and description
        results <- lapply(tmpResults, function(x) x[-(1:2)])
    }
    
    return(results)
}
