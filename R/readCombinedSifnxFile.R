#' Read a combined file with nodes and edges 
#' 
#' @param inputFile
#' 
#' @return a list of nodes and edges 
#' 
#' @concept paxtoolsr
#' @export
readCombinedSifnxFile <- function(inputFile) {
    edgesFile <- tempfile("edges", fileext=".txt")
    nodesFile <- tempfile("nodes", fileext=".txt")
    
    # Open file connections
    edgesCon <- file(edgesFile, "w")
    nodesCon <- file(nodesFile, "w")
    
    con <- file(inputFile)
    
    newLineFlag <- FALSE
    
    # Read single lines
    lineTmp <- readLines(con, warn=FALSE)
    
    for (i in 1:length(lineTmp)) {
        line <- lineTmp[i]
        
        if(grepl("^$", line)) {
            newLineFlag <- TRUE
            next
        }
        
        if(!newLineFlag) {
            writeLines(line, edgesCon)
        } else {
            writeLines(line, nodesCon)        
        }
    }
    
    close(edgesCon)
    close(nodesCon)
    close(con)
    
    edges <- read.table(edgesFile, header=TRUE, sep="\t", quote="", 
                        stringsAsFactors=FALSE, fill=TRUE)
    nodes <- read.table(nodesFile, header=TRUE, sep="\t", quote="", 
                        stringsAsFactors=FALSE, fill=TRUE)   
    
    results <- list(nodes=nodes, 
                    edges=edges)
    
    return(results)
}


 
