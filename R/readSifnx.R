#' Read in a Extended SIF file 
#' 
#' @param inputFile an inputFile
#' @param asDT TODO
#' 
#' @return a list with nodes and edges entries 
#' 
#' @details SIFNX files from Pathway Commons commonly come a single file that 
#' includes a tab-delimited sections for nodes and another for edges. The 
#' sections are separated by an empty lines. These sections must be split before
#' they are read. 
#' 
#' @examples 
#' results <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
#' chebiIds <- lapply(results$nodesUniXref, function(x) { x[which(grepl("CHEBI", x))] })
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom data.table fread
readSifnx <- function(inputFile, asDT=TRUE) {
    if(!file.exists(inputFile)) {
        stop("ERROR: inputFile not file.")
    }
    
    # Files with small sizes will confuse fread to think there are fewer columns 
    # in the edges because it scans the 5th row to determine number of columns. 
    # Two methods of reading are therefore necessary. 
    tmp <- file.info(inputFile)
    
    if(tmp$size < 100000) {
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
    } else {
        # A warning on discarded content is expected because of the 2-files in 1 nature of the file
        suppressWarnings(tmp <- fread(inputFile, sep="\n", header=FALSE, stringsAsFactors=FALSE))
        nodes <- fread(inputFile, sep="\t", header=TRUE, stringsAsFactors=FALSE, skip="PARTICIPANT\tPARTICIPANT_TYPE", 
                       data.table=FALSE)
        
        tmp2 <- paste(tmp$V1, collapse="\n")
        edges <- fread(tmp2, sep="\t", header=TRUE, stringsAsFactors=FALSE,  data.table=FALSE)
    }

    results <- list(nodes=nodes, 
                    edges=edges)
    
    if(asDT) {
        results <- convertToDT(results)
        return(results)    
    } else {
        return(results)    
    }

#     # EDGES
#     edgesInteractionDataSource <- strsplit(as.character(edges$INTERACTION_DATA_SOURCE), ";")
#     edgesPubmedId <- strsplit(as.character(edges$INTERACTION_PUBMED_ID), ";")
#     edgesPathwayNames <- strsplit(as.character(edges$PATHWAY_NAMES), ";")
#     
#     # NODES    
#     nodesUniXref <- strsplit(as.character(nodes$UNIFICATION_XREF), ";")
#     names(nodesUniXref) <- nodes$PARTICIPANT
#     
#     nodesRelXref <- strsplit(as.character(nodes$RELATIONSHIP_XREF), ";")
#     names(nodesRelXref) <- nodes$PARTICIPANT
#     
#     nodesType <- nodes$PARTICIPANT_TYPE
#     names(nodesType) <- nodes$PARTICIPANT
#     
#     nodesName <- strsplit(as.character(nodes$PARTICIPANT_NAME), ";")
#     names(nodesName) <- nodes$PARTICIPANT
}
