#' Keep interactions in SIF network based on certain criteria
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @param interactionTypes a vector of interaction types to be kept
#'   (List of interaction types: http://www.pathwaycommons.org/pc2/formats)
#' @param dataSources a vector of data sources to be kept
#' @param ids a vector of IDs to be kept
#' @param edgelist a two-column data.frame where each row is an interaction to be kept.
#'   Directionality is ignored (e.g. Edge A B will return interactions A B and B A from SIF)
#'   
#' @return filtered interactions with three columns: "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B". 
#'   The intersection of multiple filters is returned. The return class is the same as the input: 
#'   data.frame or data.table
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' intTypes <- c("controls-state-change-of", "controls-expression-of", "catalysis-precedes")
#' filteredNetwork <- filterSif(results, intTypes)
#' 
#' tmp <- readSifnx(system.file("extdata", "test_sifnx_250.txt", package = "paxtoolsr"))
#' results <- filterSif(tmp$edges, dataSources=c("INOH", "KEGG"))
#' results <- filterSif(tmp$edges, ids=c("CHEBI:17640", "MCM3"))
#' results <- filterSif(tmp$edges, dataSources=c("IntAct"), ids=c("CHEBI:17640", "MCM3"))
#' 
#' tmp <- readSifnx(system.file("extdata", "test_sifnx_250.txt", package = "paxtoolsr"))
#' edgelist <- read.table(system.file("extdata", "test_edgelist.txt", package = "paxtoolsr"), 
#'   sep="\t", header=FALSE, stringsAsFactors=FALSE)
#' results <- filterSif(tmp$edges, edgelist=edgelist)
#' 
#' @concept paxtoolsr
#' @export
filterSif <- function(sif, interactionTypes=NULL, dataSources=NULL, ids=NULL, edgelist=NULL) {
    idxList <- NULL
    
    if(!is.null(ids)) {
        aIdx <- which(sif$PARTICIPANT_A %in% ids) 
        bIdx <- which(sif$PARTICIPANT_B %in% ids) 
        
        idxIds <- unique(c(aIdx, bIdx))
        
        #cat("II: ", paste(idxIds, collapse=","), "\n")
        idxList[["idxIds"]] <- idxIds
    } 
    
    if(!is.null(edgelist)) {
        aIdx <- which(sif$PARTICIPANT_A %in% edgelist[,1]) 
        bIdx <- which(sif$PARTICIPANT_B %in% edgelist[,2]) 
        idxEdgelist1 <- intersect(aIdx, bIdx)
        
        # Same in reverse
        aIdx <- which(sif$PARTICIPANT_A %in% edgelist[,2]) 
        bIdx <- which(sif$PARTICIPANT_B %in% edgelist[,1]) 
        idxEdgelist2 <- intersect(aIdx, bIdx)
        
        idxEdgelist <- c(idxEdgelist1, idxEdgelist2)
        
        #cat("II: ", paste(idxIds, collapse=","), "\n")
        idxList[["idxEdgelist"]] <- idxEdgelist
    } 
    
    if(!is.null(dataSources)) {
        if(!("data.table" %in% class(sif))) {
            stop("ERROR: SIF must be must be a data.table. SUGGESTION: Use convertToDT")
        }
        
        results <- searchListOfVectors(dataSources, sif$INTERACTION_DATA_SOURCE)
        
        idxDataSources <- unique(unlist(results))
        
        #cat("IDS: ", paste(idxDataSources, collapse=","), "\n")
        idxList[["idxDataSources"]] <- idxDataSources
    } 
    
    if(!is.null(interactionTypes)) {
        idxInteractionTypes <- which(sif$INTERACTION_TYPE %in% interactionTypes) 
        
        #cat("IIT: ", paste(idxInteractionTypes, collapse=","), "\n")
        idxList[["idxInteractionTypes"]] <- idxInteractionTypes
    } 

    idx <- Reduce(intersect, idxList)
    
    filteredNetwork <- sif[idx, ]
    
    return(filteredNetwork)
}
