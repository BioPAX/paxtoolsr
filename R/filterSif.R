#' Keep interactions in SIF network based on certain criteria
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @param ids a vector of IDs to be kept
#' @param interactionTypes a vector of interaction types to be kept
#'   (List of interaction types: http://www.pathwaycommons.org/pc2/formats)
#' @param dataSources a vector of data sources to be kept. For Extended SIF.
#' @param interactionPubmedId a vector of Pubmed IDs to be kept. For Extended SIF.
#' @param pathwayNames a vector of pathway names to be kept. For Extended SIF.
#' @param mediatorIds a vector of mediator IDs to be kept. For Extended SIF.
#'   Mediator IDs are the full BioPAX objects that were simplified to interaction 
#'   given in the SIF. For Extended SIF.
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
#' results <- filterSif(tmp$edges, ids=c("CHEBI:17640", "MCM3"))
#' results <- filterSif(tmp$edges, dataSources=c("INOH", "KEGG"))
#' results <- filterSif(tmp$edges, dataSources=c("IntAct"), ids=c("CHEBI:17640", "MCM3"))
#' results <- filterSif(tmp$edges, pathwayNames=c("Metabolic pathways"))
#' results <- filterSif(tmp$edges, mediatorIds=c("http://purl.org/pc2/8/MolecularInteraction_1452626895158"))
#' results <- filterSif(tmp$edges, interactionPubmedId="17654400")
#' 
#' 
#' tmp <- readSifnx(system.file("extdata", "test_sifnx_250.txt", package = "paxtoolsr"))
#' edgelist <- read.table(system.file("extdata", "test_edgelist.txt", package = "paxtoolsr"), 
#'   sep="\t", header=FALSE, stringsAsFactors=FALSE)
#' results <- filterSif(tmp$edges, edgelist=edgelist)
#' 
#' @concept paxtoolsr
#' @export
filterSif <- function(sif, ids=NULL, interactionTypes=NULL, dataSources=NULL, interactionPubmedIds=NULL, pathwayNames=NULL, mediatorIds=NULL, edgelist=NULL, verbose=FALSE) {
    idxList <- NULL
    
    if(!is.null(ids)) {
        aIdx <- which(sif$PARTICIPANT_A %in% ids) 
        bIdx <- which(sif$PARTICIPANT_B %in% ids) 
        
        idxIds <- unique(c(aIdx, bIdx))
        
        #cat("II: ", paste(idxIds, collapse=","), "\n")
        idxList[["idxIds"]] <- idxIds
    } 
    
    if(!is.null(interactionTypes)) {
        idxInteractionTypes <- which(sif$INTERACTION_TYPE %in% interactionTypes) 
        
        #cat("IIT: ", paste(idxInteractionTypes, collapse=","), "\n")
        idxList[["idxInteractionTypes"]] <- idxInteractionTypes
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
    
    if(!is.null(interactionPubmedIds)) {
        idxInteractionPubmedId <- which(sif$INTERACTION_PUBMED_ID %in% interactionPubmedIds) 
        
        #cat("IIT: ", paste(idxInteractionPubmedId, collapse=","), "\n")
        idxList[["idxInteractionPubmedId"]] <- idxInteractionPubmedId
    }
    
    if(!is.null(pathwayNames)) {
        idxPathwayNames <- which(sif$PATHWAY_NAMES %in% pathwayNames) 
        
        #cat("IIT: ", paste(idxPathwayNames, collapse=","), "\n")
        idxList[["idxPathwayNames"]] <- idxPathwayNames
    } 
    
    if(!is.null(mediatorIds)) {
        results <- searchListOfVectors(mediatorIds, sif$MEDIATOR_IDS)
        
        idxMediatorIds <- unique(unlist(results))
        
        #cat("IIT: ", paste(idxMediatorIds, collapse=","), "\n")
        idxList[["idxMediatorIds"]] <- idxMediatorIds
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

    idx <- Reduce(intersect, idxList)
    
    filteredNetwork <- sif[idx, ]
    
    return(filteredNetwork)
}
