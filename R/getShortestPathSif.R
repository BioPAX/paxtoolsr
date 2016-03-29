#' Get the shortest between two IDs (HGNC or CHEBI)
#' 
#' @param sif a SIF network 
#' @param idA HGNC or CHEBI (CHEBI:XXXXX) ID
#' @param idB HGNC or CHEBI (CHEBI:XXXXX) ID
#' 
#' @return a data.frame representing a SIF network
#' 
#' @concept paxtoolsr
#' @export
getShortestPathSif <- function(g, idA, idB, mode=c("all", "out", "in"), weights=NA) {
    #idA <- "CCND1"
    #idB <- "MAZ"
    
    #g <- loadSifInIgraph(sif)
    
    aIdx <- match(idA, V(g)$name)
    bIdx <- match(idB, V(g)$name)    
    
    if(!is.na(aIdx) && !is.na(bIdx)) {
        s1 <- shortest_paths(g, aIdx, bIdx, output="epath", mode=mode, weights=weights)
        cat("PATH: ", length(s1), "\n")
        s2 <- all_shortest_paths(g, aIdx, bIdx, output="epath", mode=mode)
        cat("ALL PATHS: ", length(s2), "\n")
        #s1$epath[[1]]    
    } else {
        stop("ERROR: aIdx", aIdx, " bIdx: ", bIdx, "\n")
    }
    
    results <- data.frame(PARTICIPANT_A=character(), 
                          INTERACTION_TYPE=character(), 
                          PARTICIPANT_B=character(), 
                          INTERACTION_DATA_SOURCE=character(), 
                          INTERACTION_PUBMED_ID=character(), 
                          PATHWAY_NAMES=character(),
                          stringsAsFactors=FALSE)
    
    for(i in 1:length(s1$epath[[1]])) {
        idx <- s1$epath[[1]][i]
        tmpV <- ends(g, idx)
        tmpInteractionType <- E(g)[idx]$interactionType
        
        if("interactionPubmedId" %in% list.edge.attributes(g)) {
            tmpInteractionPubmedId <- lapply(E(g)[idx]$interactionPubmedId, paste, collapse=";")[[1]]
        }
        
        if("pathwayNames" %in% list.edge.attributes(g)) {
            tmpPathwayNames <- lapply(E(g)[idx]$pathwayNames, paste, collapse=";")[[1]]
        }

        if("interactionDataSource" %in% list.edge.attributes(g)) {
            tmpInteractionDataSource <- lapply(E(g)[idx]$interactionDataSource, paste, collapse=";")[[1]]
        }
        
        if("mediatorIds" %in% list.edge.attributes(g)) {
            tmpMediatorIds <- lapply(E(g)[idx]$mediatorIds, paste, collapse=";")[[1]]
        }
        
        tmpResults <- data.frame(PARTICIPANT_A=tmpV[1, 1], 
                                 INTERACTION_TYPE=tmpInteractionType, 
                                 PARTICIPANT_B=tmpV[1, 2], 
                                 INTERACTION_DATA_SOURCE=tmpInteractionDataSource, 
                                 INTERACTION_PUBMED_ID=tmpInteractionPubmedId, 
                                 PATHWAY_NAMES=tmpPathwayNames,
                                 MEDIATOR_IDS=tmpMediatorIds, 
                                 stringsAsFactors=FALSE)
        
        results <- rbind(results, tmpResults)
    }
    
    return(results)
}
