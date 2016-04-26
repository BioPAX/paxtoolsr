#' Get the shortest between two IDs (HGNC or CHEBI)
#' 
#' @param sif a SIF network 
#' @param idA HGNC or CHEBI (CHEBI:XXXXX) ID
#' @param idB HGNC or CHEBI (CHEBI:XXXXX) ID
#' @param mode see shortest_paths() in igraph
#' @param weights see shortest_paths() in igraph
#' @param filterFun a function to filter multiple paths of the same length
#' @param ... additional arguments passed on to filterFun
#' 
#' @return a data.frame representing a SIF network
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom igraph V V<- all_shortest_paths are.connected ends list.edge.attributes
getShortestPathSif <- function(sif, idA, idB, mode=c("all", "out", "in"), weights=NULL, filterFun, ...) {
    #idA <- "CCND1"
    #idB <- "MAZ"
    
    g <- loadSifInIgraph(sif)
    
    aIdx <- match(idA, V(g)$name)
    bIdx <- match(idB, V(g)$name)    
    
    if(!is.na(aIdx) && !is.na(bIdx)) {
        # s1 <- shortest_paths(g, aIdx, bIdx, output="epath", mode=mode, weights=weights)
        # cat("PATH: ", length(s1), "\n")
        s2 <- all_shortest_paths(g, aIdx, bIdx, mode=mode, weights=weights)
        cat("ALL PATHS: ", length(s2$res), "\n")
        #s1$epath[[1]]    
    } else {
        stop("ERROR: aIdx", aIdx, " bIdx: ", bIdx, "\n")
    }
    
    s1 <- filterFun(s2$res, ...)
    v1 <- s1$name
    e1 <- NULL 
    
    for(i in 1:(length(v1)-1)) {
        if(are.connected(g, v1[i], v1[i+1])) {
            r1 <- E(g, P=c(v1[i], v1[i+1]))
        } else {
            r1 <- E(g, P=c(v1[i+1], v1[i]))
        }
        
        e1 <- c(e1, r1)
    }
    
    #E(g)[e1]
    #E(g)[e1]$interactionType
    
    epath <- E(g)[e1]
    
    results <- data.frame(PARTICIPANT_A=character(), 
                          INTERACTION_TYPE=character(), 
                          PARTICIPANT_B=character(), 
                          INTERACTION_DATA_SOURCE=character(), 
                          INTERACTION_PUBMED_ID=character(), 
                          PATHWAY_NAMES=character(),
                          stringsAsFactors=FALSE)
    
    for(i in 1:length(epath)) {
        idx <- epath[i]
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
