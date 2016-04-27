#' Load SIF as igraph Network
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @param directed a boolean weather the returned graph should be directed (DEFAULT: TRUE)  
#'   
#' @return a directed igraph network with interaction types 
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' g <- loadSifInIgraph(results)
#' 
#' @details Users are likely to run into issues if the input SIF has factor levels
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom igraph graph.edgelist E E<- set_edge_attr
#' @importFrom data.table setDF
loadSifInIgraph <- function(sif, directed=TRUE) {
    if("data.table" %in% class(sif)) {
        setDF(sif)
    }

    # Handle SIF undirected reactions 
    tmpSifUndirected <- sif[which(sif$INTERACTION_TYPE %in% "in-complex-with"),]
    a <- tmpSifUndirected$PARTICIPANT_A
    b <- tmpSifUndirected$PARTICIPANT_B 
    tmpSifUndirected$PARTICIPANT_A <- b
    tmpSifUndirected$PARTICIPANT_B <- a
    sif <- rbind(sif, tmpSifUndirected)

    # Convert to igraph 
    tmpSif <- sif[, c("PARTICIPANT_A", "PARTICIPANT_B")]
    g <- graph.edgelist(as.matrix(tmpSif), directed=directed)
    g <- set_edge_attr(g, "interactionType", index=E(g), sif[, "INTERACTION_TYPE"])
    
    interactionDataSourceIdx <- which("INTERACTION_DATA_SOURCE" == colnames(sif))
    
    if(length(interactionDataSourceIdx) == 1) {
        g <- set_edge_attr(g, "interactionDataSource", index=E(g), sif[, interactionDataSourceIdx])
    }
    
    interactionPubmedIdIdx <- which("INTERACTION_PUBMED_ID" == colnames(sif))
    
    if(length(interactionPubmedIdIdx) == 1) {
        g <- set_edge_attr(g, "interactionPubmedId", index=E(g), sif[, interactionPubmedIdIdx])
    }
    
    pathwayIdx <- which("PATHWAY_NAMES" == colnames(sif))
    
    if(length(pathwayIdx) == 1) {
        g <- set_edge_attr(g, "pathwayNames", index=E(g), sif[, pathwayIdx])
    }
    
    mediatorIdsIdx <- which("MEDIATOR_IDS" == colnames(sif))
    
    if(length(mediatorIdsIdx) == 1) {
        g <- set_edge_attr(g, "mediatorIds", index=E(g), sif[, mediatorIdsIdx])
    }
    
    return(g)
}
