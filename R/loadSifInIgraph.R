#' Load SIF as igraph Network
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#'   
#' @return a directed igraph network with interaction types 
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' g <- loadSifInIgraph(results)
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom igraph graph.edgelist E E<-
loadSifInIgraph <- function(sif) {
    gWithType <- graph.edgelist(as.matrix(sif[, c("PARTICIPANT_A", "PARTICIPANT_B")]), directed=TRUE)
    E(gWithType)$edgeType <- sif[, "INTERACTION_TYPE"]

    pathwayIdx <- which("PATHWAY_NAMES" == colnames(sif))

    if(length(pathwayIdx) == 1) {
        E(gWithType)$pathwayNames <- sif[, pathwayIdx]
    }

    interactionDataSourceIdx <- which("INTERACTION_DATA_SOURCE" == colnames(sif))
    
    if(length(pathwayIdx) == 1) {
        E(gWithType)$interactionDataSource <- sif[, interactionDataSourceIdx]
    }
    
    return(gWithType)
}
