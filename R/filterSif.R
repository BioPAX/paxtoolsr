#' Keep interactions in SIF network of certain interaction types
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @param interactionTypes a vector of interaction types 
#'   (List of interaction types: http://www.pathwaycommons.org/pc2/formats)
#'   
#' @return a data.frame of filtered interactions with three columns: "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' intTypes <- c("controls-state-change-of", "controls-expression-of", "catalysis-precedes")
#' filteredNetwork <- filterSif(results, intTypes)
#' 
#' @concept paxtoolsr
#' @export
filterSif <- function(sif, interactionTypes) {
    idx <- which(sif$INTERACTION_TYPE %in% interactionTypes) 
    filteredNetwork <- sif[idx, ]
    
    return(filteredNetwork)
}
