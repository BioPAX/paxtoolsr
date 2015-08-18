#' Summarize a SIF Network 
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @return a list containing a count of the unique genes in the SIF and counts for the interaction types in the network
#' 
#' @examples
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' summarizeSif(results)
#' 
#' @concept paxtoolsr
#' @export
summarizeSif <- function(sif) {
    uniqueNodes <- length(unique(c(sif[,1], sif[,3])))
    interactionTypeFreq <- table(sif[,2])
    
    results <- list(uniqueNodes=uniqueNodes, totalInteractions=nrow(sif), interactionTypeFreq=interactionTypeFreq)
    
    return(results) 
}  
