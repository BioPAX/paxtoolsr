#' Convert SIF to GMT 
#' 
#' @param sif a data.frame representing a SIF (Simple Interaction Format)
#' @param name the name of the gene set
#' @param returnSmallMolecules a boolean whether to return genes or small molecules in the gene set
#' 
#' @return a list with one entry being a vector 
#' 
#' @concept paxtoolsr
#' @export
convertSifToGmt <- function(sif, name="gmt", returnSmallMolecules=FALSE) {
    ids <- unique(sif$PARTICIPANT_A, sif$PARTICIPANT_B)
    
    if(returnSmallMolecules) {
        idx <- grepl("^CHEBI", ids)
    } else {
        idx <- !grepl("^CHEBI", ids)
    }
    
    results <- list()
    results[[name]] <- ids[idx]
    
    return(results)
}
