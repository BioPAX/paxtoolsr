#' Get a list of categories of SIF interactions
#' 
#' @return a list of interactions in categories
#' 
#' @details 
#' Description of interaction types: http://www.pathwaycommons.org/pc2/formats
#' Categories provided: 
#'   BetweenProteins, 
#'   BetweenProteinsOther (often from high-throughput experiments),
#'   BetweenProteinSmallMolecule, 
#'   BetweenSmallMolecules
#' 
#' @examples 
#' sifCat <- getSifInteractionCategories()
#' sifCat[["BetweenProteins"]]
#' 
#' @concept paxtoolsr
#' @export
getSifInteractionCategories <- function() {
    protInt <- c("controls-state-change-of", "controls-expression-of", 
                 "controls-degradation-of", "controls-transport-of",
                 "catalysis-precedes", "in-complex-with")
    
    protOtherInt <- c("interacts-with", "neighbor-of")
    
    protSmMolInt <- c("consumption-controlled-by", "controls-production-of",
                      "controls-transport-of-chemical", "chemical-affects")
    
    smMolInt <- c("reacts-with", "used-to-produce")
    
    return(list("BetweenProteins"=protInt, 
                "BetweenProteinsOther"=protOtherInt,
                "BetweenProteinSmallMolecule"=protSmMolInt, 
                "BetweenSmallMolecules"=smMolInt))
}
