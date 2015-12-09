#' Convert SIF Interaction Types to SPIA types
#' 
#' @param edges a data.frame of interactions; must have INTERACTION_TYPE column
#' 
#' @return the edges data.frame with the converted interaction types
#' 
#' @concept paxtoolsr
#' @export
convertSifToSpia <- function(edges) {
    sifType <- c("controls-state-change-of", 
                 "controls-expression-of",
                 "controls-degradation-of",
                 "controls-transport-of",
                 "catalysis-precedes",
                 "in-complex-with")
    
    spiaType <- c("process(phosphorylation)", 
                  "process(expression)", 
                  "process(indirect effect)", 
                  "process(indirect effect)", 
                  "process(indirect effect)",
                  "process(binding/association)")

    tmp <- cbind(sifType, spiaType)
    spiaConv <- as.data.frame(tmp, stringsAsFactors=FALSE)
    
    for(i in 1:nrow(spiaConv)) {
        idx <- edges[,"INTERACTION_TYPE"] == spiaConv[i, "sifType"]
        edges[idx, "INTERACTION_TYPE"] <- spiaConv[i, "spiaType"]
    }
    
    return(edges)
}
