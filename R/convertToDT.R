#' Convert Results from readSifnx to data.table
#' 
#' @param lst a list returned from readSifnx
#' @return a list entries converted to data.table
#' 
#' @details The SIFNX format is an evolving format. Older datasets may not have 
#'  all the columns this function expects. In these cases, the columns will be 
#'  added with all NULL entries. 
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom data.table setDT
convertToDT <- function(lst) {
    nodes <- lst$nodes
    nodes <- setDT(nodes)
    nodes$PARTICIPANT_NAME <- strsplit(nodes$PARTICIPANT_NAME, ";")
    nodes$UNIFICATION_XREF <- strsplit(nodes$UNIFICATION_XREF, ";")
    nodes$RELATIONSHIP_XREF <- strsplit(nodes$RELATIONSHIP_XREF, ";")
    
    edges <- lst$edges
    edges <- setDT(edges)
    edges$INTERACTION_DATA_SOURCE <- strsplit(edges$INTERACTION_DATA_SOURCE, ";")
    edges$INTERACTION_PUBMED_ID <- strsplit(as.character(edges$INTERACTION_PUBMED_ID), ";")
    edges$PATHWAY_NAMES <- strsplit(as.character(edges$PATHWAY_NAMES), ";")  
    edges$MEDIATOR_IDS <- strsplit(as.character(edges$MEDIATOR_IDS), ";") 
    
    lst$edges <- edges
    lst$nodes <- nodes
    
    return(lst)
}
