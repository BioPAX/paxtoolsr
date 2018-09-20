#' Convert Results from readSifnx to data.frame
#' 
#' @param lst a list returned from readSifnx
#' @return a list entries converted to data.frame
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @examples 
#' sifnx <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
#' class(sifnx$edges)
#' 
#' dfSifnx <- convertToDF(sifnx)
#' class(dfSifnx$edges)
#' 
#' dtSifnx <- convertToDT(dfSifnx)
#' class(dtSifnx$edges)
#' 
#' @importFrom data.table setDF
convertToDF <- function(lst) {
    nodes <- lst$nodes
    nodes <- setDF(nodes)
    nodes$PARTICIPANT_NAME <- vapply(nodes$PARTICIPANT_NAME, paste, collapse = ";", character(1L))
    nodes$UNIFICATION_XREF <- vapply(nodes$UNIFICATION_XREF, paste, collapse = ";", character(1L))
    nodes$RELATIONSHIP_XREF <- vapply(nodes$RELATIONSHIP_XREF, paste, collapse = ";", character(1L))
    
    edges <- lst$edges
    edges <- setDF(edges)
    edges$INTERACTION_DATA_SOURCE <- vapply(edges$INTERACTION_DATA_SOURCE, paste, collapse = ";", character(1L))
    edges$INTERACTION_PUBMED_ID <- vapply(edges$INTERACTION_PUBMED_ID, paste, collapse = ";", character(1L))
    edges$PATHWAY_NAMES <- vapply(edges$PATHWAY_NAMES, paste, collapse = ";", character(1L))
    edges$MEDIATOR_IDS <- vapply(edges$MEDIATOR_IDS, paste, collapse = ";", character(1L))
    
    lst$edges <- edges
    lst$nodes <- nodes
    
    return(lst)
}
