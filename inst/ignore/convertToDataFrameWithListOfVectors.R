#' Convert Results from readSifnx to data.frame
#' 
#' @param lst a list returned from readSifnx
#' @return a list entries converted to data.frame
#' 
#' @details The SIFNX format is an evolving format. Older datasets may not have 
#'  all the columns this function expects. In these cases, the columns will be 
#'  added with all NULL entries. 
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @examples 
#' lst <- readSifnx(system.file("extdata", "test_sifnx_250.txt", package="paxtoolsr"))
#' newSifnx <- convertToDataFrameWithListOfVectors(lst)
convertToDataFrameWithListOfVectors <- function(lst) {
    nodes <- lst$nodes
    nodes <- as.data.frame(nodesOrg)
    for(col in colnames(nodes)) {
        nodes[, col] <- I(strsplit(nodes[, col], ";"))
    }
    
    nodes$UNIFICATION_XREF <- I(strsplit(nodes$UNIFICATION_XREF, ";"))
    
    edges <- lst$edges
    for(col in colnames(edges)) {
        edges[, col] <- I(strsplit(edges[, col], ";"))
    }
    
    lst$edges <- edges
    lst$nodes <- nodes
    
    return(lst)
}
