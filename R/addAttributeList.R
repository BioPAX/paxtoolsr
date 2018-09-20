#' Add attributes using a list of vectors to an igraph object
#' 
#' @param g an igraph object
#' @param attr the name of the attribute
#' @param l the list of vectors 
#' 
#' @return the modified igraph object
#' 
#' @examples 
#' library(igraph)
#' g <- barabasi.game(20)
#' g <- set_vertex_attr(g, "name", value=LETTERS[1:20])
#' g <- addAttributeList(g, "isProt", list(A=TRUE, B=FALSE, C=TRUE, D=TRUE, E=FALSE))
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom igraph V V<- set_vertex_attr 
addAttributeList <- function(g, attr, l) {
    for(i in 1:length(l)) {
        #cat("i", i, "\n")
        vertex <- V(g)$name[V(g)$name %in% names(l)[i]]
        
        # Check if vertex exists 
        if(length(vertex) == 1) {
            idx <- match(vertex, V(g)$name)
            #cat("attr", attr, "idx", idx, " v ", vertex, "\n")
            
            g <- set_vertex_attr(g, attr, idx, l[[vertex]])
        } 
    }
    
    return(g)
}
