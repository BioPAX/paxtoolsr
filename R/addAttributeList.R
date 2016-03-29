#' Add attributes using a list of vectors to an igraph object
#' 
#' @param g an igraph object
#' @param attr the name of the attribute
#' @param the list of vectors 
#' 
#' @return the modified igraph object
#' 
#' @concept paxtoolsr
#' @export
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
