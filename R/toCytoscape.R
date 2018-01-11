#' Convert igraph to Cytoscape JSON
#' 
#' @param igraphobj an igraph object 
#' 
#' @note From https://github.com/idekerlab/cy-rest-R/blob/17f748426bb5e48ba4075b9d97318ad582b250da/utility/cytoscape_util.R
#' 
#' @return a JSON object
#' 
#' @examples 
#' library(igraph)
#' g <- barabasi.game(20) 
#' json <- toCytoscape(g)
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom jsonlite toJSON
#' @importFrom igraph graph.attributes list.vertex.attributes vertex.attributes V get.edgelist ecount edge.attributes
toCytoscape <- function (igraphobj) {
    # Extract graph attributes
    graph_attr = graph.attributes(igraphobj)
    
    # Extract nodes
    node_count = length(V(igraphobj))
    if('name' %in% list.vertex.attributes(igraphobj)) {
        V(igraphobj)$id <- V(igraphobj)$name
    } else {
        V(igraphobj)$id <- as.character(c(1:node_count))
    }
    
    nodes <- V(igraphobj)
    v_attr = vertex.attributes(igraphobj)
    v_names = list.vertex.attributes(igraphobj)
    
    nds <- array(0, dim=c(node_count))
    for(i in 1:node_count) {
        if(i %% 1000 == 0) {
            print(i)
        }
        nds[[i]] = list(data = mapAttributes(v_names, v_attr, i))
    }
    
    edges <- get.edgelist(igraphobj)
    edge_count = ecount(igraphobj)
    e_attr <- edge.attributes(igraphobj)
    e_names = list.edge.attributes(igraphobj)
    
    attr_exists = FALSE
    e_names_len = 0
    if(identical(e_names, character(0)) == FALSE) {
        attr_exists = TRUE
        e_names_len = length(e_names)
    }
    e_names_len <- length(e_names)
    
    eds <- array(0, dim=c(edge_count))
    for(i in 1:edge_count) {
        st = list(source=toString(edges[i,1]), target=toString(edges[i,2]))
        
        # Extract attributes
        if(attr_exists) {
            eds[[i]] = list(data=c(st, mapAttributes(e_names, e_attr, i)))
        } else {
            eds[[i]] = list(data=st)
        }
        
        if(i %% 1000 == 0) {
            print(i)
        }
    }
    
    el = list(nodes=nds, edges=eds)
    
    x <- list(data = graph_attr, elements = el)
    #print("Done.")
    return (toJSON(x, auto_unbox=TRUE, pretty=TRUE))
}

#' Map Attributes from igraph to Cytoscape JSON
#' 
#' @param attr.names names of attributes
#' @param all.attr all attributes
#' @param i index
#' 
#' @note From https://github.com/idekerlab/cy-rest-R/blob/17f748426bb5e48ba4075b9d97318ad582b250da/utility/cytoscape_util.R
#' 
#' @return attributes
#' 
#' @concept paxtoolsr
#' @export
mapAttributes <- function(attr.names, all.attr, i) {
    attr = list()
    cur.attr.names = attr.names
    attr.names.length = length(attr.names)
    
    for(j in 1:attr.names.length) {
        if(is.na(all.attr[[j]][i]) == FALSE) {
            #       attr[j] = all.attr[[j]][i]
            attr <- c(attr, all.attr[[j]][i])
        } else {
            cur.attr.names <- cur.attr.names[cur.attr.names != attr.names[j]]
        }
    }
    names(attr) = cur.attr.names
    return (attr)
}
