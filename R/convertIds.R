#' Covert IDs Using org.Hs.eg.db
#' 
#' @param ids a vector of ids
#' @param from the original ID type
#' @param to the ID type to convert to
#' 
#' @return a vector of converted IDs 
#' 
#' @examples 
#' convertIds("TP53", "SYMBOL", "ENTREZID")
#' 
#' @concepts paxtoolsr
#' @export
convertIds <- function(ids, from="UNIPROT", to="SYMBOL") {
    if(!require("org.Hs.eg.db")) {
        stop("This function requires org.Hs.eg.db.")
    }
    
    df <- select(org.Hs.eg.db, keys=ids, columns=c(from, to), keytype=from)
    
    return(df[,2])
}
