#' Acceptable Pathway Commons Graph Queries 
#' 
#' A simple function to see valid options 
#' 
#' @return acceptable Pathway Commons graph queries  
#' 
#' @details 
#'   
#' \itemize{
#'   \item COMMONSTREAM searches common downstream or common upstream of a
#'   specified set of entities based on the given directions within the
#'   boundaries of a specified length limit
#'   \item NEIGHBORHOOD searches the neighborhood of given source set of nodes
#'   \item PATHSBETWEEN finds the paths between specific source set of states or
#'   entities within the boundaries of a specified length limit
#'   \item PATHSFROMTO finds the paths from a specific source set of states or
#'   entities to a specific target set of states or entities within the
#'   boundaries of a specified length limit
#' }
#' 
#' @examples
#' pcGraphQueries()
#' 
#' @concept paxtoolsr
#' @export 
pcGraphQueries <- function() {
    pcGraphQueries <- c("COMMONSTREAM", "NEIGHBORHOOD", "PATHSBETWEEN", 
                        "PATHSBETWEEN", "PATHSFROMTO")
    
    return(pcGraphQueries)
}
