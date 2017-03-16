#' Acceptable Pathway Commons Directions
#' 
#' A simple function to see valid options 
#' 
#' @return acceptable Pathway Commons directions
#' 
#' @details 
#'
#' \itemize{
#'   \item BOTHSTREAM where the current entity can either be the source or 
#'     target of an interaction 
#'   \item DOWNSTREAM where the current entity can only be the source
#'   \item UPSTREAM where the current entity can only be the target
#' }
#' 
#' @examples 
#' pcDirections() 
#' 
#' @concept paxtoolsr
#' @export 
pcDirections <- function() {
    pcDirections <- c("BOTHSTREAM", "DOWNSTREAM", "UPSTREAM")
    
    return(pcDirections)
}
