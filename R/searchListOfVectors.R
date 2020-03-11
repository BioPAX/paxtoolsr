#' Search List of Vectors 
#' 
#' @param q query vector
#' @param lst list of vectors to search
#' 
#' @return a list of vectors with the same length as the query vector, each list
#'   entry will have indicies for lst where there was a match with the query 
#'   vector. Return NA if there were no matches.
#'   
#' @details 
#' Taken from: http://stackoverflow.com/questions/11002391/fast-way-of-getting-index-of-match-in-list
#' 
#' @examples 
#' lst <- list(1:3, 3:5, 3:7)
#' q <- c(3, 5)
#' results <- searchListOfVectors(q, lst)
#' names(results) <- q
#' 
#' lst <- list(LETTERS[1:3], LETTERS[3:5], LETTERS[3:7])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst)
#' 
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- "C"
#' searchListOfVectors(q, lst)
#' 
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- c("C")
#' searchListOfVectors(q, lst)
#' 
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst)
#' 
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- "Z"
#' searchListOfVectors(q, lst)
#' 
#' @concept paxtoolsr
#' @export
searchListOfVectors <- function(q, lst) {
    tmp <- rep(seq_along(lst), sapply(lst, length))
    resultsSe <- sapply(q, function(x) tmp[which(unlist(lst) %in% x)], simplify=FALSE)
    
    if(!is(resultsSe, "list")) {
        return(NA)
    }
    
    return(resultsSe)
}
