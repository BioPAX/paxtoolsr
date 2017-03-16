#' Map values from One Vector to Another 
#' 
#' @param data a vector of strings where values will be replaced
#' @param oldValue a vector that matches values in the data vector
#' @param newValue a vector of new values that will replace the old values
#' 
#' @return return the vector with the mapped values. If there was no 
#'   corresponding entry then replace it with an NA. 
#'   
#' @examples 
#' data <- c("A", "B", "C", "X", "Y", "Z") 
#' oldValue <- LETTERS[1:20]
#' newValue <- letters[1:20]
#' results <- mapValues(data, oldValue, newValue)
#' 
#' @concept paxtoolsr
#' @export
mapValues <- function(data, oldValue, newValue) {
    # convert any factors to characters
    #if (is.factor(data))     data     <- as.character(data)
    #if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
    #if (is.factor(newvalue)) newvalue <- as.character(newvalue)
    
    # Create the return vector
    newVec <- data
    
    # Put replaced values into the correct position in the return vector
    for (i in unique(oldValue)) newVec[data == i] <- newValue[oldValue == i]
    
    return(newVec)
}
