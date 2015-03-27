#' Read in gene sets from GMT files 
#'
#' This function will read in gene sets in the GMT format into a named list. 
#' 
#' @param inputFile an inputFile in the GMT format
#' @return a named list where each entry corresponds to a gene set
#' 
#' @examples 
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readGmt <- function(inputFile) {
    f <- readLines(inputFile)
    lst <- sapply(f, function(x) unlist(strsplit(x, "\t", fixed = TRUE)))
    names(lst) <- sapply(lst, function(x) x[1])
    lst <- lapply(lst, function(x) x[-(1:2)])
    return(lst)
}

#' Read in a Binary SIF file 
readBinarySif <- function(inputFile) {
    edges <- fread(inputFile, sep="\t", header=TRUE, stringsAsFactors=FALSE)
    return(edges)
}

readSifnx <- function() {
}

readBiopax <- function() {
}

#' TODO: Make function 
convertToPathwayObject <- function() {
}

#' TODO: Make function 
extractPathways <- function() {
}

#' TODO: Make function <
#' interacts-with/neighbor-of
#' chemical-X
#' other 
#' vector of interactions 
filterSif <- function(keepX, keepY, keepZ) {
    
}

#' TODO: Make function
loadSifInIgraph <- function() {
    
}

#' TODO: Make function
summarizeSif <- function() {
    
}

#' TODO: Make function
getNodeIds <- function() {
    
}
