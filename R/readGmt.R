#' Read in gene sets from GMT files 
#'
#' This function will read in gene sets in the GMT format into a named list. 
#' 
#' @param inputFile an inputFile
#' @return a named list where each entry corresponds to a gene set
#' 
#' @examples 
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readGmt <- function(inputFile) {
    f <- readLines(inputFile)
    results <- sapply(f, function(x) unlist(strsplit(x, "\t", fixed = TRUE)))
    names(results) <- sapply(results, function(x) x[1])
    results <- lapply(results, function(x) x[-(1:2)])
    return(results)
}
