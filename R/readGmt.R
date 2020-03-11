#' Read in gene sets from GMT files
#'
#' This function will read in gene sets in the GMT format into a named list.
#'
#' @param inputFile an inputFile
#' @param removePrefix Pathway Commons genesets are prefixed with a NCBI organism taxonomy number (e.g. 9606 for humans); this is a boolean whether to remove the prefix (default: FALSE)
#' @param returnInfo a boolean whether to return information on genesets; these results are returned a list of two items: 1) basic GMT results and 2) datasource, organism, and id type information for each gene set (default: FALSE)
#'
#' @return a named list where each entry corresponds to a gene set or a list described in the returnInfo parameter
#'
#' @examples
#' f1 <- system.file("extdata", "test_PathwayCommons12.kegg.hgnc.gmt", 
#'   package="paxtoolsr")
#' f2 <- system.file("extdata", "test_PathwayCommons12.netpath.hgnc.gmt", 
#'   package="paxtoolsr")
#'   
#' results <- readGmt(f1)
#' results <- readGmt(f2)
#' results <- readGmt(f1, removePrefix=TRUE)
#' results <- readGmt(f2, returnInfo=TRUE)
#'
#' @concept paxtoolsr
#' @importFrom methods is 
#' @export
readGmt <- function(inputFile, removePrefix=FALSE, returnInfo=FALSE) {
    checkInputFilePc(inputFile)

    fileContents <- readLines(inputFile)

    # NOTE: Removing empty strings is necessary
    fileContents <- fileContents[fileContents != ""]

    # Extract file contents and make a list of vectors
    tmpResults <- sapply(fileContents, function(x) {
      tmp <- unlist(strsplit(x, "\t", fixed = TRUE))
    })

    if(is(tmpResults, "matrix")) { 
        t2 <- tmpResults
        tmpResults <- list()
        tmpResults[[t2[1]]] <- as.vector(t2)
    }

    # Extract the URI as the name for the geneset
    names(tmpResults) <- sapply(tmpResults, function(x) {
        if(removePrefix) { 
            t1 <- strsplit(x, ": ")[[1]]
            t2 <- paste(t1[2:length(t1)], collapse = ": ")
            results <- trimws(t2)
        } else {
            results <- x[1]
        }

        return(results)
    })

    # This splits the name into various elements
    if(returnInfo) { 
        results <- lapply(tmpResults, function(x) {
            t1 <- trimws(strsplit(x[2], ";")[[1]])
            tmp <- strsplit(t1, ": ")

            name <- tmp[[1]][2]
            dataSource <- tmp[[2]][2]
            organism <- tmp[[3]][2]
            idType <- tmp[[4]][2]
            geneSet <- x[-(1:2)]

            results <- list(geneSet = geneSet, name = name, dataSource = dataSource, organism = organism, idType = idType)

            return(results)
        })
    } else {
        # Remove the first two entries, i.e. the name and description
        results <- lapply(tmpResults, function(x) x[-(1:2)])
    }

    return(results)
}
