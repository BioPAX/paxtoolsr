#' Read small extended SIFs (SIFNX)
#'
#' @param inputFile Name of the imput file
#'
#' @return a list of nodes and edges
#'
#' @note This function is a dependency free version of readSifnx; this is not suitable for very large files
#'
#' @examples
#' sifnx <- readSifnxSmall(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
#'
#' @concept paxtoolsr
#' @importFrom utils read.table
readSifnxSmall <- function(inputFile) {
    checkInputFilePc(inputFile)

    edgesFile <- tempfile("edges", fileext=".txt")
    nodesFile <- tempfile("nodes", fileext=".txt")

    # Open file connections
    edgesCon <- file(edgesFile, "w")
    nodesCon <- file(nodesFile, "w")

    con <- file(inputFile)

    newLineFlag <- FALSE

    # Read single lines
    lineTmp <- readLines(con, warn=FALSE)

    for (i in 1:length(lineTmp)) {
        line <- lineTmp[i]

        if(grepl("^$", line)) {
            newLineFlag <- TRUE
            next
        }

        if(!newLineFlag) {
            writeLines(line, edgesCon)
        } else {
            writeLines(line, nodesCon)
        }
    }

    close(edgesCon)
    close(nodesCon)
    close(con)

    edges <- read.table(edgesFile, header=TRUE, sep="\t", quote="",
                        stringsAsFactors=FALSE, fill=TRUE, row.names=NULL)
    nodes <- read.table(nodesFile, header=TRUE, sep="\t", quote="",
                        stringsAsFactors=FALSE, fill=TRUE, row.names=NULL)

    results <- list(nodes=nodes,
                    edges=edges)

    return(results)
}

# data <- c("A", "B", "C", "X", "Y", "Z")
# oldValue <- LETTERS[1:20]
# newValue <- letters[1:20]
# results <- mapValues(data, oldValue, newValue)
