#' Read in a Extended SIF file
#'
#' @param inputFile an inputFile
#'
#' @return a list with nodes and edges entries
#'
#' @details SIFNX files from Pathway Commons commonly come a single file that
#' includes a tab-delimited sections for nodes and another for edges. The
#' sections are separated by an empty lines. These sections must be split before
#' they are read.
#'
#' @examples
#' results <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
#'
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom readr read_tsv cols
readSifnx <- function(inputFile) {
  checkInputFilePc(inputFile)

  tmp <- readChar(inputFile, nchars=file.info(inputFile)$size, useBytes=TRUE)
  
  edgesFile <- tempfile()
  nodesFile <- tempfile()
  
  idx <- gregexpr("PARTICIPANT\tPARTICIPANT_TYPE", tmp, fixed = TRUE)
  tmpEdges <- substr(tmp, 1, idx[[1]][1]-1)
  writeChar(tmpEdges, edgesFile)
  
  tmpNodes <- substr(tmp, idx[[1]][1], nchar(tmp))
  writeChar(tmpNodes, nodesFile)

  nodes <- suppressWarnings(read_tsv(nodesFile, 
                                     progress = TRUE, 
                                     col_names = TRUE, 
                                     col_types = cols(.default = "c")))
  
  edges <- suppressWarnings(read_tsv(edgesFile, 
                                     progress = TRUE, 
                                     col_names = TRUE, 
                                     col_types = cols(.default = "c")))
    
  results <- list(nodes=nodes, edges=edges)
  
  return(results)

#     # EDGES
#     edgesInteractionDataSource <- strsplit(as.character(edges$INTERACTION_DATA_SOURCE), ";")
#     edgesPubmedId <- strsplit(as.character(edges$INTERACTION_PUBMED_ID), ";")
#     edgesPathwayNames <- strsplit(as.character(edges$PATHWAY_NAMES), ";")
#
#     # NODES
#     nodesUniXref <- strsplit(as.character(nodes$UNIFICATION_XREF), ";")
#     names(nodesUniXref) <- nodes$PARTICIPANT
#
#     nodesRelXref <- strsplit(as.character(nodes$RELATIONSHIP_XREF), ";")
#     names(nodesRelXref) <- nodes$PARTICIPANT
#
#     nodesType <- nodes$PARTICIPANT_TYPE
#     names(nodesType) <- nodes$PARTICIPANT
#
#     nodesName <- strsplit(as.character(nodes$PARTICIPANT_NAME), ";")
#     names(nodesName) <- nodes$PARTICIPANT
}
