# toSifnx("<file1> <outEdges> <outNodes> <node-prop1,node-prop2,..> 
# <edge-prop1,edge-prop2,...>\n" +
# converter.writeInteractionsInSIFNX(m, out, out, 
# "EntityReference/name,EntityReference/xref" "Interaction/dataSource/displayName"

#' Converts BioPAX OWL file to extended binary SIF representation
#' 
#' @param inputFile a string with the name of the input BioPAX OWL file
#' @param outputNodesFile a string with the name of the output file for node 
#'   information
#' @param outputEdgesFile a string with the name of the output file for edge 
#'   information
#' @param nodeProps a string node properties to be saved; these are set up as XPath like 
#'   expressions of data in BioPAX files (e.g. c("EntityReference/name", "EntityReference/xref"))
#' @param edgeProps a string edge properties to be saved; these are set up as XPath like 
#'   expressions of data in BioPAX files (e.g. "Interaction/dataSource/displayName")
#' 
#' @return a list with two entries 
#' \itemize{
#'   \item nodes a data.frame with interaction participant information specified
#'     in nodeProps
#'   \item edges a data.frame with SIF formatted data (i.e. an edgelist with a 
#'     additional middle column denoting interaction type) and any additional 
#'     columns specified in edgeProps.
#' }
#' 
#' @details Information on SIF conversion is provided on the Pathway Commons 
#'   site: \url{http://www.pathwaycommons.org/pc2/}
#' 
#' @examples
#' edgesFile <- tempfile()
#' nodesFile <- tempfile()
#' results <- toSifnx(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   nodesFile,
#'   edgesFile, 
#'   c("EntityReference/name", "EntityReference/xref"), 
#'   "Interaction/dataSource/displayName") 
#' 
#' @concept paxtoolsr
#' @export
toSifnx <- function(inputFile, outputNodesFile=NULL, outputEdgesFile=NULL, 
                    nodeProps, edgeProps) {
    inputFile <- checkInputFile(inputFile)
    outputEdgesFile <- checkOutputFile(outputEdgesFile)
    outputNodesFile <- checkOutputFile(outputNodesFile)
    
    nodePropsCollapsed <- paste(nodeProps, collapse=",")
    edgePropsCollapsed <- paste(edgeProps, collapse=",")
    
    #DEBUG 
    #str(nodeProps)
    
    command <- "toSifnx"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    outputEdgesJStr <- .jnew("java/lang/String", outputEdgesFile)       
    outputNodesJStr <- .jnew("java/lang/String", outputNodesFile)
    
    nodePropsJStr <- .jnew("java/lang/String", nodePropsCollapsed)
    edgePropsJStr <- .jnew("java/lang/String", edgePropsCollapsed)
    
    argsList <- list(commandJStr, inputJStr, outputEdgesJStr, outputNodesJStr, 
                     nodePropsJStr, edgePropsJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    edges <- read.table(outputEdgesFile, sep="\t", as.is=TRUE, quote="", 
                        fill=TRUE)
    nodes <- read.table(outputNodesFile, sep="\t", as.is=TRUE, quote="", 
                        fill=TRUE)
    
    colnames(edges) <- c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B", edgeProps)
    colnames(nodes) <- c("PARTICIPANT", nodeProps)
    
    results <- list(edges=edges, nodes=nodes)
    
    return(results)
}
