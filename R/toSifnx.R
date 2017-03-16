# toSifnx("<file1> <outEdges> <outNodes> <node-prop1,node-prop2,..> 
# <edge-prop1,edge-prop2,...>\n" +
# converter.writeInteractionsInSIFNX(m, out, out, 
# "EntityReference/name,EntityReference/xref" "Interaction/dataSource/displayName"

#' Converts BioPAX OWL file to extended binary SIF representation
#' 
#' @param inputFile a string with the name of the input BioPAX OWL file
#' @param outputFile a string with the name of the output file for SIFNX 
#'   information
#' @param idType a string either "hgnc" or "uniprot" (DEFAULT: uniprot, more common)  
#' 
#' @return see readSifnx()
#' 
#' @details Information on SIF conversion is provided on the Pathway Commons 
#'   site: \url{http://www.pathwaycommons.org/pc2/}. Also, this is a Java-based
#'   methods, it is best to use full paths. 
#' 
#' @examples
#' inputFile <- system.file("extdata", "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr")
#' results <- toSifnx(inputFile=inputFile) 
#' 
#' @concept paxtoolsr
#' @export
toSifnx <- function(inputFile, outputFile=tempfile(), idType="uniprot") {
    inputFile <- checkInputFile(inputFile)
    #outputNodesFile <- checkOutputFile(outputNodesFile)
    #outputEdgesFile <- checkOutputFile(outputEdgesFile)
    outputFile <- checkOutputFile(outputFile)
    
    #nodePropsCollapsed <- paste(nodeProps, collapse=",")
    #edgePropsCollapsed <- paste(edgeProps, collapse=",")
    
    command <- "toSifnx"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    #outputEdgesJStr <- .jnew("java/lang/String", outputEdgesFile)       
    #outputNodesJStr <- .jnew("java/lang/String", outputNodesFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    #nodePropsJStr <- .jnew("java/lang/String", nodePropsCollapsed)
    #edgePropsJStr <- .jnew("java/lang/String", edgePropsCollapsed)
    idTypeJStr <- .jnew("java/lang/String", idType)
    
    #argsList <- list(commandJStr, inputJStr, outputJStr, nodePropsJStr, edgePropsJStr) 
    argsList <- list(commandJStr, inputJStr, outputJStr, idTypeJStr) 
    
    #DEBUG 
    #str(argsList)
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
#     nodes <- read.table(outputNodesFile, sep="\t", as.is=TRUE, quote="", 
#                         fill=TRUE)
#     edges <- read.table(outputEdgesFile, sep="\t", as.is=TRUE, quote="", 
#                         fill=TRUE)
#     
#     colnames(edges) <- c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B", edgeProps)
#     colnames(nodes) <- c("PARTICIPANT", nodeProps)
#     
#     results <- list(edges=edges, nodes=nodes)
    
    results <- readSifnx(outputFile)
    
    return(results)
}
