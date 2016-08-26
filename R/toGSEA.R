#' Converts a BioPAX OWL file to a GSEA GMT gene set
#' 
#' This function converts pathway information stored as BioPAX files into the 
#' the GSEA .gmt format. 
#' 
#' @param inputFile a string of the name of the input OWL file
#' @param outputFile a string of the name of the output file
#' @param database a string of the name of the identifier type to be included 
#'   (e.g. "HGNC Symbol")
#' @param crossSpeciesCheckFlag a boolean that ensures participant protein is 
#'   from same species
#' @return see readGmt()
#' 
#' @details The GSEA GMT format is a tab-delimited format where each row 
#' represents a gene set. The first column is the gene set name. The second 
#' column is a brief description. Other columns for each row contain genes in 
#' the gene set; these rows may be of unequal lengths. 
#' 
#' @examples
#' outFile <- tempfile()
#' results <- toGSEA(system.file("extdata", "biopax3-short-metabolic-pathway.owl", 
#'                               package="paxtoolsr"), 
#'                               outFile, 
#'                               "uniprot", 
#'                               crossSpeciesCheckFlag=TRUE) 
#' 
#' @concept paxtoolsr
#' @export
toGSEA <- function(inputFile, outputFile=NULL, database="uniprot", crossSpeciesCheckFlag=TRUE) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    if(crossSpeciesCheckFlag) {
        crossSpeciesCheckFlag <- "crossSpeciesCheck"        
    } else {
        crossSpeciesCheckFlag <- ""
    }
    
    command <- "toGSEA"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    dbJStr <- .jnew("java/lang/String", database)
    flagJStr <- .jnew("java/lang/String", as.character(crossSpeciesCheckFlag))
    
    argsList <- list(commandJStr, inputJStr, outputJStr, dbJStr, flagJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
#     tmp <- read.table(outputFile, sep="\t", as.is=TRUE, fill=TRUE)
#     
#     # as.vector(unlist()) to remove column names from tmp
#     results <- list(name=tmp[,1], 
#                     description=tmp[,2], 
#                     geneSet=as.vector(unlist(tmp[,3:length(tmp)])))
    
    results <- readGmt(outputFile)
    
    return(results)
}
