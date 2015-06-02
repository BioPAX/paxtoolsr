
#' Download Pathway Commons data (DEPRECATED)
#' 
#' Download Pathway Commons data in various formats
#' 
#' @param format a string describing the format to be downloaded; 
#'   currently, only the Extended Simple Interaction Format (SIF) "SIFNX" and 
#'   Gene Set Enrichment Analysis "GMT" formats for the entire Pathway Commons 
#'   database are supported. 
#' @param verbose a boolean debugging information
#' @return a named list with named pathways, each entry contains a vector of gene 
#'   symbols (for GMT) or a list with two data.frames (for SIFNX): 
#'   \itemize{ 
#'     \item edges Network edges with the following columns: 
#'       PARTICIPANT_A: Edge (interaction) participant, 
#'       INTERACTION_TYPE: Interaction type (see details), 
#'       PARTICIPANT_B: Edge (interaction) participant, 
#'       INTERACTION_DATA_SOURCE: Semi-colon delimited list of database sources 
#'         of the interaction, 
#'       INTERACTION_PUBMED_ID: Semi-colon delimited list of NCBI Pubmed IDs 
#'         that give evidence for the interaction 
#'     \item nodes Node information: 
#'       PARTICIPANT: Interaction participant,
#'       PARTICIPANT_TYPE: BioPAX class (see details),
#'       PARTICIPANT_NAME: Display name for the participant, 
#'       UNIFICATION_XREF: A UnificationXref defines a reference to an entity 
#'         in an external resource that has the same biological identity as 
#'         the referring entity, 
#'       RELATIONSHIP_XREF: An RelationshipXref defines a reference to an entity
#'         in an external resource that does not have the same biological 
#'         identity as the referring entity.
#'   }
#'  
#' @details 
#' 
#' Use \code{\link{downloadPc2}}
#' 
#' @examples 
#' \dontrun{
#' downloadPc(format="GMT") 
#' }
#' 
#' @concept paxtoolsr
#' @seealso \code{\link{downloadPc2}}
#' @export 
downloadPc <- function(format=c("SIFNX", "GMT"), verbose=FALSE) {    
    if(format == "SIFNX") {
        orgFile <- tempfile("sifnx", fileext=".gz")
        
        url <- paste0(getPcUrl(), "downloads/Pathway%20Commons.", getOption("pc.version"), 
                      ".All.EXTENDED_BINARY_SIF.hgnc.sif.gz ")
        
        if(verbose) {
            cat("URL: ", url, "\n")
        }
        
        download.file(url, orgFile)
        
        #con <- gzcon(file(orgFile, "r"))
        results <- readSifnx(orgFile)
    }
    
    if(format == "BIOPAX") {
    }  
    
    if(format == "GMT") {
        file <- tempfile("gmt", fileext = ".gz")
        url <- paste0(getPcUrl(), "downloads/Pathway%20Commons.", getOption("pc.version"), 
                      ".All.GSEA.hgnc.gmt.gz")
        
        if(verbose) {
            cat("URL: ", url, "\n")
        }
        
        download.file(url, file)
        
        results <- readGmt(gzfile(file))
    }
    
    return(results)       
}
