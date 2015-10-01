#' Map IDs to Primary Uniprot or ChEBI IDs 
#' 
#' Unambiguously maps, e.g., HGNC gene symbols, NCBI Gene, RefSeq, ENS*, and
#' secondary UniProt identifiers to the primary UniProt accessions, or - ChEBI
#' and PubChem IDs to primary ChEBI. You can mix different standard ID types in
#' one query.
#' 
#' @param ids a vector of IDs 
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return a list of where each entry is a HGNC symbol provided and the each 
#'   value is a primary UniProt or ChEBI ID. 
#'   
#' @details This is a specific id-mapping (not general-purpose) for reference
#'   proteins and small molecules; it was first designed for internal use, such
#'   as to improve BioPAX data integration and allow for graph queries accept
#'   not only URIs but also standard IDs. The mapping tables were derived
#'   exclusively from Swiss-Prot (DR fields) and ChEBI data (manually created
#'   tables and other mapping types and sources can be added in the future
#'   versions if necessary).
#' 
#' @examples
#' genes <- c("BRCA2", "TP53")
#' #results <- idMapping(genes)
#' 
#' @concept paxtoolsr
#' @export 
idMapping <- function(ids, verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "idmapping?id=")
    url <- paste0(baseUrl, ids[1])
    
    if(length(ids) >= 2) {
        for(i in 2:length(ids)) {
            url <- paste0(url, "&id=", ids[i])
        }        
    }
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "JSON")
    return(results)
}
