#' Extract IDs from an Extended SIF
#'
#' @param nodes extended SIF nodes entries as a data.frame with RELATIONSHIP_XREF as a vector of IDs
#' @param participantType a vector of types of participants to search;
#'   useful to only search protein (ProteinReference) or small molecule (SmallMoleculeReference) related entries.
#' @param idType the type of ID to search for; case-insensitive
#'
#' @return a named vector of the first matches for the given ID type
#'
#' @details IMPORTANT: Only the first matching ID will be returned. In some cases, multiple IDs will exist.
#'
#' @examples
#' t1 <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
#' t2 <- convertToDataFrameWithListOfVectors(t1)
#'
#' results <- extractIds(t2$nodes)
#'
#' @concept paxtoolsr
#' @export
extractIds <- function(nodes, participantType="ProteinReference", idType="hgnc symbol") {
    idx <- which(nodes$PARTICIPANT_TYPE %in% participantType)

    t1 <- sapply(nodes$RELATIONSHIP_XREF[idx], function(x) {
        tmp <- grep(idType, x, ignore.case=TRUE, value=TRUE)[1]

        # Account for colon at the end of ID names
        substr(tmp, (nchar(idType)+2), nchar(tmp))
    })

    t3 <- unlist(t1)

    # Only get the first entry
    #nodeNames <- unlist(lapply(nodes$PARTICIPANT[idx], function(x){ x[[1]][1] }))
    nodeNames <- nodes$PARTICIPANT[idx]
    names(t3) <- nodeNames

    return(t3)
}
