#' Convert IDs in a SIFNX
#' 
#' @param sifnx a SIFNX object (e.g. from the downloadPc2 function) 
#' @param participantType the type of participant on which the conversion will occur.
#'   Important because not all ID types apply to all entities and otherwise those entities
#'   would be labeled as missing an ID. 
#' @param mapping a two column data.frame with columns mapping$PARTICIPANT (old 
#'   IDs to convert from) and mapping$ID (new IDs to convert to) 
#' @param idType an ID type for conversion (not used if mapping parameter used)
#' @param naRm remove edges where NA's were introduced due to failed conversions
#' 
#' @return a SIFNX list with nodes and edges. Only edges will have converted IDs
#' 
#' @concept paxtoolsr
#' @export
convertSifnxIds <- function(sifnx, participantType="ProteinReference", idType="NCBI Gene", mapping=NULL, naRm=TRUE) {

    if(is.null(mapping)) {
        t1 <- extractIds(sifnx$nodes, participantType=participantType, idType=idType)
        t2 <- data.table(PARTICIPANT=names(t1), ID=t1)
        
        newA <- mapValues(sifnx$edges$PARTICIPANT_A, t2$PARTICIPANT, t2$ID)
        newB <- mapValues(sifnx$edges$PARTICIPANT_B, t2$PARTICIPANT, t2$ID)
    } else {
        newA <- mapValues(sifnx$edges$PARTICIPANT_A, mapping$PARTICIPANT, mapping$ID)
        newB <- mapValues(sifnx$edges$PARTICIPANT_B, mapping$PARTICIPANT, mapping$ID)
    }
    
    #newA <- mapvalues(sifnx$edges$PARTICIPANT_A, t2$PARTICIPANT, t2$ID, warn_missing=FALSE)
    #newB <- mapvalues(sifnx$edges$PARTICIPANT_B, t2$PARTICIPANT, t2$ID, warn_missing=FALSE)
    
    # Replace entries not found with NA 
    notFound <- setdiff(newA, sifnx$edges$PARTICIPANT_A) 
    for(i in notFound) {
        idx <- which(sifnx$nodes$PARTICIPANT == i)
        
        if(sifnx$nodes[idx,"PARTICIPANT_TYPE"] == participantType) {
            newA[newA == idx] <- NA
        }
    }
    
    # Replace entries not found with NA 
    notFound <- setdiff(newB, sifnx$edges$PARTICIPANT_B) 
    for(i in notFound) {
        idx <- which(sifnx$nodes$PARTICIPANT == i)
        
        if(sifnx$nodes[idx,"PARTICIPANT_TYPE"] == participantType) {
            newB[newB == idx] <- NA
        }
    }
    
    sifnx$edges$PARTICIPANT_A <- newA
    sifnx$edges$PARTICIPANT_B <- newB
    
    if(naRm) {
        a <- which(is.na(sifnx$edges$PARTICIPANT_A))
        b <- which(is.na(sifnx$edges$PARTICIPANT_B))
        
        tmp <- unique(c(a, b))
        idx <- setdiff(1:nrow(sifnx$edges), tmp)
        
        sifnx$edges <- sifnx$edges[idx, ]
    }
    
    return(sifnx)
}
