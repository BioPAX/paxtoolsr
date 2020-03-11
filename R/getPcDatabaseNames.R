#' Get a Pathway Commons Databases
#' 
#' @param version PC2 version 
#' 
#' @return a names of databases that can be used as part of queries
#' 
#' @examples 
#' getPcDatabaseNames(version=10)
#' 
#' @concept paxtoolsr
#' @export
getPcDatabaseNames <- function(version) {
    dbMapping <- c("psp"="phosphositeplus", "reconx"="recon x", "wp"="wikipathways")
    
    t1 <- downloadPc2(returnNames="BioPAX", version=version)
    t2 <- strsplit(t1, "\\.")
    t3 <- lapply(t2, function(x) {
      tmp <- setdiff(x, c("PathwayCommons", "PathwayCommons10", "PathwayCommons9", "BIOPAX", "gz", "owl", "8", "7", "6", "5", "4"))
      tmp
    })
    t4 <- setdiff(unlist(t3), c("All", "Warehouse", "Detailed"))
    t5 <- t4
    for(i in seq_along(t5)) {
        key <- names(dbMapping)[i]
        if(t5[i] %in% names(dbMapping)) {
            t5[i] <- dbMapping[[t5[i]]]
        }
    }
    
    t6 <- sort(t5)
    
    return(t6)
}
