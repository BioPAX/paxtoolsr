#' Get a Pathway Commons Databases
#' 
#' @return a names of databases that can be used as part of queries
#' 
#' @examples 
#' getPcDatabaseNames()
#' 
#' @concept paxtoolsr
#' @export
getPcDatabaseNames <- function() {
    dbMapping <- c("psp"="phosphositeplus", "reconx"="recon x", "wp"="wikipathways")
    
    t1 <- downloadPc2(returnNames="BioPAX")
    t2 <- strsplit(t1, "\\.")
    t3 <- lapply(t2, function(x) {
        x[3]
    })
    t4 <- setdiff(unlist(t3), c("All", "Warehouse", "Detailed"))
    t5 <- t4
    for(i in 1:length(t5)) {
        key <- names(dbMapping)[i]
        if(t5[i] %in% names(dbMapping)) {
            t5[i] <- dbMapping[[t5[i]]]
        }
    }
    
    return(t5)
}
