#' Splits SIFNX entries into individual pathways 
#'  
#' @param edges a data.frame with SIF content with the additional column "PATHWAY_NAMES".
#'   "PATHWAY_NAMES" should include pathway names delimited with a semi-colon: ";".
#' @param parallel a boolean that will parallelize the process; requires foreach/doSNOW/parallel packages  
#' @return a list of where each entry is a vector of row indicies for a given pathway
#' 
#' @details 
#' This method can be slow; ~1.5 minutes for 150K+ rows. 
#' Has a parallelized method to speed things up. 
#' 
#' @concept paxtoolsr
#' @export
splitSifnxByPathway <- function(edges, parallel=FALSE) {
    stopifnot("PATHWAY_NAMES" %in% colnames(edges))
    
    #tmp <- strsplit(edges$PATHWAY_NAMES, ";", fixed=TRUE)
    tmp <- edges$PATHWAY_NAMES
    tmp2 <- unique(tmp)
    pathwayNames <- unique(unlist(tmp2))
    
    iterations <- length(pathwayNames)
    cat("ITERATIONS: ", iterations, "\n")
    
    # Make sure the necessary packages are available 
    if(all(parallel,
           requireNamespace("foreach"), 
           requireNamespace("doSNOW"), 
           requireNamespace("parallel"))) {
        numCores <- parallel::detectCores()
        cl <- parallel::makeCluster(numCores, outfile="") # number of cores. Notice 'outfile'
        doSNOW::registerDoSNOW(cl)
        
        pb <- txtProgressBar(min = 1, max = iterations, style = 3)
        
        results <- foreach::foreach(i=1:iterations, .packages=c("paxtoolsr")) %dopar% {
            setTxtProgressBar(pb, i) 
            pathwayName <- pathwayNames[i]
            
            tmpResults <-  searchListOfVectors(pathwayName, tmp)
            return(as.vector(tmpResults))	
        }
        
        names(results) <- pathwayNames
        
        close(pb)
        parallel::stopCluster(cl) 
    } else {
        pb <- txtProgressBar(min = 1, max = iterations, style = 3)
        
        results <- list()
        
        for(i in 1:iterations) {
            setTxtProgressBar(pb, i) 
            pathwayName <- pathwayNames[i]
            
            tmpResults <-  searchListOfVectors(pathwayName, tmp)
            results[[pathwayName]] <- as.vector(tmpResults)
        }
    }
    
    return(results)
}
