#' Read in gene sets from GMT files 
#'
#' This function will read in gene sets in the GMT format into a named list. 
#' 
#' @param inputFile an inputFile
#' @return a named list where each entry corresponds to a gene set
#' 
#' @examples 
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readGmt <- function(inputFile) {
    f <- readLines(inputFile)
    results <- sapply(f, function(x) unlist(strsplit(x, "\t", fixed = TRUE)))
    names(results) <- sapply(results, function(x) x[1])
    results <- lapply(results, function(x) x[-(1:2)])
    return(results)
}

#' Read in a binary SIF file 
#' 
#' @param inputFile an inputFile
#' @return a data.frame with the interactions in the binary SIF format
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom data.table fread
readSif <- function(inputFile) {
    results <- fread(inputFile, sep="\t", header=TRUE, stringsAsFactors=FALSE)
    results <- as.data.frame(results)
    colnames(results) <- c("PARTICIPANT_A",  "INTERACTION_TYPE", "PARTICIPANT_B")
    
    return(results)
}

#' Read in a Extended SIF file 
#' 
#' @param inputFile an inputFile
#' @return a list with nodes and edges entries 
#' 
#' @details SIFNX files from Pathway Commons commonly come a single file that 
#' includes a tab-delimited sections for nodes and another for edges. The 
#' sections are separated by an empty lines. These sections must be split before
#' they are read. 
#' 
#' @examples 
#' results <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
#' chebiIds <- lapply(results$nodesUniXref, function(x) { x[which(grepl("CHEBI", x))] })
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom data.table fread
readSifnx <- function(inputFile) {
    # Files with small sizes will confuse fread to think there are fewer columns 
    # in the edges because it scans the 5th row to determine number of columns. 
    # Two methods of reading are therefore necessary. 
    tmp <- file.info(inputFile)

    if(tmp$size < 100000) {
        edgesFile <- tempfile("edges", fileext=".txt")
        nodesFile <- tempfile("nodes", fileext=".txt")
        
        # Open file connections
        edgesCon <- file(edgesFile, "w")
        nodesCon <- file(nodesFile, "w")
        
        con <- file(inputFile)

        newLineFlag <- FALSE

        # Read single lines
        lineTmp <- readLines(con, warn=FALSE)
        
        for (i in 1:length(lineTmp)) {
            line <- lineTmp[i]
            
            if(grepl("^$", line)) {
                newLineFlag <- TRUE
                next
            }
            
            if(!newLineFlag) {
                writeLines(line, edgesCon)
            } else {
                writeLines(line, nodesCon)        
            }
        }
        
        close(edgesCon)
        close(nodesCon)
        close(con)
        
        edges <- read.table(edgesFile, header=TRUE, sep="\t", quote="", 
                            stringsAsFactors=FALSE, fill=TRUE)
        nodes <- read.table(nodesFile, header=TRUE, sep="\t", quote="", 
                            stringsAsFactors=FALSE, fill=TRUE)        
    } else {
        # A warning on discarded content is expected because of the 2-files in 1 nature of the file
        suppressWarnings(tmp <- fread(inputFile, sep="\n", header=FALSE, stringsAsFactors=FALSE))
        nodes <- fread(inputFile, sep="\t", header=TRUE, stringsAsFactors=FALSE, skip="PARTICIPANT\tPARTICIPANT_TYPE", 
                       data.table=FALSE)
        
        tmp2 <- paste(tmp$V1, collapse="\n")
        edges <- fread(tmp2, sep="\t", header=TRUE, stringsAsFactors=FALSE,  data.table=FALSE)
    }
    
    
    # EDGES
    edgesPathwayNames <- strsplit(as.character(edges$PATHWAY_NAMES), ";")
    
    # NODES    
    nodesUniXref <- strsplit(as.character(nodes$UNIFICATION_XREF), ";")
    names(nodesUniXref) <- nodes$PARTICIPANT
    
    nodesRelXref <- strsplit(as.character(nodes$RELATIONSHIP_XREF), ";")
    names(nodesRelXref) <- nodes$PARTICIPANT
    
    nodesType <- nodes$PARTICIPANT_TYPE
    names(nodesType) <- nodes$PARTICIPANT
    
    nodesName <- strsplit(as.character(nodes$PARTICIPANT_NAME), ";")
    names(nodesName) <- nodes$PARTICIPANT
    
    results <- list(nodes=nodes, 
                    edges=edges, 
                    nodesType=nodesType,
                    nodesName=nodesName,
                    nodesUniXref=nodesUniXref,
                    nodesRelXref=nodesRelXref)
    return(results)
}

#' Read BioPAX files as XML documents 
#' 
#' @param inputFile an inputFile
#' @return an XMLInternalDocument
#' 
#' @examples 
#' results <- readBiopax(system.file("extdata", "biopax3-short-metabolic-pathway.owl", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readBiopax <- function(inputFile) {
    results <- xmlTreeParse(inputFile, useInternalNodes=TRUE)
    return(results)
}

#' Read SBGN files as XML documents 
#' 
#' @param inputFile an inputFile
#' @return an XMLInternalDocument
#' 
#' @examples 
#' results <- readSbgn(system.file("extdata", "test_sbgn.xml", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readSbgn <- function(inputFile) {
    results <- xmlTreeParse(inputFile, useInternalNodes=TRUE)
    return(results)
}

#' Convert Results from readSifnx to data.table
#' 
#' @param lst a list returned from readSifnx
#' @return lst list entries converted to data.table
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom data.table setDT
convertToDT <- function(lst) {
    nodes <- lst$nodes
    nodes <- setDT(nodes)
    nodes$UNIFICATION_XREF <- strsplit(nodes$UNIFICATION_XREF, ";")
    nodes$RELATIONSHIP_XREF <- strsplit(nodes$RELATIONSHIP_XREF, ";")
    
    edges <- lst$edges
    edges <- setDT(edges)
    edges$INTERACTION_DATA_SOURCE <- strsplit(edges$INTERACTION_DATA_SOURCE, ";")
    edges$INTERACTION_PUBMED_ID <- strsplit(as.character(edges$INTERACTION_PUBMED_ID), ";")
    edges$PATHWAY_NAMES <- strsplit(as.character(edges$PATHWAY_NAMES), ";")  
    
    lst$edges <- edges
    lst$nodes <- nodes
    
    return(lst)
}

# # Convert to graphite Pathway Object 
# # 
# # @param lst list returned from 
# # 
# # @concept paxtoolsr
# # @export
# convertToPathwayList <- function(id="kegg", title="kegg", ident="DISPLAYNAME", 
#                                  species="hsapiens", lst) {
#     database <- unique(dbResults$edges$INTERACTION_DATA_SOURCE)
#     
#     edges <- data.frame(src=lst$edges$PARTICIPANT_A, 
#                         dest=lst$edges$PARTICIPANT_B, 
#                         direction=nrow(lst$edges), 
#                         type=lst$edges$INTERACTION_TYPE)
#     timestamp <- Sys.Date()
#     
#     pathway <- new("Pathway", 
#               id=id, 
#               title=title,
#               edges=edges,
#               database=database,
#               species=species,
#               identifier=ident,
#               timestamp=timestamp)
#     
#     return(pathway)
# }

#' Splits SIFNX entries into individual pathways 
#'  
#' @param edges a data.frame with SIF content with the additional column "PATHWAY_NAMES".
#'   "PATHWAY_NAMES" should include pathway names delimited with a semi-colon: ";".
#' @return a list of where each entry is a vector of row indicies for a given pathway
#' 
#' @details 
#' This method can be slow; ~1.5 minutes for 150K+ rows
#' 
#' @concept paxtoolsr
#' @export
splitSifnxByPathway <- function(edges) {
    stopifnot("PATHWAY_NAMES" %in% colnames(edges))
    
    tmp <- strsplit(edges$PATHWAY_NAMES, ";")
    tmp2 <- unique(tmp)
    pathwayNames <- unique(unlist(tmp2))

    results <-  searchListOfVectors(pathwayNames, tmp)
    
    return(results)
}

#' Search List of Vectors 
#' 
#' @param q query vector
#' @param lst list of vectors to search
#' 
#' @return a list of vectors with the same length as the query vector, each list
#'   entry will have indicies for lst where there was a match with the query 
#'   vector
#' 
#' @examples 
#' lst <- list(1:3, 3:5, 3:7)
#' q <- c(3, 5)
#' results <- searchListOfVectors(q, lst)
#' 
#' @concept paxtoolsr
#' @export
searchListOfVectors <- function(q, lst) {
    tmp <- rep(seq_along(lst), sapply(lst, length))
    resultsSe <- sapply(q, function(x) tmp[which(unlist(lst) %in% x)])
}

#' Keep interactions in SIF network of certain interaction types
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @param interactionTypes a vector of interaction types 
#'   (List of interaction types: http://www.pathwaycommons.org/pc2/formats)
#'   
#' @return a data.frame of filtered interactions with three columns: "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' intTypes <- c("controls-state-change-of", "controls-expression-of", "catalysis-precedes")
#' filteredNetwork <- filterSif(results, intTypes)
#' 
#' @concept paxtoolsr
#' @export
filterSif <- function(sif, interactionTypes) {
    idx <- which(sif$INTERACTION_TYPE %in% interactionTypes) 
    filteredNetwork <- sif[idx, ]
    
    return(filteredNetwork)
}

#' Get a list of categories of SIF interactions
#' 
#' @return a list of interactions in categories
#' 
#' @details 
#' Description of interaction types: http://www.pathwaycommons.org/pc2/formats
#' Categories provided: 
#'   BetweenProteins, 
#'   BetweenProteinsOther (often from high-throughput experiments),
#'   BetweenProteinSmallMolecule, 
#'   BetweenSmallMolecules
#' 
#' @examples 
#' sifCat <- getSifInteractionCategories()
#' sifCat[["BetweenProteins"]]
#' 
#' @concept paxtoolsr
#' @export
getSifInteractionCategories <- function() {
    protInt <- c("controls-state-change-of", "controls-expression-of", 
                 "controls-degradation-of", "controls-transport-of",
                 "catalysis-precedes", "in-complex-with")
    
    protOtherInt <- c("interacts-with", "neighbor-of")

    protSmMolInt <- c("consumption-controlled-by", "controls-production-of",
                      "controls-transport-of-chemical", "chemical-affects")
    
    smMolInt <- c("reacts-with", "used-to-produce")
    
    return(list("BetweenProteins"=protInt, 
                "BetweenProteinsOther"=protOtherInt,
                "BetweenProteinSmallMolecule"=protSmMolInt, 
                "BetweenSmallMolecules"=smMolInt))
}

#' Load SIF as igraph Network
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#'   
#' @return a directed igraph network with interaction types 
#' 
#' @examples 
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' g <- loadSifInIgraph(results)
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom igraph graph.edgelist E E<-
loadSifInIgraph <- function(sif) {
    gWithType <- graph.edgelist(as.matrix(sif[, c(1, 3)]), directed=TRUE)
    E(gWithType)$type <- sif[, 2]
    
    return(gWithType)
}

#' Summarize a SIF Network 
#' 
#' @param sif a binary SIF as a data.frame with three columns: 
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"
#' @return a list containing a count of the unique genes in the SIF and counts for the interaction types in the network
#' 
#' @examples
#' results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
#' summarizeSif(results)
#' 
#' @concept paxtoolsr
#' @export
summarizeSif <- function(sif) {
    uniqueNodes <- length(unique(c(sif[,1], sif[,3])))
    interactionTypeFreq <- table(sif[,2])
    
    results <- list(uniqueNodes=uniqueNodes, totalInteractions=nrow(sif), interactionTypeFreq=interactionTypeFreq)
    
    return(results) 
}  

#' Check Cache and Download File 
#' 
#' @param baseUrl a string, entire download URL except filename
#' @param fileName a string, the filename of file to be downloaded 
#' @param cacheEnv a string, the environment variable that points to the specific cache
#' @param destDir a string, the path where a file should be saved 
#' @return a boolean TRUE if the file was downloaded or already exists, FALSE otherwise
#' 
#' @details 
#' Description of file formats: http://www.pathwaycommons.org/pc2/formats
#' 
#' @examples 
#' downloadFile("http://google.com/", fileName="index.html", destDir=tempdir())
#' 
#' @concept paxtoolsr
#' @seealso \code{\link{readSif}, \link{readBiopax}, \link{readSbgn}, \link{readSifnx}, \link{readGmt}}
#' @export
#' 
#' @importFrom httr HEAD GET http_status write_disk progress add_headers http_date
downloadFile <- function(baseUrl, fileName, cacheEnv="PAXTOOLSR_CACHE", destDir=NULL) {
    cacheMapPath <- file.path(Sys.getenv(cacheEnv), "cacheMap.txt")
    cacheMap <- read.table(cacheMapPath, sep="\t", header=TRUE, stringsAsFactors=FALSE)
    
    url <- URLencode(paste0(baseUrl, fileName))
    
    if(!is.null(destDir)) {
        filePath <- file.path(destDir, fileName)
    } else {
        filePath <- file.path(Sys.getenv(cacheEnv), fileName)
    }
    
    fileIdx <- which(cacheMap[,"fileName"] == fileName)
    
    if(length(fileIdx) == 0) {
        headResp <- HEAD(url, add_headers("If-Modified-Since"=""))    
    } else {
        headResp <- HEAD(url, add_headers("If-Modified-Since"=cacheMap[fileIdx,"retrievedDate"]))   
    }
    
    httpStatus <- http_status(headResp)
    
    if(httpStatus$category == "success") {
        getResp <- GET(url, write_disk(filePath, overwrite=TRUE), progress())
        
        # Current date
        retrievedDate <- http_date(as.POSIXlt(Sys.time(), "GMT"))
        
        if(length(fileIdx) == 0) {
            cacheMap <- rbind(cacheMap, data.frame(fileName=fileName, retrievedDate=retrievedDate, url=url))
        } else {
            cacheMap[fileIdx,"fileName"] <- fileName
            cacheMap[fileIdx,"retrievedDate"] <- retrievedDate
            cacheMap[fileIdx,"url"] <- url
        }
        
        write.table(cacheMap, file=cacheMapPath, quote=FALSE, sep="\t", row.names=FALSE)
    }
    
    if(length(fileIdx) != 0 || httpStatus$category == "success") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Download Pathway Commons files (uses menu and cache)
#' 
#' @param destDir a string, the destination directory for the file to be 
#'   downloaded (Default: NULL). If NULL, then file will be downloaded to cache
#'   directory file.path(Sys.getenv("HOME"), ".paxtoolsRCache")
#'   
#' @return an R object using one of the read* methods provided in this package 
#'   corresponding to the file downloaded 
#'   
#' @examples 
#' \dontrun{
#'   downloadPc2(tempdir())
#' }
#'   
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom R.utils gunzip
downloadPc2 <- function(destDir=NULL) {
    baseUrl <- "http://www.pathwaycommons.org"
    downloadsSubDir <- "/pc2/downloads/"
    
    doHack <- FALSE
    
    if(!doHack) {
        # Parse webpage
        doc <- htmlParse(paste0(baseUrl, downloadsSubDir)) 
        
        # Extract links
        links <- xpathSApply(doc, "//a/@href")
        
        # Process links; get only gzipped files
        idx <- grepl(".gz", links)
        tmp <- strsplit(links[idx], ";")
        tmp2 <- lapply(tmp, function(x) { x[1] }) 
        tmp3 <- unname(unlist(tmp2))        
        
        filenames <- gsub(downloadsSubDir, "", tmp3)
    } else {
        # TODO Fix this hack 
        filenames <- c("Pathway Commons.7.All.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.All.BIOPAX.owl.gz", "Pathway Commons.7.All.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.All.GSEA.hgnc.gmt.gz", "Pathway Commons.7.All.GSEA.uniprot.gmt.gz", "Pathway Commons.7.BIND.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.BIND.BIOPAX.owl.gz", "Pathway Commons.7.BIND.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.BioGRID.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.BioGRID.BIOPAX.owl.gz", "Pathway Commons.7.BioGRID.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.CORUM.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.CORUM.BIOPAX.owl.gz", "Pathway Commons.7.CORUM.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Comparative Toxicogenomics Database.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Comparative Toxicogenomics Database.BIOPAX.owl.gz", "Pathway Commons.7.Comparative Toxicogenomics Database.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Comparative Toxicogenomics Database.GSEA.hgnc.gmt.gz", "Pathway Commons.7.Comparative Toxicogenomics Database.GSEA.uniprot.gmt.gz", "Pathway Commons.7.Database of Interacting Proteins.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Database of Interacting Proteins.BIOPAX.owl.gz", "Pathway Commons.7.Database of Interacting Proteins.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Detailed.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Detailed.BIOPAX.owl.gz", "Pathway Commons.7.Detailed.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Detailed.GSEA.hgnc.gmt.gz", "Pathway Commons.7.Detailed.GSEA.uniprot.gmt.gz", "Pathway Commons.7.DrugBank.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.DrugBank.BIOPAX.owl.gz", "Pathway Commons.7.DrugBank.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.HPRD.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.HPRD.BIOPAX.owl.gz", "Pathway Commons.7.HPRD.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.HumanCyc.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.HumanCyc.BIOPAX.owl.gz", "Pathway Commons.7.HumanCyc.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.HumanCyc.GSEA.hgnc.gmt.gz", "Pathway Commons.7.HumanCyc.GSEA.uniprot.gmt.gz", "Pathway Commons.7.IntAct.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.IntAct.BIOPAX.owl.gz", "Pathway Commons.7.IntAct.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.KEGG Pathway.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.KEGG Pathway.BIOPAX.owl.gz", "Pathway Commons.7.KEGG Pathway.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.KEGG Pathway.GSEA.hgnc.gmt.gz", "Pathway Commons.7.KEGG Pathway.GSEA.uniprot.gmt.gz", "Pathway Commons.7.MiRTarBase.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.MiRTarBase.BIOPAX.owl.gz", "Pathway Commons.7.MiRTarBase.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.MiRTarBase.GSEA.hgnc.gmt.gz", "Pathway Commons.7.MiRTarBase.GSEA.uniprot.gmt.gz", "Pathway Commons.7.PANTHER Pathway.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.PANTHER Pathway.BIOPAX.owl.gz", "Pathway Commons.7.PANTHER Pathway.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.PANTHER Pathway.GSEA.hgnc.gmt.gz", "Pathway Commons.7.PANTHER Pathway.GSEA.uniprot.gmt.gz", "Pathway Commons.7.PhosphoSitePlus.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.PhosphoSitePlus.BIOPAX.owl.gz", "Pathway Commons.7.PhosphoSitePlus.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Reactome.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Reactome.BIOPAX.owl.gz", "Pathway Commons.7.Reactome.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Reactome.GSEA.hgnc.gmt.gz", "Pathway Commons.7.Reactome.GSEA.uniprot.gmt.gz", "Pathway Commons.7.Recon X.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Recon X.BIOPAX.owl.gz", "Pathway Commons.7.Recon X.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Recon X.GSEA.hgnc.gmt.gz", "Pathway Commons.7.Recon X.GSEA.uniprot.gmt.gz", "Pathway Commons.7.TRANSFAC.BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.TRANSFAC.BIOPAX.owl.gz", "Pathway Commons.7.TRANSFAC.EXTENDED_BINARY_SIF.hgnc.sif.gz", "Pathway Commons.7.Warehouse.BIOPAX.owl.gz")
    }
    
    # Construct URLs
    tmp3 <- paste0(baseUrl, downloadsSubDir, filenames)
    
    # Download file
    selectedFileName <- select.list(filenames, graphics=FALSE)

    if(is.null(destDir)) {
        stopifnot(Sys.getenv("PAXTOOLSR_CACHE") != "")
        selectedFilePath <- file.path(Sys.getenv("PAXTOOLSR_CACHE"), selectedFileName)
    } else {
        selectedFilePath <- file.path(destDir, selectedFileName)
    }
    
    # NOTE: File not found if not first URL encoded
    tmpUrl <- paste0(baseUrl, downloadsSubDir)
    downloadResult <- downloadFile(baseUrl=tmpUrl, fileName=selectedFileName, destDir=destDir)
    
    #DEBUG
    #str(tmpUrl)
    #str(selectedFileName)
    #str(destDir)
    
    stopifnot(downloadResult)
    
    tmpFile <- gunzip(selectedFilePath, remove=FALSE, temporary=TRUE, skip=TRUE)
    
    # Parse GMT 
    if(grepl("GSEA", selectedFileName)) {
        results <- readGmt(tmpFile)
        return(results)
    }      
    
    # Parse EXTENDED_BINARY_SIF
    if(grepl("EXTENDED_BINARY_SIF", selectedFileName)) {
        results <- readSifnx(tmpFile)
        return(results)
    }      
    
    # Parse BINARY_SIF
    if(grepl("BINARY_SIF", selectedFileName)) {
        results <- readSif(tmpFile)
        return(results)
    }  
    
    # Parse BIOPAX
    if(grepl("BIOPAX", selectedFileName)) {
        results <- readBiopax(tmpFile)
        return(results)
    }
}

#' List files in cache directory 
#' 
#' @return a vector of the files in the cache directory
#' 
#' @examples 
#' getCacheFiles()
#' 
#' @concept paxtoolsr
#' @export
getCacheFiles <- function() {
    return(dir(Sys.getenv("PAXTOOLSR_CACHE")))
}
