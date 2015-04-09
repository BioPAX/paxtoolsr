#' Read PSIMI file
#' 
#' This function reads in a PSIMI file. 
#' 
#' @param inputFile a string of the name of the input PSIMI file
#' @param outputFile a string of the name of the output BioPAX OWL file
#' @param bpLevelArg a string representing the BioPAX level for the output file
#'   (default: NULL)
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details The Proteomics Standard Initiative (PSIMI) format is described at 
#' https://code.google.com/p/psimi/wiki/PsimiTabFormat
#' 
#' @examples
#' outFile <- tempfile()
#' results <- fromPsimi(system.file("extdata", "10523676-compact.xml", package="paxtoolsr"), 
#'                                  outFile, 
#'                                  "3")
#'                                  
#' @concept paxtoolsr
#' @export
fromPsimi <- function(inputFile, outputFile=NULL, bpLevelArg=3) {
    command <- "fromPsimi"
    
    outputFile <- checkOutputFile(outputFile)
    bpLevelArg <- as.character(bpLevelArg)
    
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)
    bpLevelArgJStr <- .jnew("java/lang/String", bpLevelArg)

    argsList <- list(commandJStr, bpLevelArgJStr, inputJStr, outputJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
    
}

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
#' @return a vector with the GSEA content
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
toGSEA <- function(inputFile, outputFile=NULL, database, crossSpeciesCheckFlag) {
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
    
    tmp <- read.table(outputFile, sep="\t", as.is=TRUE, fill=TRUE)
    
    # as.vector(unlist()) to remove column names from tmp
    results <- list(name=tmp[,1], 
                    description=tmp[,2], 
                    geneSet=as.vector(unlist(tmp[,3:length(tmp)])))
    
    return(results)
}

#' Get the neighbors of a set of IDs in a BioPAX file
#' 
#' This function retrieves a set of neighbors for a set of IDs in a BioPAX file.
#' 
#' @param inputFile a string with the name of the input BioPAX OWL file
#' @param outputFile a string with the name of the output BioPAX OWL file
#' @param idList a vector of IDs from the BioPAX OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details Only entities in the input BioPAX file will be searched for neighbors.
#' IDs used must be URIs for the entities of interest. 
#' 
#' @examples
#' outFile <- tempfile()
#' results <- getNeighbors(system.file("extdata", 
#'   "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr"), 
#'   outFile, 
#'   c("HTTP://WWW.REACTOME.ORG/BIOPAX/48887#PROTEIN2360_1_9606", 
#'     "HTTP://WWW.REACTOME.ORG/BIOPAX/48887#PROTEIN1631_1_9606")) 
#' 
#' @concept paxtoolsr
#' @export
getNeighbors <- function(inputFile, outputFile=NULL, idList) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    idList <- paste(idList, collapse=",")

    command <- "getNeighbors"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)

    idListJStr <- .jnew("java/lang/String", idList)
    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, inputJStr, idListJStr, outputJStr) 
    #DEBUG 
    #cat("ARGSLIST:", commandJStr, "\n")
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}

#' Fetch a set of IDs from a BioPAX OWL file
#'
#' This function will create a subsetted object with specified URIs. 
#'
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string with the name of the output BioPAX OWL file
#' @param idList a vector of IDs from the BioPAX OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details Only entities in the input BioPAX file will be used in the fetch.
#' IDs used must be URIs for the entities of interest. Additional properties 
#' such as cross-references for fetched entities will be included in the output. 
#' 
#' @examples 
#' outFile <- tempfile()
#' ids <- c("http://identifiers.org/uniprot/P36894", 
#'          "http://identifiers.org/uniprot/Q13873")
#' results <- fetch(system.file("extdata", "REACT_12034-3.owl", package="paxtoolsr"), 
#'                  outFile, ids)
#' 
#' @concept paxtoolsr
#' @export
fetch <- function(inputFile, outputFile=NULL, idList) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    idList <- paste(idList, collapse=",")
    
    command <- "fetch"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)

    idListJStr <- .jnew("java/lang/String", idList)
    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, inputJStr, idListJStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}

#' Convert a BioPAX OWL file to BioPAX Level 3
#' 
#' This file will convert older BioPAX objects to BioPAX Level 3
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output BioPAX OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @examples
#' outFile <- tempfile()
#' results <- toLevel3(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   outFile) 
#' 
#' @concept paxtoolsr
#' @export
toLevel3 <- function(inputFile, outputFile=NULL) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "toLevel3" 

    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, inputJStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}

#' Convert a BioPAX OWL file to SBGNML
#' 
#' This function will convert a BioPAX OWL file into the Systems Biology Graphical
#' Notation (SBGN) Markup Language (SBGNML) XML representation 
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output SBGNML file
#' @return an XMLInternalDocument representing a SBGNML file
#'
#' @details Objects in the SBGNML format are laid out using a Compound Spring 
#' Embedder (CoSE) layout
#'
#' @references \url{http://www.cs.bilkent.edu.tr/~ivis/layout/cose-animated-demo/cose.html}
#'
#' @examples 
#' outFile <- tempfile()
#' results <- toSBGN(system.file("extdata", "biopax3-short-metabolic-pathway.owl", 
#'   package="paxtoolsr"), 
#'   outFile) 
#' 
#' @concept paxtoolsr
#' @export
toSBGN <- function(inputFile, outputFile=NULL) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "toSBGN"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, inputJStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}

# toSifnx("<file1> <outEdges> <outNodes> <node-prop1,node-prop2,..> 
# <edge-prop1,edge-prop2,...>\n" +
# converter.writeInteractionsInSIFNX(m, out, out, 
# "EntityReference/name,EntityReference/xref" "Interaction/dataSource/displayName"

#' Converts BioPAX OWL file to extended binary SIF representation
#' 
#' @param inputFile a string with the name of the input BioPAX OWL file
#' @param outputNodesFile a string with the name of the output file for node 
#'   information
#' @param outputEdgesFile a string with the name of the output file for edge 
#'   information
#' @param nodeProps a string node properties to be saved; these are set up as XPath like 
#'   expressions of data in BioPAX files (e.g. c("EntityReference/name", "EntityReference/xref"))
#' @param edgeProps a string edge properties to be saved; these are set up as XPath like 
#'   expressions of data in BioPAX files (e.g. "Interaction/dataSource/displayName")
#' 
#' @return a list with two entries 
#' \itemize{
#'   \item nodes a data.frame with interaction participant information specified
#'     in nodeProps
#'   \item edges a data.frame with SIF formatted data (i.e. an edgelist with a 
#'     additional middle column denoting interaction type) and any additional 
#'     columns specified in edgeProps.
#' }
#' 
#' @details Information on SIF conversion is provided on the Pathway Commons 
#'   site: \url{http://www.pathwaycommons.org/pc2/}
#' 
#' @examples
#' edgesFile <- tempfile()
#' nodesFile <- tempfile()
#' results <- toSifnx(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   nodesFile,
#'   edgesFile, 
#'   c("EntityReference/name", "EntityReference/xref"), 
#'   "Interaction/dataSource/displayName") 
#' 
#' @concept paxtoolsr
#' @export
toSifnx <- function(inputFile, outputNodesFile=NULL, outputEdgesFile=NULL, 
                    nodeProps, edgeProps) {
    inputFile <- checkInputFile(inputFile)
    outputEdgesFile <- checkOutputFile(outputEdgesFile)
    outputNodesFile <- checkOutputFile(outputNodesFile)

    nodePropsCollapsed <- paste(nodeProps, collapse=",")
    edgePropsCollapsed <- paste(edgeProps, collapse=",")
    
    #DEBUG 
    #str(nodeProps)
    
    command <- "toSifnx"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    outputEdgesJStr <- .jnew("java/lang/String", outputEdgesFile)       
    outputNodesJStr <- .jnew("java/lang/String", outputNodesFile)

    nodePropsJStr <- .jnew("java/lang/String", nodePropsCollapsed)
    edgePropsJStr <- .jnew("java/lang/String", edgePropsCollapsed)

    argsList <- list(commandJStr, inputJStr, outputEdgesJStr, outputNodesJStr, 
                     nodePropsJStr, edgePropsJStr) 
    
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 
    
    edges <- read.table(outputEdgesFile, sep="\t", as.is=TRUE, quote="", 
                        fill=TRUE)
    nodes <- read.table(outputNodesFile, sep="\t", as.is=TRUE, quote="", 
                        fill=TRUE)
    
    colnames(edges) <- c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B", edgeProps)
    colnames(nodes) <- c("PARTICIPANT", nodeProps)
    
    results <- list(edges=edges, nodes=nodes)
    
    return(results)
}

#' Convert a BioPAX OWL file to SIF
#' 
#' Convert a BioPAX OWL file to a binary SIF file 
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output SIF file (Optional)
#' @return a 3-column data.frame where the columns are named: PARTICIPANT_A, 
#'   INTERACTION_TYPE, PARTICIPANT_B
#'   
#' @details Information on SIF conversion is provided on the Pathway Commons 
#'   site: \url{http://www.pathwaycommons.org/pc2/}
#'  
#' @examples   
#' outFile <- tempfile()
#' results <- toSif(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   outFile) 
#'   
#' @concept paxtoolsr
#' @export
toSif <- function(inputFile, outputFile=NULL) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- checkOutputFile(outputFile)

    #DEBUG 
    #cat("OUTPUTFILE: ", outputFile)
    
    command <- "toSif"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, inputJStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck() 

    results <- read.table(outputFile, sep="\t", as.is=TRUE, quote="")
    colnames(results) <- c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B")
    
    return(results)        
}

#' Integrate two BioPAX OWL files (DEPRECATED)
#' 
#' This function merges two BioPAX OWL files 
#' 
#' @param inputFile1 a string of the name of the input BioPAX OWL file 
#' @param inputFile2 a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output integrated BioPAX 
#'   OWL file
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details This method is deprecated. Use mergeBiopax instead. 
#' 
#' @examples
#' outFile <- tempfile()
#' results <- integrateBiopax(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), 
#'   system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
#'   outFile) 
#' 
#' @concept paxtoolsr
#' @export
#' @seealso \code{\link{mergeBiopax}}
integrateBiopax <- function(inputFile1, inputFile2, outputFile=NULL) {
    inputFile1 <- checkInputFile(inputFile1)
    inputFile2 <- checkInputFile(inputFile2)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "integrate"
    commandJStr <- .jnew("java/lang/String", command)
    file1JStr <- .jnew("java/lang/String", inputFile1)
    file2JStr <- .jnew("java/lang/String", inputFile2)

    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, file1JStr, file2JStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}

#' Merges two BioPAX OWL files
#' 
#' This function merges two BioPAX OWL files 
#' 
#' @param inputFile1 a string of the name of the input BioPAX OWL file 
#' @param inputFile2 a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output merged BioPAX 
#'   OWL file (Optional)
#' @return an XMLInternalDocument representing a BioPAX OWL file
#' 
#' @details Only entities that share IDs will be merged. No additional merging
#'   occurs on cross-references. Merging may result in warning messages caused 
#'   as a result of redundant actions being checked against by the Java library; 
#'   these messages may be ignored. 
#' 
#' @examples    
#' outFile <- tempfile()
#' results <- mergeBiopax(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'                        package="paxtoolsr"), 
#'                        system.file("extdata", "dna_replication.owl", 
#'                        package="paxtoolsr"), 
#'                        outFile) 
#' 
#' @concept paxtoolsr
#' @export
mergeBiopax <- function(inputFile1, inputFile2, outputFile=NULL) {
    inputFile1 <- checkInputFile(inputFile1)
    inputFile2 <- checkInputFile(inputFile2)
    outputFile <- checkOutputFile(outputFile)
    
    command <- "merge"
    commandJStr <- .jnew("java/lang/String", command)
    file1JStr <- .jnew("java/lang/String", inputFile1)
    file2JStr <- .jnew("java/lang/String", inputFile2)

    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, file1JStr, file2JStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
    return(results) 
}

#' Summarize a BioPAX file
#' 
#' This function provides a summary of BioPAX classes.
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file 
#' @return list with BioPAX class counts 
#' 
#' @details BioPAX classes are defined by the BioPAX specification: 
#'   \url{http://www.biopax.org/}
#' 
#' @examples
#' summary <- summarize(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#' package="paxtoolsr")) 
#' 
#' @concept paxtoolsr
#' @export
summarize <- function(inputFile) {
    inputFile <- checkInputFile(inputFile)
    outputFile <- tempfile()

    command <- "summarize"
    commandJStr <- .jnew("java/lang/String", command)
    inputJStr <- .jnew("java/lang/String", inputFile)
    outputJStr <- .jnew("java/lang/String", outputFile)

    argsList <- list(commandJStr, inputJStr, outputJStr) 

    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
        
    # Make a vector with each line text as a vector entry
    lines <- readLines(outputFile)
    
    results <- list()
    
    for(line in lines) {        

        # Makes sure a line matches
        if(grepl("^[A-Za-z]+\\s=\\s\\d+", line)) {

            # Removes any characters at the end of the line that probably have parentheses
            tmp <- gsub("^([A-Za-z]+)\\s=\\s(\\d+).*", "\\1=\\2", line) 

            # Produces a vector with two entries 
            tmp2 <- strsplit(tmp, "=")[[1]]
            results[[tmp2[1]]] <- tmp2[2]
        }
    }
        
    return(results)
}

#' Validate BioPAX files 
#' 
#' This function validates BioPAX files for errors.
#' 
#' @param inputFile a string of the name of the input BioPAX OWL file
#' @param outputFile a string of the name of the output file containing 
#'   validation results
#' @param type a string denoting the type of output: xml (default), html, biopax 
#' @param autoFix a boolean that determines if the input file should be 
#'   fixed automatically. Errors that can be automatically fixed include 
#'   generating displayName properties from names, inferring organism, and 
#'   inferring dataSource
#' @param onlyErrors a boolean of whether to only display errors
#' @param maxErrors a integer denoting the number of errors to return 
#' @param notStrict a boolean of whether to be strict in validation (default: FALSE)
#' 
#' @return an XMLInternalDocument is returned if type is set to "xml" otherwise
#'   the location of the outputfile is returned.  
#' 
#' @details See the publication by Rodchenkov, et al. for information on the 
#'   BioPAX validator. See \url{http://biopax.baderlab.org/validator} for 
#'   additional information on validator. 
#'   See \url{http://biopax.baderlab.org/validator/errorTypes.html} for 
#'   information on error types. 
#' 
#' @references Rodchenkov I, Demir E, Sander C, Bader GD. The BioPAX Validator, 
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/23918249}
#' 
#' @examples
#' outFile <- tempfile()
#' rawDoc <- validate(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", 
#'   package="paxtoolsr"), onlyErrors=TRUE) 
#' 
#' @concept paxtoolsr
#' @export
validate <- function(inputFile, outputFile=NULL, 
                     type=c("xml", "html", "biopax"),
                     autoFix=FALSE, onlyErrors=FALSE, maxErrors=NULL, 
                     notStrict=FALSE) {
    #DEBUG 
    #inputFile <- files[i]
    #outputfile <- NULL
    #type <- NULL
    #autoFix <- FALSE
    #onlyErrors <- FALSE
    #maxErrors <- NULL
    #notStrict <- TRUE

    inputFile <- checkInputFile(inputFile)
    type <- match.arg(type)
    
    command <- "validate"
    commandJStr <- .jnew("java/lang/String", command)
    
    inputJStr <- .jnew("java/lang/String", inputFile)
    
    outputFile <- checkOutputFile(outputFile)
    
    outputJStr <- .jnew("java/lang/String", outputFile)
    
    if(is.null(type)) {
        outputTypeJStr <- .jnew("java/lang/String", "xml")
    } else {
        outputTypeJStr <- .jnew("java/lang/String", type)       
    }
    
    argsList <- list(commandJStr, inputJStr, outputJStr, outputTypeJStr)

    if(autoFix) {
        autoFixJStr <- .jnew("java/lang/String", "auto-fix")
        argsList <- append(argsList, autoFixJStr)
    }
    
    if(onlyErrors) {
        onlyErrorsJStr <- .jnew("java/lang/String", "only-errors")
        argsList <- append(argsList, onlyErrorsJStr)
    }
    
    if(!is.null(maxErrors)) {
        maxErrorsJStr <- .jnew("java/lang/String", paste("maxerrors=", 
                               maxErrors, sep=""))
        argsList <- append(argsList, maxErrorsJStr)
    }

    if(notStrict) {
        nonStrictJStr <- .jnew("java/lang/String", "notstrict")
        argsList <- append(argsList, nonStrictJStr)     
    }
            
    .jcall("org/biopax/paxtools/PaxtoolsMain","V",command,.jarray(argsList, "java/lang/String"))
    .jcheck()
    
    if(type == "xml") { 
        results <- xmlTreeParse(outputFile, useInternalNodes=TRUE)
        return(results)
    } else {
        return(outputFile) 
    }
}

#' Read in gene sets from GMT files 
#'
#' This function will read in gene sets in the GMT format into a named list. 
#' 
#' @param inputFile an inputFile in the GMT format
#' @return a named list where each entry corresponds to a gene set
#' 
#' @examples 
#' results <- readGmt(system.file("extdata", "test_gsea.gmt", package="paxtoolsr"))
#' 
#' @concept paxtoolsr
#' @export
readGmt <- function(inputFile) {
    f <- readLines(inputFile)
    lst <- sapply(f, function(x) unlist(strsplit(x, "\t", fixed = TRUE)))
    names(lst) <- sapply(lst, function(x) x[1])
    lst <- lapply(lst, function(x) x[-(1:2)])
    return(lst)
}
 
#' Utility method; create temporary file if necessary
#' 
#' @param file a string or XMLInternalDocument
#' @return location of file 
#' 
#' @concept paxtoolsr
#' @noRd
checkInputFile <- function(file) {
    if(typeof(file) == "externalptr") {
        tmp <- tempfile()
        saveXML(file, tmp)
        
        file <- tmp
    }
    
    return(file)
}

#' Utility method; create temporary file if necessary
#' 
#' @param file a string
#' @return location of file 
#' 
#' @concept paxtoolsr
#' @noRd
checkOutputFile <- function(file) {
    if(is.null(file)) {
        file <- tempfile() 
    }    
    
    return(file)
}
