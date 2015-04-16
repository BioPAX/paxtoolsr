#' Search Pathway Commons
#' 
#' This command provides a text search using the Lucene query syntax. 
#' 
#' @param q a keyword, name, external identifier, or a Lucene query string.
#' @param page an integer giving the search result page number (N>=0, default:
#'   0)
#' @param datasource a vector that is a filter by data source (use names or URIs
#'   of pathway data sources or of any existing Provenance object). If multiple
#'   data source values are specified, a union of hits from specified sources is
#'   returned. For example, datasource as c("reactome", "pid") returns hits
#'   associated with Reactome or PID.
#' @param organism a vector that is an organism filter. The organism can be
#'   specified either by official name, e.g. "homo sapiens" or by NCBI taxonomy
#'   id, e.g. "9606". Similar to data sources, if multiple organisms are
#'   declared a union of all hits from specified organisms is returned. For
#'   example organism as c("9606", "10016") returns results for both human and
#'   mice. Only humans, "9606" is officially supported.
#' @param type BioPAX class filter. See Details.
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return an XMLInternalDocument with results
#' 
#' @details Indexed fields were selected based on most common searches. Some of 
#' these fields are direct BioPAX properties, others are composite
#' relationships. All index fields are (case-sensitive):comment, ecnumber,
#' keyword, name, pathway, term, xrefdb, xrefid, dataSource, and organism. The
#' pathway field maps to all participants of pathways that contain the
#' keyword(s) in any of its text fields. This field is transitive in the sense
#' that participants of all sub-pathways are also returned. Finally, keyword is
#' a transitive aggregate field that includes all searchable keywords of that
#' element and its child elements - e.g. a complex would be returned by a
#' keyword search if one of its members has a match. Keyword is the default
#' field type. All searches can also be filtered by data source and organism. It
#' is also possible to restrict the domain class using the 'type' parameter.
#' This query can be used standalone or to retrieve starting points for graph
#' searches. Search strings are case insensitive unless put inside quotes.
#' 
#' BioPAX classes can be found at \url{http://www.pathwaycommons.org/pc2/#biopax_types}
#' 
#' @examples
#' query <- "Q06609"
#' #results <- searchPc(query)
#' 
#' query <- "glycolysis"
#' #results <- searchPc(query, type="Pathway")
#' 
#' @concept paxtoolsr
#' @export
searchPc <- function(q, page=0, datasource=NULL, organism=NULL, type=NULL, 
                     verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "search.xml?q=")
    url <- paste0(baseUrl, q, "&page=", page) 
    
    if(!is.null(datasource)) {
        # Put into the correct format
        datasources <- paste(paste0("datasource=", datasource), collapse="&")
        url <- paste(url, "&", datasources, sep="")
    }
    
    if(!is.null(organism)) {
        organisms <- paste(paste0("organism=", organism), collapse="&")
        url <- paste(url, "&", organisms, sep="")
    }
    
    if(!is.null(type)) {
        url <- paste(url, "&type=", type, sep="")
    }

    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    
    return(results)
}

#' Get Pathway Commons BioPAX elements
#' 
#' This command retrieves full pathway information for a set of elements such as
#' pathway, interaction or physical entity given the RDF IDs. 
#' 
#' @param uri a vector that includes valid/existing BioPAX element's URI (RDF
#'   ID; for utility classes that were "normalized", such as entity refereneces
#'   and controlled vocabularies, it is usually a Idntifiers.org URL. Multiple
#'   IDs are allowed per query, for example,
#'   c("http://identifiers.org/uniprot/Q06609",
#'   "http://identifiers.org/uniprot/Q549Z0") See also about MIRIAM and
#'   Identifiers.org in details. 
#' @param format output format (Default: BIOPAX). Valid options can be found using 
#'   \code{\link{pcFormats}}
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return a XMLInternalDocument object
#' 
#' @details Get commands only retrieve the BioPAX elements that are directly
#'   mapped to the ID. Use the "traverse query to traverse BioPAX graph and
#'   obtain child/owner elements.
#'   
#'   Information on MIRIAM and Identifiers.org 
#'   \url{http://www.pathwaycommons.org/pc2/#miriam}
#' 
#' @seealso \code{\link{pcFormats}}
#' 
#' @examples 
#' uri <- "http://identifiers.org/uniprot/O14503"
#' #results <- getPc(uri)
#' 
#' uri <- c("http://identifiers.org/uniprot/O14503", "http://identifiers.org/uniprot/Q9P2X7")
#' #results <- getPc(uri, verbose=TRUE)
#' 
#' @concept paxtoolsr
#' @export
getPc <- function(uri, format="BIOPAX", verbose=FALSE) {
    uris <- paste(paste0("uri=", uri), collapse="&")
    
    baseUrl <- paste0(getPcUrl(), "get?")
    url <- paste(baseUrl, uris, sep="") 
            
    stopifnot(format %in% pcFormats())
    
    if(!is.null(format)) {
        url <- paste0(url, "&format=", format)
    }
    
    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, format)
    return(results) 
}

#' Get Pathway Commons BioPAX elements
#' 
#' This function will retrieve a set of BioPAX elements given a graph query match. 
#' 
#' @param kind graph query. Valid options can be found using \code{\link{pcGraphQueries}}
#'   See Details for information on graph queries. 
#' @param source source object's URI/ID. Multiple source URIs/IDs are allowed
#'   per query, for example c("http://identifiers.org/uniprot/Q06609", 
#'   "http://identifiers.org/uniprot/Q549Z0")
#'   See a note about MIRIAM and Identifiers.org in details
#' @param target [Required for PATHSFROMTO graph query] target URI/ID. Multiple
#'   target URIs are allowed per query; for example c("http://identifiers.org/uniprot/Q06609", 
#'   "http://identifiers.org/uniprot/Q549Z0")
#'   See a note about MIRIAM and Identifiers.org in details
#' @param direction [Optional, for NEIGHBORHOOD and COMMONSTREAM algorithms] -
#'   graph search direction. Valid options: \code{\link{pcDirections}}.
#' @param limit graph query search distance limit (default: 1).
#' @param format output format. Valid options: \code{\link{pcFormats}}
#' @param datasource datasource filter (same as for 'search').
#' @param organism organism filter (same as for 'search').
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return depending on the the output format a different object may be returned. 
#'   \code{\link{pcFormats}}
#' 
#' @seealso \code{\link{pcFormats}, \link{pcDirections}}
#' 
#' @examples
#' source <- "http://identifiers.org/uniprot/O14503"
#' #results <- graphPc(source=, kind="neighborhood", format="EXTENDED_BINARY_SIF")
#' 
#' @concept paxtoolsr
#' @export 
graphPc <- function(kind, source, target=NULL, direction=NULL, limit=NULL, 
                    format=NULL, datasource=NULL, organism=NULL, 
                    verbose=FALSE) {
    # Pre-process arguments 
    ## Convert to uppercase to avoid issues in if statements
    format <- toupper(format)
    
    baseUrl <- paste0(getPcUrl(), "graph?kind=")
    url <- paste(baseUrl, kind, sep="") 
    
    stopifnot(format %in% pcFormats())
    
    if(!is.null(source)) {
        # Put into the correct format
        sources <- paste(paste0("source=", source), collapse="&")
        url <- paste(url, "&", sources, sep="")
    }
    
    #DEBUG 
    #cat("TARGET: ", target, "\n")
    #cat("KIND: ", kind, "\n")
    
    if(kind == "PATHSFROMTO") {
        if(!is.null(target)) {
            targets <- paste(paste0("target=", target), collapse="&")
            url <- paste(url, "&", targets, sep="")
        } else {
            stop("target must be set if kind is PATHSFROMTO")
        }
    }
    
    if(!is.null(direction)) {
        direction <- toupper(direction)     
        stopifnot(direction %in% pcDirections())
        
        url <- paste(url, "&direction=", direction, sep="")
    }

    if(!is.null(limit)) {
        url <- paste(url, "&limit=", limit, sep="")
    }

    if(!is.null(format)) {
        url <- paste(url, "&format=", format, sep="")
    }

    if(!is.null(datasource)) {
        url <- paste(url, "&datasource=", datasource, sep="")
    }

    if(!is.null(organism)) {
        url <- paste(url, "&organism=", organism, sep="")
    }

    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, format)
    return(results)
}

#' Process Pathway Commons request in various formats
#' 
#' @param content a string, content to be processed
#' @param format a string, the type of format
#' 
#' @return an R object using one of the read* methods provided in this package 
#'   corresponding to the format
#' 
#' @examples 
#' fileName <- system.file("extdata", "test_biopax.owl", package="paxtoolsr")
#' content <- readChar(fileName, file.info(fileName)$size)
#' results <- processPcRequest(content, "BIOPAX")
#' 
#' @seealso \code{\link{pcFormats}}
#' 
#' @concept paxtoolsr
#' @export 
#' 
#' @importFrom rjson fromJSON
processPcRequest <- function(content, format) {
    if(format == "JSON") {     
        results <- fromJSON(content)
        return(results)
    } else if(format == "XML") {
        results <- xmlTreeParse(content, useInternalNodes=TRUE)
        return(results)
    }
    
    filename <- tempfile() 
    write(content, file=filename) 
    stopifnot(file.info(filename)$size > 0)
    
    #DEBUG 
    #cat("FILENAME: ", filename, "\n") 

    if(format == "EXTENDED_BINARY_SIF") {
        results <- readSifnx(filename) 
    } else if(format == "BINARY_SIF") {
        results <- readSif(filename)           
    } else if(format == "BIOPAX") {     
        results <- readBiopax(filename)
    } else if(format == "SBGN") {     
        results <- readSbgn(filename)
    } else if(format == "GSEA") {     
        results <- readGmt(filename)
    } else {
        results <- content
    }
    
    return(results)
}

#' Access Pathway Commons using XPath-type expressions 
#' 
#' This command provides XPath-like access to the Pathway Commons. 
#' 
#' @param uri a BioPAX element URI - specified similarly to the 'GET' command
#'   above). Multiple IDs are allowed (uri=...&uri=...&uri=...).
#' @param path a BioPAX propery path in the form of
#'   property1[:type1]/property2[:type2]; see properties, inverse properties,
#'   Paxtools, org.biopax.paxtools.controller.PathAccessor.
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return an XMLInternalDocument with results
#' 
#' @details With traverse users can explicitly state the paths they would like
#'   to access. The format of the path query is in the form: [Initial 
#'   Class]/[property1]:[classRestriction(optional)]/[property2]... A "*" sign 
#'   after the property instructs path accessor to transitively traverse that 
#'   property. For example, the following path accessor will traverse through
#'   all physical entity components within a complex: 
#'   "Complex/component*/entityReference/xref:UnificationXref" The following
#'   will list display names of all participants of interactions, which are
#'   components (pathwayComponent) of a pathway (note: pathwayOrder property,
#'   where same or other interactions can be reached, is not considered here): 
#'   "Pathway/pathwayComponent:Interaction/participant*/displayName" The
#'   optional parameter classRestriction allows to restrict/filter the returned
#'   property values to a certain subclass of the range of that property. In the
#'   first example above, this is used to get only the Unification Xrefs. Path
#'   accessors can use all the official BioPAX properties as well as additional
#'   derived classes and parameters in paxtools such as inverse parameters and
#'   interfaces that represent anonymous union classes in OWL. (See Paxtools
#'   documentation for more details).
#' 
#' @examples
#' uri <- "http://identifiers.org/uniprot/P38398"
#' #results <- traverse(uri=uri, path="ProteinReference/organism/displayName")
#' 
#' @references Paxtools Documentation: \url{http://www.biopax.org/m2site/}
#' 
#' @concept paxtoolsr
#' @export   
traverse <- function(uri, path, verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "traverse?")
    
    if(!is.null(uri)) {
        # Put into the correct format
        uris <- paste(paste0("uri=", uri), collapse="&")
        url <- paste(baseUrl, uris, sep="")
    }
    
    url <- paste(url, "&path=", path, sep="") 

    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    return(results) 
}

#' Retrieve top pathways
#' 
#' This command returns all "top" pathways.
#' 
#' @param datasource filter by data source (same as for 'search').
#' @param organism organism filter (same as for 'search').
#' @param verbose a boolean, display the command used to query Pathway Commons
#' @return a data.frame with the following columns:
#'   \itemize{ 
#'     \item uri URI ID for the pathway
#'     \item biopaxClass the type of BioPAX object
#'     \item name a human readable name
#'     \item dataSource the dataSource for the pathway 
#'     \item organism an organism identifier 
#'     \item pathway URI ID for the pathway
#'   }
#'   
#' @details Pathways that are neither 'controlled' nor 'pathwayComponent' of 
#'   another process.
#' 
#' @examples
#' datasource <- "panther"
#' #results <- topPathways(datasource=datasource)
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom plyr ldply
topPathways <- function(datasource=NULL, organism=NULL, verbose=FALSE) {
    baseUrl <- paste0(getPcUrl(), "top_pathways?")
    url <- baseUrl

    if(!is.null(datasource)) {
        url <- paste(url, "&datasource=", datasource, sep="")
    }

    if(!is.null(organism)) {
        url <- paste(url, "&organism=", organism, sep="")
    }

    tmp <- getPcRequest(url, verbose)
    results <- processPcRequest(tmp, "XML")
    
    #DEBUG
    #return(results)
    
    resultsDf <- ldply(xmlToList(results), data.frame, stringsAsFactors=FALSE)
    resultsDf <- resultsDf[,c("uri", "biopaxClass", "name", 
                              "dataSource", "organism")]
    
    # Remove NAs
    resultsDf <- resultsDf[which(!is.na(resultsDf[,"uri"])),]
    
    return(resultsDf) 
}

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

#' Acceptable Pathway Commons Formats 
#' 
#' A simple function to see valid options 
#' 
#' @return acceptable Pathway Commons formats 
#' 
#' @details See references.
#' 
#' @references Output Formats Description: \url{http://www.pathwaycommons.org/pc2/help/formats.html}
#' 
#' @examples
#' pcFormats()
#' 
#' @concept paxtoolsr
#' @export 
pcFormats <- function() {
    pcFormats <- c("BINARY_SIF", "BIOPAX", "EXTENDED_BINARY_SIF", "GSEA", "SBGN") 
    
    return(pcFormats)
}

#' Acceptable Pathway Commons Graph Queries 
#' 
#' A simple function to see valid options 
#' 
#' @return acceptable Pathway Commons graph queries  
#' 
#' @details 
#'   
#' \itemize{
#'   \item COMMONSTREAM searches common downstream or common upstream of a
#'   specified set of entities based on the given directions within the
#'   boundaries of a specified length limit
#'   \item NEIGHBORHOOD searches the neighborhood of given source set of nodes
#'   \item PATHSBETWEEN finds the paths between specific source set of states or
#'   entities within the boundaries of a specified length limit
#'   \item PATHSFROMTO finds the paths from a specific source set of states or
#'   entities to a specific target set of states or entities within the
#'   boundaries of a specified length limit
#' }
#' 
#' @examples
#' pcGraphQueries()
#' 
#' @concept paxtoolsr
#' @export 
pcGraphQueries <- function() {
    pcGraphQueries <- c("COMMONSTREAM", "NEIGHBORHOOD", "PATHSBETWEEN", 
                        "PATHSBETWEEN", "PATHSFROMTO")
    
    return(pcGraphQueries)
}

#' Acceptable Pathway Commons Directions
#' 
#' A simple function to see valid options 
#' 
#' @return acceptable Pathway Commons directions
#' 
#' @details 
#'
#' \itemize{
#'   \item BOTHSTREAM where the current entity can either be the source or 
#'     target of an interaction 
#'   \item DOWNSTREAM where the current entity can only be the source
#'   \item UPSTREAM where the current entity can only be the target
#' }
#' 
#' @examples 
#' pcDirections() 
#' 
#' @concept paxtoolsr
#' @export 
pcDirections <- function() {
    pcDirections <- c("BOTHSTREAM", "DOWNSTREAM", "UPSTREAM")
    
    return(pcDirections)
}

#' Get base Pathway Commons URL
#' 
#' @return a string with base Pathway Commons URL
#' 
#' @details paxtoolsr will support versions Pathway Commons 5 and later. Old 
#' versions of the webservice will not be not be operational. Users can parse
#' older BioPAX outputs as an alternative. 
#' 
#' @examples 
#' url <- getPcUrl()
#' 
#' @concept paxtoolsr
#' @export
#' 
#' @importFrom httr url_success
getPcUrl <- function() {
    url <- NULL
    
    #baseUrl <- "http://www.pathwaycommons.org/pc2"
    baseUrl <- "http://purl.org/pc2/"
    
    curUrl <- paste0(baseUrl, getOption("pc.version"), "/")
    tmpVersion <- as.numeric(getOption("pc.version")) + 1 
    nextUrl <- paste0(baseUrl, tmpVersion, "/")
    
    if(url_success(curUrl)) {
        url <- curUrl
    } 
    
    if(url_success(nextUrl)) {
        url <- nextUrl
    }     
    
    if(is.null(url)) {
        stop(paste("ERROR: Pathway Commons webservice cannot be reached. URLs tried:", curUrl, nextUrl))
    }
    
    return(url)
}

#' Get Error Message for a Pathway Commons Error 
#' 
#' @param code a three digit numerical error code
#' @return an error message for the code 
#' 
#' @examples 
#' \dontrun{
#'   results <- getErrorMessage("300")
#' }
#' 
#' @concept paxtoolsr
#' @keywords internal
#' @noRd
getErrorMessage <- function(code) {
    codes <- c("452", "460", "500", "503")
    messages <- c("Bad Request (illegal or no arguments)", 
                  "No Results Found", 
                  "Internal Server Error", 
                  "Server is temporarily unavailable due to regular maintenance")
    
    errors <- data.frame(codes=codes, messages=messages, stringsAsFactors=FALSE)
    
    message <- errors$messages[errors$codes==code]
    
    if(length(message)==1) {
        return(message)     
    } else {
        return("Unknown Error")
    }
}

#' Get a Pathway Commons Webservice Request 
#' 
#' @param url Pathway Commons webservice request URL 
#' @return request results 
#' 
#' @concept paxtoolsr
#' @keywords internal
#' @noRd
#' @importFrom httr HEAD GET content accept
getPcRequest <- function(url, verbose) {
    url <- URLencode(url)
    
    if(verbose) {
        cat("URL: ", url, "\n") 
    }
    
#     tmp <- tryCatch(getURLContent(url, .opts=list(followlocation=TRUE)),
#                     error=function(e) {
#                         #DEBUG
#                         #cat("X", e$message, "\n")
#                         
#                         code <- substr(e$message, 1, 3)
#                         
#                         # Make sure the code is numeric
#                         if(grepl("^\\d+$", code)) {
#                             message <- getErrorMessage(code)                                            
#                         } else {
#                             code <- NA
#                             message <- e$message
#                         }
#                         
#                         result <- paste("ERROR: Code:", code, "Message:", message)
#                         result
#                     })
#    
#    statusCode <- url.exists(url, .opts=list(followlocation=TRUE), .header=TRUE)["status"]
    statusCode <- HEAD(url)$status
    
    # Check HTTP status code; 200 is success 
    if(statusCode == "200") {
        #tmp <- getURLContent(url, .opts=list(followlocation=TRUE))
        
        #Set preference order of accept types
        tmp <- content(GET(url, accept("text/xml,text/plain,application/json")), as="text")
    } else {
        # Make sure the statusCode is numeric
        if(grepl("^\\d+$", statusCode)) {
            message <- getErrorMessage(statusCode)                                            
        } else {
            statusCode <- NA
            message <- NA
        }
        
        tmp <- paste("ERROR: Code:", statusCode, "Message:", message)
    }
    
    if(grepl("^ERROR", tmp)) {
        stop(paste(tmp, "(PC Webservice Error)"))
    }
    
    return(tmp)
}
