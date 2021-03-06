# All tests are done on files in package using system.file()

context("Pathway Commons Functionality")

test_that("getPcUrl", {
    expect_is(getPcUrl(), "character")
})

test_that("getErrorMessage", {
    expect_is(getErrorMessage("452"), "character")
})

test_that("pcDirections", {
    expect_is(pcDirections(), "character")
})

test_that("pcGraphQueries", {
    expect_is(pcGraphQueries(), "character")
})

test_that("pcFormats", {
    expect_is(pcFormats(), "list")
})

test_that("getPc", {
    outFile <- tempfile()   
    results <- getPc("http://identifiers.org/uniprot/Q06609")
    expect_is(results, "XMLInternalDocument")
    
    outFile <- tempfile()   
    results <- getPc(c("http://identifiers.org/uniprot/Q06609", 
                       "http://identifiers.org/uniprot/Q96EB6"), 
                     verbose=TRUE)
    expect_is(results, "XMLInternalDocument")
})

test_that("searchPc", {
    skip_on_bioc()
    
    #A basic text search. This query returns all entities that contain the 
    #"Q06609" keyword in XML
    outFile <- tempfile()
    results <- searchPc("Q06609", verbose=TRUE)
    expect_is(results, "XMLInternalDocument")
    
    #Search for ProteinReference entries that contain "brca2" keyword in any 
    #indexed field, return only human proteins from NCI Pathway Interaction 
    #Database
    outFile <- tempfile()
    results <- searchPc(q="brca2", datasource="pid", type="ProteinReference", 
                        organism="homo sapiens", verbose=TRUE)
    expect_is(results, "XMLInternalDocument")

    #Same as above with multiple datasources and organisms
    outFile <- tempfile()
    # searchPc_reactomePid.xml
    results <- searchPc(q="brca2", datasource=c("reactome", "pid"), type="ProteinReference", 
                        organism=c("9606", "10016"), verbose=TRUE)
    expect_is(results, "XMLInternalDocument")
})

test_that("getPc", {
    skip_on_bioc()
    
    outFile <- tempfile()   
    results <- getPc("http://identifiers.org/uniprot/Q06609")
    expect_is(results, "XMLInternalDocument")

    outFile <- tempfile()   
    results <- getPc(c("http://identifiers.org/uniprot/Q06609", 
                       "http://identifiers.org/uniprot/Q96EB6"), 
                     verbose=TRUE)
    expect_is(results, "XMLInternalDocument")
})

test_that("graphPc", {  
    skip_on_bioc()
    
    results <- graphPc(source="http://identifiers.org/uniprot/Q8N5Y8", 
                       kind="neighborhood", 
                       format="TXT", 
                       verbose=TRUE)
    expect_true(all(c("nodes", "edges") %in% names(results)))
    
    # Test Against Error Code 460
    # expect_error(graphPc(source="http://identifiers.org/uniprot/PXXXXX", 
    #                      kind="neighborhood", 
    #                      format="TXT", 
    #                      verbose=TRUE), ".*500.*PC Webservice Error.*")
    
    expect_error(graphPc(source="http://identifiers.org/uniprot/PXXXXX", 
                         kind="neighborhood", 
                         format="TXT", 
                         verbose=TRUE), "ERROR: Result was empty")

    expect_error(graphPc(source="http://identifiers.org/uniprot/O14503", 
                         kind="PATHSFROMTO", 
                         format="TXT", 
                         verbose=TRUE), "target must be set if kind is PATHSFROMTO")
    
    results <- graphPc(source="http://identifiers.org/uniprot/O14503", 
                       target="http://identifiers.org/uniprot/O00327",
                       kind="PATHSFROMTO", 
                       format="TXT", 
                       verbose=TRUE)
    expect_true(all(c("nodes", "edges") %in% names(results)))
    
    expect_is(graphPc(source=c("http://identifiers.org/uniprot/Q06609", 
                               "http://identifiers.org/uniprot/Q96EB6"), 
                         kind="neighborhood", 
                         format="TXT", 
                         verbose=TRUE), "list")

    expect_is(graphPc(source=c("http://identifiers.org/uniprot/O14503", 
                               "http://identifiers.org/uniprot/A2I2N6"),
                      target=c("http://identifiers.org/uniprot/Q9P2X7",
                               "http://identifiers.org/uniprot/O15516"),
                      kind="PATHSFROMTO", 
                      format="TXT", 
                      verbose=TRUE), "list")
    
    # Multiple datasources
    expect_is(graphPc(source="http://identifiers.org/uniprot/O14503", 
                      kind="neighborhood", 
                      format="TXT", 
                      datasource=c("DIP","Reactome","HumanCyc"),
                      verbose=TRUE), "list")
})

test_that("outputFormatsSupported", {
    skip_on_bioc()
    
    genes <- c("AKT1", "IRS1")

    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="TXT", 
                                 verbose=TRUE)
    
    expect_true(all(c("nodes", "edges") %in% names(results)))
    
    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="BIOPAX", 
                                 verbose=TRUE)
    
    # externalptr is returned by typeof for an XMLDocument
    expect_equal(typeof(results), "externalptr")
    
    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="SIF",
                                 verbose=TRUE)
    
    expect_equal(ncol(results), 3)
    
    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="GSEA",
                                 verbose=TRUE)
    
    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="SBGN", 
                                 verbose=TRUE)  
})

test_that("traverse", {
    skip_on_bioc()
    
    outFile <- tempfile()
    results <- traverse(uri="http://identifiers.org/uniprot/P38398", 
                        path="ProteinReference/organism/displayName")
    expect_is(results, "XMLInternalDocument")

    # Multiple URIs
    outFile <- tempfile()
    results <- traverse(uri=c("http://identifiers.org/uniprot/P38398", 
                              "http://identifiers.org/uniprot/Q96EB6"), 
                        path="ProteinReference/organism/displayName")
    expect_is(results, "XMLInternalDocument")
})

test_that("topPathways", {  
    skip_on_bioc()
    
    results <- topPathways(q="TP53", datasource="panther", verbose = TRUE)
    expect_is(results, "data.frame")
})

test_that("readSifnx", {
    expect_error(readSifnx("BAD"), "ERROR: inputFile not file.")
    
    results <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
    expect_true(all(c("nodes", "edges") %in% names(results)))

    results <- readSifnx(system.file("extdata", "test_sifnx.txt", package="paxtoolsr"))
    expect_equal(ncol(results$edges), 6)
        
    results <- readSifnx(system.file("extdata", "test_sifnx2.txt", package="paxtoolsr"))
    expect_equal(ncol(results$edges), 6)
    
    results <- readSifnx(system.file("extdata", "test_sifnx_sm.txt", package="paxtoolsr"))
    expect_equal(ncol(results$edges), 6)
})

test_that("readBiopax", {
    expect_error(readBiopax("BAD"), "ERROR: inputFile not file.")
    
    results <- readBiopax(system.file("extdata", "test_biopax.owl", package="paxtoolsr"))
    expect_is(results, "XMLInternalDocument")
})

test_that("readSbgn", {
    expect_error(readSbgn("BAD"), "ERROR: inputFile not file.")
    
    results <- readSbgn(system.file("extdata", "test_sbgn.xml", package="paxtoolsr"))
    expect_is(results, "XMLInternalDocument")
})

test_that("readGmt", {
    expect_error(readGmt("BAD"), "ERROR: inputFile not file.")
    
    results <- readGmt(system.file("extdata", "test_PathwayCommons12.kegg.hgnc.gmt", package="paxtoolsr"))
    expect_is(results, "list")
})

test_that("readSif", {
    expect_error(readSif("BAD"), "ERROR: inputFile not file.")
    
    results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
    expect_is(results, "data.frame")
})

test_that("summarizeSif", {
    sif <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
    results <- summarizeSif(sif)
    expect_is(results, "list")
})

test_that("loadSifInIgraph", {
    sif <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
    results <- loadSifInIgraph(sif)
    expect_is(results, "igraph")    
})

test_that("getSifInteractionCategories", {
    results <- getSifInteractionCategories()
    expect_is(results, "list")    
})

test_that("processPcRequest", {
    fileName <- system.file("extdata", "test_biopax.owl", package="paxtoolsr")
    content <- readChar(fileName, file.info(fileName)$size)
    results <- processPcRequest(content, "BIOPAX")
    expect_is(results, "XMLInternalDocument")   
})

test_that("downloadFile", {
    skip_on_bioc()
    
    results <- downloadFile("http://google.com/", fileName="index.html", destDir=tempdir())
    expect_true(results)
})

test_that("downloadPc2", {
    # Skip still results in error using TAP reporter
    #skip("NA")
    #downloadPc2("Pathway Commons.7.pid.GSEA.hgnc.gmt.gz", Sys.getenv("HOME"))
})

test_that("filterSif", {
    results <- readSif(system.file("extdata", "test_sif.txt", package="paxtoolsr"))
    intTypes <- c("controls-state-change-of", "controls-expression-of", "catalysis-precedes")
    filteredNetwork <- filterSif(results, intTypes)
    ints <- summarizeSif(filteredNetwork)
    expect_equal(ints$totalInteractions, 0)
})

test_that("getCacheFiles", {
    results <- getCacheFiles()
    expect_is(results, "character")
})

test_that("convertToPathwayObject", {})
test_that("splitSifnxByPathway", {})



