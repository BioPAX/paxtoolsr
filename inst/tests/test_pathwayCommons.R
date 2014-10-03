# All tests are done on files in package using system.file()

context("Pathway Commons Functionality")

test_that("searchPc", {
    
    #A basic text search. This query returns all entities that contain the 
    #"Q06609" keyword in XML
    outFile <- tempfile()
    results <- searchPc("Q06609")
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
    results <- searchPc(q="brca2", datasource=c("reactome", "pid"), type="ProteinReference", 
                        organism=c("9606", "10016"), verbose=TRUE)
    expect_is(results, "XMLInternalDocument")
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

test_that("graphPc", {  
    results <- graphPc(source="http://identifiers.org/uniprot/P20908", 
                       kind="neighborhood", 
                       format="EXTENDED_BINARY_SIF", 
                       verbose=TRUE)
    
    # Test Against Error Code 460
    expect_error(graphPc(source="http://identifiers.org/uniprot/PXXXXX", 
                         kind="neighborhood", 
                         format="EXTENDED_BINARY_SIF", 
                         verbose=TRUE), ".*460.*PC Webservice Error.*")
    
    expect_equal(length(names(results)), 2)

    expect_error(graphPc(source="http://identifiers.org/uniprot/P20908", 
                         kind="PATHSFROMTO", 
                         format="EXTENDED_BINARY_SIF", 
                         verbose=TRUE), "target must be set if kind is PATHSFROMTO")
    
    results <- graphPc(source="http://identifiers.org/uniprot/P20908", 
                       target="http://identifiers.org/uniprot/P93738",
                       kind="PATHSFROMTO", 
                       format="EXTENDED_BINARY_SIF", 
                       verbose=TRUE)
    expect_equal(length(names(results)), 2)
    
    expect_is(graphPc(source=c("http://identifiers.org/uniprot/Q06609", 
                                  "http://identifiers.org/uniprot/Q96EB6"), 
                         kind="neighborhood", 
                         format="EXTENDED_BINARY_SIF", 
                         verbose=TRUE), "list")

    expect_is(graphPc(source=c("http://identifiers.org/uniprot/P93738", 
                               "http://identifiers.org/uniprot/Q96EB6"),
                      target=c("http://identifiers.org/uniprot/P20908",
                               "http://identifiers.org/uniprot/Q06609"),
                      kind="PATHSFROMTO", 
                      format="EXTENDED_BINARY_SIF", 
                      verbose=TRUE), "list")
})

test_that("outputFormatsSupported", {
    genes <- c("AKT1", "IRS1")

    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="EXTENDED_BINARY_SIF", 
                                 verbose=TRUE)
    
    expect_equal(length(names(results)), 2)
    
    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="BIOPAX", 
                                 verbose=TRUE)
    
    # externalptr is returned by typeof for an XMLDocument
    expect_equal(typeof(results), "externalptr")
    
    results <- graphPc(source=genes, 
                                 kind="PATHSBETWEEN", 
                                 format="BINARY_SIF",
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
    results <- topPathways()
    expect_is(results, "data.frame")
})

test_that("idMapping", {
    outFile <- tempfile()
    
    results <- idMapping(c("BRCA2", "TP53"))
        
    expect_equal(results$BRCA2, "P51587")
    expect_equal(results$TP53, "P04637")
})
