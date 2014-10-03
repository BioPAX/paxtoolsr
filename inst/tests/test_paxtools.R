# All tests are done on files in package using system.file()

context("Paxtools Functionality")

test_that("fetch", {
    outFile <- tempfile()
    
    tmp <- getPc(uri="http://purl.org/pc2/4/Pathway_8cd6ef6591606767f64719b8cbac35df", 
                             format="BIOPAX", 
                             verbose=TRUE)
    
    ids <- c("http://identifiers.org/uniprot/P32754", 
             "http://identifiers.org/uniprot/O43708")
    
    results <- fetch(tmp, outFile, ids)
    
    expect_is(results, "XMLInternalDocument")
})

test_that("fromPsimi", {
    outFile <- tempfile()
    
    results <- fromPsimi(system.file("extdata", "10523676-compact.xml", package="paxtoolsr"), 
                                     outFile, 
                                     3)
    
    expect_is(results, "XMLInternalDocument")
})

test_that("toGSEA", {
    outFile <- tempfile()
    
    results <- toGSEA(system.file("extdata", "biopax3-short-metabolic-pathway.owl", package="paxtoolsr"), 
                                  outFile, 
                                  "uniprot", 
                                  crossSpeciesCheckFlag=TRUE) 

    # The file should be tab delimited and have at least 3 columns 
    colNum <- ncol(read.delim(outFile, sep="\t", header=FALSE))
    expect_true(colNum > 3)
    
    expect_is(results, "list")
})

test_that("validate", {
    
    rawDoc <- validate(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), onlyErrors=TRUE)                             
    expect_is(rawDoc, "XMLInternalDocument")

    outFile <- tempfile()
    rawDoc <- validate(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                       type="html", outputFile=outFile, onlyErrors=TRUE) 
    expect_is(rawDoc, "character")
})

test_that("getNeighbors", {
    outFile <- tempfile()
    
    results <- getNeighbors(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr"), 
                                        outFile, 
                                        c("HTTP://WWW.REACTOME.ORG/BIOPAX/48887#PROTEIN2360_1_9606",
                                          "HTTP://WWW.REACTOME.ORG/BIOPAX/48887#PROTEIN1631_1_9606")) 
                             
    expect_is(results, "XMLInternalDocument")
})

test_that("toLevel3", {
    outFile <- tempfile()
    
    results <- toLevel3(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                                    outFile) 
    
    expect_is(results, "XMLInternalDocument")
})

test_that("toSBGN", {
    outFile <- tempfile()
    
    results <- toSBGN(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                                  outFile) 
    
    expect_is(results, "XMLInternalDocument")
})

test_that("toSifnx", {
    nodesFile <- tempfile()
    edgesFile <- tempfile()
    
    #nodeProps <- c("EntityReference/name", "EntityReference/xref")
    nodeProps <- c("EntityReference/name", "EntityReference/xref")
    edgeProps <- "Interaction/dataSource/displayName"
    
    results <- toSifnx(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                       nodesFile, 
                       edgesFile,
                       nodeProps,
                       edgeProps) 

    expect_equal(names(results), c("edges", "nodes"))
    expect_equal(length(colnames(results$edges)), (3 + length(edgeProps)))
    expect_equal(length(colnames(results$nodes)), (1 + length(nodeProps)))
})

test_that("toSif", {
    outFile <- tempfile()
    
    results <- toSif(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                                            outFile) 
    
    expect_that(colnames(results), equals(c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B"))) 
})

test_that("integrateBioPax", {
    outFile <- tempfile()
    
    results <- integrateBiopax(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                                            system.file("extdata", "circadian_clock.owl", package="paxtoolsr"), 
                                            outFile) 
    
    expect_is(results, "XMLInternalDocument")
})

test_that("mergeBiopax", {
    outFile <- tempfile()
    
    results <- mergeBiopax(system.file("extdata", "dna_replication.owl", package="paxtoolsr"), 
                                            system.file("extdata", "circadian_clock.owl", package="paxtoolsr"), 
                                            outFile) 
    
    expect_is(results, "XMLInternalDocument")
})

test_that("summarize", {
    summary <- summarize(system.file("extdata", "dna_replication.owl", package="paxtoolsr")) 
    
    expect_true(is.list(summary))
})

test_that("xmlInternalDocumentInput", {
    tmp <- getPc(uri="http://purl.org/pc2/4/Pathway_8cd6ef6591606767f64719b8cbac35df", 
                                 format="BIOPAX", 
                                 verbose=TRUE)
    
    results <- toSif(tmp) 
    
    expect_is(results, "data.frame")
})

#DEBUG 
#test_that("FAIL", {    
#   expect_that(FALSE, is_true())
#})
