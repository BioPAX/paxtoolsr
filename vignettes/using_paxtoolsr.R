## ----knitrSetup, include=FALSE-------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center", tidy=TRUE)


## ----style, include=FALSE, echo=FALSE, results='asis'--------------------
BiocStyle::markdown()

## ----installPaxtoolsr, eval=FALSE----------------------------------------
## source("http://bioconductor.org/biocLite.R")
## biocLite("paxtoolsr")

## ----loadLibrary, message=FALSE, warning=FALSE---------------------------
library(paxtoolsr)

## ----searchHelp, eval=FALSE, tidy=FALSE----------------------------------
## help.search("paxtoolsr")

## ----showHelp, eval=FALSE, tidy=FALSE------------------------------------
## help(graphPc)
## ?graphPc

## ----paxtoolsMergeExample------------------------------------------------
file1 <- system.file("extdata", "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr")
file2 <- system.file("extdata", "biopax3-short-metabolic-pathway.owl", package="paxtoolsr")

mergedFile <- mergeBiopax(file1, file2)

## ----summarize, results='hold'-------------------------------------------
s1 <- summarize(file1)
s2 <- summarize(file2)
s3 <- summarize(mergedFile)

s1$Catalysis
s2$Catalysis
s3$Catalysis

## ----paxtoolsValidationExample, eval=FALSE-------------------------------
## errorLog <- validate(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr"), onlyErrors=TRUE)

## ----loadSifFromFile-----------------------------------------------------
sif <- toSif(system.file("extdata", "biopax3-short-metabolic-pathway.owl", package="paxtoolsr")) 

## ----viewSifHead---------------------------------------------------------
head(sif)

## ----toSifnxExample, tidy=TRUE-------------------------------------------
# Select additional node and edge properties
nodeProps <- c("EntityReference/name", "EntityReference/xref")
edgeProps <- "Interaction/dataSource/displayName"
    
results <- toSifnx(inputFile=system.file("extdata", "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr"), nodeProps=nodeProps, edgeProps=edgeProps) 

## ----downloadSif, eval=FALSE---------------------------------------------
## results <- downloadPc(format="SIFNX")

## ----searchResultsExample, eval=FALSE------------------------------------
## ## Search Pathway Commons for "glycolysis"-related pathways
## searchResults <- searchPc(q="glycolysis", type="pathway")

## ----searchResultsExampleVerbose-----------------------------------------
## Search Pathway Commons for "glycolysis"-related pathways 
searchResults <- searchPc(q="glycolysis", type="pathway", verbose=TRUE)

## ----searchResultsType---------------------------------------------------
str(searchResults)

## ----searchResultsXpathExample-------------------------------------------
xpathSApply(searchResults, "/searchResponse/searchHit/name", xmlValue)[1]
xpathSApply(searchResults, "/searchResponse/searchHit/pathway", xmlValue)[1]

## ----convertXmlToDf, message=FALSE---------------------------------------
library(plyr)
searchResultsDf <- ldply(xmlToList(searchResults), data.frame)

# Simplified results
simplifiedSearchResultsDf <- searchResultsDf[, c("name", "uri", "biopaxClass")]

## ----saveBiopaxFileFromPcQuery, message=FALSE, results='hide'------------
## Use an XPath expression to extract the results of interest. In this case, the URIs (IDs) for the pathways from the results
searchResults <- xpathSApply(searchResults, "/searchResponse/searchHit/uri", xmlValue)

## Generate temporary file to save content into
biopaxFile <- tempfile() 

## Extract a URI for a pathway in the search results and save into a file  
idx <- which(grepl("panther", simplifiedSearchResultsDf$uri) & 
             grepl("glycolysis", simplifiedSearchResultsDf$name, ignore.case=TRUE))
uri <- simplifiedSearchResultsDf$uri[idx]
saveXML(getPc(uri, "BIOPAX"), biopaxFile) 

## ----traverse------------------------------------------------------------
# Get the protein Uniprot ID for a given HGNC gene symbol
id <- idMapping("AKT1")

# Covert the Uniprot ID to a URI that would be found in Pathway Commons
uri <- paste0("http://identifiers.org/uniprot/", id)

# Get URIs for only the ModificationFeatures of the protein
xml <- traverse(uri=uri, path="ProteinReference/entityFeature:ModificationFeature")

# Extract all the URIs 
uris <- xpathSApply(xml, "//value/text()", xmlValue)

# For the first URI get the modification position and type
tmpXml <- traverse(uri=uris[1], path="ModificationFeature/featureLocation:SequenceSite/sequencePosition")
cat("Modification Position: ", xpathSApply(tmpXml, "//value/text()", xmlValue))

tmpXml <- traverse(uri=uris[1], path="ModificationFeature/modificationType/term")
cat("Modification Type: ", xpathSApply(tmpXml, "//value/text()", xmlValue))

## ----loadGraphLibraries, message=FALSE-----------------------------------
library(igraph)

## ----plotGraph-----------------------------------------------------------
sif <- toSif(system.file("extdata", "biopax3-short-metabolic-pathway.owl", package="paxtoolsr")) 

# graph.edgelist requires a matrix
g <- graph.edgelist(as.matrix(sif[,c(1,3)]), directed=FALSE)
plot(g, layout=layout.fruchterman.reingold)

## ----graphQueryExample---------------------------------------------------
genes <- c("AKT1", "IRS1", "MTOR", "IGF1R")
t1 <- graphPc(source=genes,
              kind="PATHSBETWEEN",
              format="BINARY_SIF",
              verbose=TRUE)
t2 <- t1[which(t1[,2] == "controls-state-change-of"),]

g <- graph.edgelist(as.matrix(t2[,c(1,3)]), directed=FALSE)
plot(g, layout=layout.fruchterman.reingold)

## ----dataOverlayExample--------------------------------------------------
library(RColorBrewer)

# Generate a color palette that goes from white to red that contains 10 colors 
numColors <- 10
colors <- colorRampPalette(brewer.pal(9, "Reds"))(numColors)

# Generate values that could represent some experimental values 
values <- runif(length(V(g)$name))

# Scale values to generate indicies from the color palette  
xrange <- range(values)
newrange <- c(1, numColors)

factor <- (newrange[2]-newrange[1]) / (xrange[2]-xrange[1])
scaledValues <- newrange[1] + (values-xrange[1]) * factor
indicies <- as.integer(scaledValues)

# Color the nodes based using the indicies and the color palette created above 
g <- set.vertex.attribute(g, "color", value=colors[indicies])

#get.vertex.attribute(h, "color")

plot(g, layout = layout.fruchterman.reingold) 

## ----sifNetStats---------------------------------------------------------
# Degree for each node in the igraph network 
degree(g)
#Number of nodes 
length(V(g)$name)
#Clustering coefficient 
transitivity(g)
#Network density 
graph.density(g)
# Network diameter
diameter(g)

## ----sifShortestPath-----------------------------------------------------
# Get the first shortest path between two nodes
tmp <- get.shortest.paths(g, from="IRS1", to="MTOR")

# igraph seems to return different objects on Linux versus OS X for get.shortest.paths()
if(class(tmp[[1]]) == "list") {
	path <- tmp[[1]][[1]]	# Linux
} else {
  path <- tmp[[1]] # OS X
}

# Convert from indicies to vertex names 
V(g)$name[path]

## ----gseaExampleLibraries, message=FALSE---------------------------------
library(paxtoolsr) # To retrieve data from Pathway Commons
library(limma) # Contains geneSetTest
library(affy) # To load microarray data
library(hgu95av2) # Annotation packages for the hgu95av2 platform
library(hgu95av2cdf)
library(XML) # To parse XML files

## ----gseaExampleGenGeneSet, results='hide', eval=FALSE-------------------
## # Generate a Gene Set
## ## Search Pathway Commons for "glycolysis"-related pathways
## searchResults <- searchPc(q="glycolysis", type="pathway")
## 
## ## Use an XPath expression to extract the results of interest. In this case, the URIs (IDs) for the pathways from the results
## searchResults <- xpathSApply(searchResults, "/searchResponse/searchHit/uri", xmlValue)
## 
## ## Generate temporary files to save content into
## biopaxFile <- tempfile()
## 
## ## Extract the URI for the first pathway in the search results and save into a file
## uri <- searchResults[2]
## saveXML(getPc(uri, "BIOPAX"), biopaxFile)

## ----gseaExampleGenGeneSetSave, results='hide'---------------------------
## Generate temporary files to save content into
gseaFile <- tempfile() 

## Generate a gene set for the BioPAX pathway with gene symbols 
### NOTE: Not all search results are guaranteed to result in gene sets 
tmp <- toGSEA(biopaxFile, gseaFile, "HGNC Symbol", FALSE)
geneSet <- tmp$geneSet

## ----gseaExampleMicroarrayAnalyis----------------------------------------
# Process Microarray Data
## Load/process the estrogen microarray data 
estrogenDataDir <- system.file("extdata", package="estrogen")
targets <- readTargets(file.path(estrogenDataDir, "estrogen.txt"), sep="")

abatch <- ReadAffy(filenames=file.path(estrogenDataDir, targets$filename))
eset <- rma(abatch)

f <- paste(targets$estrogen,targets$time.h,sep="")
f <- factor(f)
design <- model.matrix(~0+f)
colnames(design) <- levels(f)

fit <- lmFit(eset, design)
 
cont.matrix <- makeContrasts(E10="present10-absent10",E48="present48-absent48",Time="absent48-absent10",levels=design)

fit2  <- contrasts.fit(fit, cont.matrix)
fit2  <- eBayes(fit2)

## Map the gene symbols to the probe IDs 
symbol <- unlist(as.list(hgu95av2SYMBOL))

### Check that the gene symbols are on the microarray platform 
genesOnChip <- match(geneSet,symbol)
genesOnChip # CHECK FOR ERROR HERE 

genesOnChip <- genesOnChip[!is.na(genesOnChip)]

### Grab the probe IDs for the genes present
genesOnChip <- names(symbol[genesOnChip])
genesOnChip <- match(genesOnChip, rownames(fit2$t))
genesOnChip <- genesOnChip[!is.na(genesOnChip)]

## Run the Gene Set Test from the limma Package
geneSetTest(genesOnChip,fit2$t[,1],"two.sided")

## ----idMapping-----------------------------------------------------------
sif <- toSif(system.file("extdata", "raf_map_kinase_cascade_reactome.owl", package="paxtoolsr"))

# Generate a mapping between the HGNC symbols in the SIF to the Uniprot IDs
library(biomaRt)
ensembl <- useMart("ensembl")
ensembl <- useDataset("hsapiens_gene_ensembl", mart=ensembl)

hgnc_symbol <- c(sif$PARTICIPANT_A, sif$PARTICIPANT_B)
output <- getBM(attributes=c('hgnc_symbol', 'uniprot_sptrembl'), filters='hgnc_symbol', values=hgnc_symbol, mart=ensembl)

# Remove blank entries
output <- output[output[,2] != "",] 

head(output)

## ----filePathExample, eval=FALSE-----------------------------------------
## toSif("/directory/file")
## #or
## toSif("X:\\directory\\file")

## ----changeHeapSize, eval=FALSE------------------------------------------
## options(java.parameters="-Xmx1024m")
## 
## library(paxtoolsr)
## 
## # Megabyte size
## mbSize <- 1048576.0
## 
## runtime <- .jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
## maxMemory <- .jcall(runtime, "J", "maxMemory")
## maxMemoryMb <- maxMemory / mbSize
## cat("Max Memory: ", maxMemoryMb, "\n")

## ----sessionInfo---------------------------------------------------------
sessionInfo()

