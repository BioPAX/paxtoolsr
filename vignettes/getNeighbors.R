With paxtoolsr the heap size can also be increased; see below
  
http://bioconductor.org/packages/release/bioc/vignettes/paxtoolsr/inst/doc/using_paxtoolsr.html#memory-limits-specify-jvm-maximum-heap-size

# Change heap size when using paxtoolsr 
options(java.parameters = "-Xmx4g")
library(paxtoolsr)

gzFile <- "PathwayCommons.8.reactome.BIOPAX.owl.gz"
owlFile <- "PathwayCommons.8.reactome.BIOPAX.owl"
untar(gzFile, exdir=".")

out1 <- "out1.out"
out2 <- "out2.out"
t1 <- getNeighbors(owlFile, out1, "http://pathwaycommons.org/pc2/Protein_c1c45c11f5ebaffd5a137865b2f4a6a3")

But what I anticipate what you want to do is not completely straight-forward with getNeighbors(). You cannot simply pass gene IDs to getNeighbors with either in paxtools (or paxtoolsr that calls paxtools getNeighbors). You need to pass in URIs 

NOTE: GNG2: http://pathwaycommons.org/pc2/Protein_c1c45c11f5ebaffd5a137865b2f4a6a3
NOTE: Equivalent paxtools command: java -Xmx4g -jar paxtools-4.3.1.jar getNeighbors PathwayCommons.8.reactome.BIOPAX.owl http://pathwaycommons.org/pc2/Protein_c1c45c11f5ebaffd5a137865b2f4a6a3 cmd.out

You'll end up with a BioPAX file that can be converted to a SIF

t2 <- toSif(t1, out2)

In my understanding of getNeighbors this will return all the genes and small molecules that interact with GNG2 **PLUS** all the genes those entities interact with 

I wrote the filterSif function does not use the URIs that I use pretty often for research projects: 

sif <- downloadPc2("PathwayCommons.8.reactome.BINARY_SIF.hgnc.txt.sif.gz", version="8")
filteredSif <- filterSif(sif, ids="SDC1", interactionTypes="controls-state-change-of")

This allows you to use gene symbols and ignore interaction types you might not want. But you need the development version from GitHub (https://github.com/BioPAX/paxtoolsr) if you want this functionality:

# Install 
setRepositories(ind=1:6)
options(repos="http://cran.rstudio.com/")
if(!require(devtools)) { install.packages("devtools") }
library(devtools) 
install_github("BioPAX/paxtoolsr")

gp <- graphPc(kind="NEIGHBORHOOD", datasource="reactome", format="BINARY_SIF", source="SDC1")

