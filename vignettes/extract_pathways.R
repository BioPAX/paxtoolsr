Try this. First, update to the paxtoolsr development version (I just updated a few things):

setRepositories(ind=1:6)
options(repos="http://cran.rstudio.com/")
if(!require(devtools)) { install.packages("devtools") }
library(devtools) 
install_github("BioPAX/paxtoolsr")

# Example 

library(paxtoolsr)

exampleFileInPaxtoolsr <- system.file("extdata", "REACT_12034-3.owl", package="paxtoolsr")
sifnx <- toSifnx(exampleFileInPaxtoolsr, "/Users/cannin/Downloads/output.txt", "uniprot")

# Not all rows represented, but that's because not every row has a pathway listed
rowIndiciesForPathways <- splitSifnxByPathway(sifnx$edges) 

bmp <- sifnx$edges[rowIndiciesForPathways$`Signaling by BMP`, ]

# If you prefer a data.frame over a data.table (data.table is used for file reading speed), then do this:
library(data.table)
class(bmp) # Should be "data.table" "data.frame"
setDF(bmp)
class(bmp) # Should be "data.frame"

# Plot in R

# For simple plotting in R
g <- loadSifInIgraph(bmp)
plot(g)


