![R-CMD-check](https://github.com/BioPAX/paxtoolsr/actions/workflows/R-CMD-check.yaml/badge.svg)

**NOTE:** Users interested in the source code should download the code directly from Bioconductor repositories:

* [Release Version](http://bioconductor.org/packages/devel/bioc/html/paxtoolsr.html)
* [Development Version](http://bioconductor.org/packages/devel/bioc/html/paxtoolsr.html)

# PaxtoolsR

An R package providing [Paxtools](http://www.biopax.org/paxtools.php) and [Pathway Commons](http://www.pathwaycommons.org/) functionality. This project provides users with the ability to read BioPAX files and access Pathway Commons web service functions to:

* Merge multiple BioPAX files
* Extract sub-networks from BioPAX files
* Do a number of format conversions
* Validate BioPAX files
* Search and retrieve Pathway Commons data

This package is primarily directed towards R users who wish to work with binary interactions networks in the form of Simple Interaction Format (SIF) networks.

## Install PaxtoolsR from Bioconductor (Recommended)

### Dependencies
#### Windows (tested on Windows 10)

[Java](http://www.oracle.com/technetwork/java/javase/downloads/index.html) needs to be installed. NOTE: If using a 64-bit system, make sure to install (or re-install) the 64-bit version. Otherwise, you may encounter an [rJava issue with JAVA_HOME](http://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/).

* NOTE: Installation on Windows 10 from GitHub using devtools::install_github may require args="--no-multiarch"

#### OS X (tested on Mavericks OSX 10.9+)

Java needs to be installed. If it is not installed, you will be prompted to install Java the first time you load the paxtoolsr package (NOTE: This prompt may crash RStudio, but installation of Java should not be affected).

* NOTE: Further instructions on rJava installation are found here: http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite

#### Ubuntu (tested on Ubuntu 14.04)

Run these commands in the Terminal:

    # For latest R version
    sudo apt-add-repository -y ppa:marutter/rrutter
    sudo apt-get -y update
    sudo apt-get -y upgrade
    sudo apt-get -y install r-base
    # For plyr
    sudo apt-get -y install g++
    # For RCurl
    sudo apt-get -y install libcurl4-openssl-dev
    # For rJava
    sudo apt-get -y install liblzma-dev
    sudo apt-get -y install libbz2-dev
    sudo apt-get -y install libpcre++-dev
    sudo apt-get -y install openjdk-7-jdk  
    # For XML
    sudo apt-get -y install libxml2-dev
    # To let R find Java
    sudo R CMD javareconf

### Install Bioconductor and PaxtoolsR

Run these commands within R:

    if (!requireNamespace("BiocManager", quietly=TRUE))
        install.packages("BiocManager")
    BiocManager::install("paxtoolsr") 

# Install PaxtoolsR Development Version from GitHub

    setRepositories(ind=1:6)
    options(repos="http://cran.rstudio.com/")
    if(!require(remotes)) { install.packages("remotes") }
    library(remotes) 

    remotes::install_github("BioPAX/paxtoolsr")
    remotes::install_github("BioPAX/paxtoolsr", args="--no-multiarch") # On Windows, 64-bit

# Using PaxtoolsR: R Vignette (Tutorial)

The tutorial describes a number of possible use cases, including network visualization
and gene set enrichment analysis using this R package. Once installed, view tutorials for
PaxtoolsR using the following command:

```
library(paxtoolsr)
browseVignettes("paxtoolsr")
```

A copy of the vignette [Using PaxtoolsR](http://bioconductor.org/packages/release/bioc/vignettes/paxtoolsr/inst/doc/using_paxtoolsr.html) is viewable from the Bioconductor website.
