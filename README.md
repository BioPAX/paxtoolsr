**NOTE:** Users interested in the source code should download the latest code directly from Bioconductor SVN repositories:

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
#### Windows (tested on Windows 8)

[Java](http://www.oracle.com/technetwork/java/javase/downloads/index.html) needs to be installed.

#### OS X (tested on Mavericks OSX 10.9)

Java needs to be installed. If it is not installed, you will be prompted to install Java the first time you load the paxtoolsr package.

#### Ubuntu (tested on Ubuntu 14.04)

Run these commands in the Terminal:

    # For latest R version
    sudo apt-add-repository -y ppa:marutter/rrutter
    sudo apt-get -y update
    sudo apt-get -y upgrade
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

    source("http://bioconductor.org/biocLite.R")
    biocLite("paxtoolsr") 

# Install PaxtoolsR Development Version

Download the [development version](http://bioconductor.org/packages/devel/bioc/html/paxtoolsr.html) package source and [install from source](http://stackoverflow.com/questions/1474081/how-do-i-install-an-r-package-from-source) with the following command:

    install.packages(file_name_and_path, repos = NULL, type="source")

# Using PaxtoolsR: R Vignette (Tutorial)

The tutorial describes a number of possible use cases, including network visualization
and gene set enrichment analysis using this R package. Once installed, view tutorials for
PaxtoolsR using the following command:


```
library(paxtoolsr)
browseVignettes("paxtoolsr")
```

A copy of the vignette [Using PaxtoolsR](http://bioconductor.org/packages/release/bioc/vignettes/paxtoolsr/inst/doc/using_paxtoolsr.html) is viewable from the Bioconductor website.