.packageName <- "paxtoolsr"

#' @import rJava
#' @import XML
#' @importFrom rappdirs user_cache_dir
.onLoad <- function(lib, pkg){
    # Set Pathway Commons version
    options(pc.version="12")
    Sys.setenv("PC_URL" = "http://www.pathwaycommons.org/pc2/")
    Sys.setenv("PC_ARCHIVE_URL" = "http://www.pathwaycommons.org/archives/PC2/")
    Sys.setenv("SIGNED_PC_URL"="http://tmp.lunean.com/")
    Sys.setenv("SIGNED_PC_FILE"="SignedPC_20160511.sif.gz")

    # Create cache directory in user home directory
    # NOTE: This is no longer backwards compatible with < 4.0.0
    cacheDir <- rappdirs::user_cache_dir("paxtoolsr")
    Sys.setenv("PAXTOOLSR_CACHE" = cacheDir)     
    
    cacheMap <- file.path(cacheDir, "cacheMap.txt")

    if(!dir.exists(Sys.getenv("PAXTOOLSR_CACHE"))) {
        dir.create(file.path(cacheDir))        
        stopifnot(dir.exists(cacheDir))
    }
    
    if(!dir.exists(cacheDir) || Sys.getenv("PAXTOOLSR_CACHE") != cacheDir) {
       stop(paste0("cacheDir does not exist: "), cacheDir) 
    }
    
    #Add cacheMap.txt
    if(!file.exists(cacheMap)) {
        tmp <- data.frame(fileName=character(),
                          retrievedDate=character(),
                          url=character(),
                          stringsAsFactors=FALSE)
        
        write.table(tmp, file=file.path(cacheDir, "cacheMap.txt"),
                    quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
    }
    
    dlp <- Sys.getenv("DYLD_LIBRARY_PATH")
    if (dlp != "") { # for Mac OS X we need to remove X11 from lib-path
        Sys.setenv("DYLD_LIBRARY_PATH"=sub("/usr/X11R6/lib","",dlp))
    }

    #jar.paxtools <- paste(lib, pkg, "java", "paxtools-jar-with-dependencies.jar",
    #  sep=.Platform$file.sep)
    #.jinit(classpath=c(jar.paxtools))
    #.jpackage(pkg, jars=c("paxtools-jar-with-dependencies.jar"))
    jars <- list.files(path=paste(lib, pkg, "java", sep=.Platform$file.sep),
                       pattern="jar$", full.names=TRUE)

    #.jaddClassPath(jars)
    #.jpackage(pkg, jars=jars)
    .jpackage(pkg, jars=c("paxtools-4.3.1.jar"))
    #.jpackage(pkg, lib)
    #print(.jclassPath())

    #DEBUG
    #packageStartupMessage(paste("paxtoolsr loaded. The classpath is: ",
    #paste(.jclassPath(), collapse=" " )))

    # Taken from xlsxjars packages
    # What's your java  version?  Need >= 1.5.0.
    jversion <- .jcall('java.lang.System','S','getProperty','java.version')
    if (jversion < "1.5.0") {
        stop(paste("Your java version is ", jversion, ". Need 1.5.0 or higher.",
                             sep=""))
    }
}

.onAttach <- function(libname, pkgname){
    packageStartupMessage('Consider citing this package: Luna A, et al. PaxtoolsR: pathway analysis in R using Pathway Commons. PMID: 26685306; citation("paxtoolsr")')
}

#jar.paxtools <- "lib/paxtools-4.2.1.jar"
#jar.paxtools <- "lib/paxtools-jar-with-dependencies.jar"
#.jinit(classpath=c(jar.paxtools))
