.packageName <- "paxtoolsr"

#' @import rJava
#' @import XML
.onLoad <- function(lib, pkg){
    # Set Pathway Commons version
    options(pc.version="7")

    # Create cache directory in user home directory 
    cacheDir <- file.path(Sys.getenv("HOME"), ".paxtoolsRCache")
    cacheMap <- file.path(cacheDir, "cacheMap.txt")
    dir.create(file.path(cacheDir), showWarnings=FALSE)
    
    if(file.exists(cacheDir)) {
        Sys.setenv("PAXTOOLSR_CACHE" = cacheDir)
        
        #Add cacheMap.txt
        if(!file.exists(cacheMap)) {
            tmp <- data.frame(fileName=character(),
                              retrievedDate=character(), 
                              url=character(), 
                              stringsAsFactors=FALSE) 
            
            write.table(tmp, file=file.path(cacheDir, "cacheMap.txt"), 
                        quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
        }
    } else {
        Sys.setenv("PAXTOOLSR_CACHE" = "")
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

#jar.paxtools <- "lib/paxtools-4.2.1.jar"
#jar.paxtools <- "lib/paxtools-jar-with-dependencies.jar"
#.jinit(classpath=c(jar.paxtools))

#' Skip a test if on Bioconductor
#' 
#' Extension on testthat code
#' 
#' @concept paxtoolsr
#' @export
skip_on_bioc <- function() {
    if(identical(Sys.getenv("NOT_BIOC"), "true")) return()
    
    message <- "On Bioconductor"
    cond <- structure(list(message = message), class = c("skip", "condition"))
    stop(cond)
}
