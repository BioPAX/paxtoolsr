.packageName <- "paxtoolsr"

#' @import rJava
#' @import XML
#' @importFrom RCurl getURLContent url.exists
#' @import rjson
#' @import plyr
.onLoad <- function(lib, pkg){
    # Set Pathway Commons version
    options(pc.version="7")

    # Create cache directory in user home directory 
    cacheDir <- file.path(Sys.getenv("HOME"), ".paxtoolsRCache")
    dir.create(file.path(cacheDir, showWarnings=FALSE)
    
    if(file.exists(cacheDir)) {
        Sys.setenv("PAXTOOLSR_CACHE" = cacheDir)
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
    .jpackage(pkg, jars=c("paxtools-4.3.0.jar"))
    
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
#' @noRd
skip_on_bioc <- function() {
    if(identical(Sys.getenv("NOT_BIOC"), "true")) return()
    
    message <- "On Bioconductor"
    cond <- structure(list(message = message), class = c("skip", "condition"))
    stop(cond)
}
