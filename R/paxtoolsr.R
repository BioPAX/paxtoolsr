.packageName <- "paxtoolsr"

#' @import rJava
#' @import XML
#' @importFrom RCurl getURLContent url.exists
#' @import rjson
#' @import plyr
.onLoad <- function(lib, pkg){
    dlp <- Sys.getenv("DYLD_LIBRARY_PATH")
    if (dlp != "") { # for Mac OS X we need to remove X11 from lib-path
        Sys.setenv("DYLD_LIBRARY_PATH"=sub("/usr/X11R6/lib","",dlp))
    }   
    
    #jar.paxtools <- paste(lib, pkg, "java", "paxtools-jar-with-dependencies.jar", 
    #  sep=.Platform$file.sep)
    #.jinit(classpath=c(jar.paxtools))
    .jpackage(pkg, jars=c("paxtools-jar-with-dependencies.jar"))
    
    #DEBUG
    #packageStartupMessage(paste("paxtoolsr loaded. The classpath is: ", 
    #paste(.jclassPath(), collapse=" " )))
    
    # Taken from xlsxjars packages
    # What's your java  version?  Need >= 1.5.0.
    jversion <- .jcall('java.lang.System','S','getProperty','java.version')
    if (jversion < "1.5.0") {
        stop(paste("Your java version is ", jversion, ".  Need 1.5.0 or higher.", 
                             sep=""))       
    }
}

#jar.paxtools <- "lib/paxtools-4.2.1.jar"
#jar.paxtools <- "lib/paxtools-jar-with-dependencies.jar"
#.jinit(classpath=c(jar.paxtools))

