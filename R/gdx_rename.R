#' gdx_rename
#' 
#' Function to replace or delete names of variables, parameters or equations in
#' gdx files (aliases and sets are currently not supported, but support could
#' be added if required).
#' 
#' 
#' @usage gdx_rename(file,...)
#' @param file File name of the gdx file in which the objects should be renamed
#' @param ... listing of renamings/deletions that should be done in the form
#' oldname="newname" (or oldname=0 in the case that the object should be
#' deleted). Alternatively also a named vector could be provided such as
#' c(oldname="newname",bla="blub",deleteme=0). If nothing is provided the
#' function will return a list of names in the given gdx file.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{readGDX}}, \code{\link{writeGDX}}
#' @examples
#' 
#' \dontrun{
#' 
#' #list all objects in the given gdx file
#' gdx_rename("bla.gdx")
#' 
#' #replace oldname with newname and oldname2 with newname2
#' gdx_rename("bla.gdx",oldname="newname",oldname2="newname2")
#' 
#' #delete "deleteme"
#' gdx_rename("bla.gdx",deleteme=0)
#' }
#' 
gdx_rename <- function(file,...) {
 reps <- c(...)
 if(length(reps)==0) return(names(readGDX(file[1],format="raw")))
 
 for (f in file) {
   cat("Renaming",f,"\n")
   x <- readGDX(file,format="raw")
   for(n in names(reps)) {
     i <- which(names(x)==n)
     if(length(i)==0) {
       warning("There is no object called \"",n,"\" in the given gdx file!")
     } else if(length(i)>1) {
       stop("The name \"",n,"\" appears more than once in the given gdx file! There must be something wrong!")
     } else {
       if(reps[n]==0) {
         #delete object
         x[[i]] <- NULL
         cat(n,"-> DELETED\n")
       } else {
         #rename object
        if(x[[i]]$type=="set") {
           warning("Set ",names(x)[i]," cannot be renamed as sets are currently not supported!")
        } else if(x[[i]]$type=="alias") {
           warning("Alias ",names(x)[i]," cannot be renamed as aliases are currently not supported!")
        } else {
          names(x)[i] <- reps[n]
          x[[i]]$name <- reps[n]
          cat(n,"->",reps[n],"\n")
        }
       }
     }   
   }
   writeGDX(x,f)
  }
}