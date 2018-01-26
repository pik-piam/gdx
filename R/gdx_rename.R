#' gdx_rename
#' 
#' Function to replace or delete names of variables, parameters or equations in
#' gdx files (aliases and sets are currently not supported, but support could
#' be added if required).
#' 
#' 
#' @param file File name of the gdx file in which the objects or set entries should be renamed
#' @param ... For level=="objects": listing of renamings/deletions that should be done in the form
#' oldname="newname" (or oldname=0 for delete). Alternatively a named vector could be provided c(oldname="newname",bla="blub",deleteme=0).
#' For level=="set_names": A named vector should be provided c(newname1="oldname1",newname2="oldname2",newname2="oldname1"). 
#' It can select only some of the old set entries or use some old entries for more than one new set entry.
#' @param set_name If you want to rename entries of a set you have to specify the name of the set. In the default case (set_name=NULL) you can rename objects.
#' @author Jan Philipp Dietrich, Lavinia Baumstark
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
#' 
#' # rename and select set entries of the set "testset"
#' gdx_rename("bla.gdx",set_name="testset",c(newentry1="oldentry1",newentry2="oldentry2"))
#' }
#' 
gdx_rename <- function(file,...,set_name=NULL) {
 reps <- c(...)
 if(length(reps)==0){ 
   return(names(readGDX(file[1],format="raw")))
   stop("you did not specify the object or set entry that you want to rename. For possible objects see the list above.")        
 }

 for (f in file) {
   cat("Renaming",f,"\n")
   x <- readGDX(file,format="raw")
   # ---------------- rename variables ------------------------------
   if(is.null(set_name)){
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
   } 
   # -------------- rename set entries --------------------------
   if(!is.null(set_name)){
     for (n in names(x)) {
       if(x[[n]]$type!="set") {
         if(set_name %in% x[[n]]$domains){
           j       <- which(x[[n]]$domains==set_name)
           old_reg <- x[[n]]$uels[[j]]
           # overwrite old region names with new names
           x[[n]]$uels[[j]] <- names(reps)
           # make empty new data
           x_new <- NULL
           for(r in names(reps)) {
             # index of the old region name
             i <- which(old_reg==reps[r])
             # manipulate data
             tmp <- x[[n]]$val[x[[n]]$val[,j]==i,,drop=FALSE]
             # substitute old region name number by new one 
             tmp[,j] <- which(names(reps)==r)
             x_new  <- rbind(x_new,tmp)
           }  
           x[[n]]$val <- x_new
         }
       }
     } 
   }   
   writeGDX(x,f)
  }
}