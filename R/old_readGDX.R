#' old_readGDX
#' 
#' Function to read gdx files in R. - \bold{DEPRECEATED!}
#' 
#' 
#' @usage old_readGDX(file,...,restore_zeros=NULL,
#' types=c("sets","equations","parameters","variables","aliases"),field="All",format="simplest")
#' @param file File name of the gdx file
#' @param ... search strings defining the objects that should be read from gdx
#' file, with *-autocompletion. Can also be vectors containing more than one
#' search strings
#' @param restore_zeros Dummy argument for downwards compatibility. Not used
#' anymore! Function will now always try to restore zeros
#' @param types Types of objects that should be extracted. Available options
#' are "sets", "equations", "parameters" and "variables".
#' @param field Defining what kind of information should be returned. "All"
#' means all available data. Other options are "l" (level value), "m"
#' (marginal), "lo" (lower bound), "up" (upper bound) and "s" (scaling factor).
#' In the case that the level value is not part of the field value (all options
#' other than "All" and "l") only values for equations and variables are
#' returned as all other types do not have this kind of information
#' @param format Output format. Four choices are currently available
#' "detailed", "simple", "simplest", "compact" and "raw". "detailed" is the old
#' default which returns a list of lists separating the outputs first in type
#' and afterwards in variable names. "simple" returns a list of variables. If
#' there is more than one object returned "simplest" behaves exactly the same
#' as "simple". However, if only one object is read from gdx file the array
#' itself is returned completely getting rid of the list structure. "raw" is
#' the data in the format as it comes from \code{rgdx}. This is especially
#' useful the data should be written again to a gdx file without having much
#' transformations in between. "simplest" is set by default because it is the
#' most convenient output. However, as "simplest" either returns a list of
#' array or an array itself it is harder to use in other functions. "compact"
#' behaves like "simple" with the only difference that data is returned as
#' magclass rather than arrays, which can be especially quite useful for sparse
#' data. Or if you anyway plan to work with magclass objects rather than
#' arrays.
#' @return The gdx objects read in the format set with the argument
#' \code{format}.
#' @note \bold{This function is depreceated! Please use \code{\link{readGDX}}
#' instead!}
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{readGDX}}, \code{\link{writeGDX}}
#' @importFrom gdxrrw gdxInfo rgdx
#' @examples
#' 
#' \dontrun{old_readGDX("bla.gdx","blub*")}
#' 


old_readGDX <- function(file,...,restore_zeros=NULL,types=c("sets","equations","parameters","variables","aliases"),field="All",format="simplest") {

  .rgdx2array <- function(x) {
    if(length(x$domains)==0) {
      if(length(x$val)==0) x$val <- 0
      out <- as.vector(x$val)  
    } else {
      x$val <- as.data.frame(x$val)
      dimnames <- x$uels
      names(dimnames) <- make.unique(x$domains,sep="")
      for(i in 1:length(x$domains)) {
        x$val[,i] <- x$uels[[i]][x$val[,i]]
        if(x$domains[i]=="*") dimnames[[i]] <- unique(x$val[,i])
      }
      if(x$type=="set") {
        if(x$dim==1) colnames(x$val) <- x$name
        else         colnames(x$val) <- make.unique(x$domains,sep="")
        out <- as.matrix(x$val)
      } else {
        out <- array(0,sapply(dimnames,length),dimnames)
        out[as.matrix(x$val[,-ncol(x$val)])] <- x$val[,ncol(x$val)]
      }
    }
    #add additional information as attribute
    attr(out,"gdxdata") <- x[!names(x)%in%c("val","uels","dim","ts")] 
    attr(out,"description") <- x$ts
    return(out)
  }
  
  .rgdx2dataframe <- function(x,restore_zeros=FALSE) {
    if(length(x$domains)==0) {
      if(length(x$val)==0) x$val <- 0
      out <- as.vector(x$val)  
    } else {
      names(x$uels) <- x$domains
      x$val <- as.data.frame(x$val)
      if(x$type=="set") {
        if(x$dim==1) colnames(x$val) <- x$name
        else         colnames(x$val) <- make.unique(x$domains,sep="")
      } else {
        colnames(x$val) <- make.unique(c(x$domains,"Value"),sep="")
      }
      for(i in 1:length(x$domains)) {
        x$val[,i] <- x$uels[[i]][x$val[,i]]
      }
      if(restore_zeros & x$type!="set" & x$type!="alias") {
        tmp <- expand.grid(x$uels)
        x$val <- merge(x$val,tmp,all=TRUE)
        x$val[is.na(x$val[,dim(x$val)[2]]),dim(x$val)[2]] <- 0
      }
      out <- x$val
    }
    #add additional information as attribute
    attr(out,"gdxdata") <- x[!names(x)%in%c("val","uels","dim","ts")] 
    attr(out,"description") <- x$ts
    return(out)
  }
  
    warning("Functions \"old_readGDX\" is depreceated. Please use readGDX instead!")
  if(!is.null(restore_zeros)) warning("Argument restore_zeroes is depreceated and ignored. Please do not use it anymore!")
  
  types <- match.arg(types,several.ok=TRUE)
  
  name <- c(...)
  if(length(name)==0) name <- "*"
  
  #translate name in standard regular expression syntax
  name <- paste("^",gsub("*",".*",name,fixed=TRUE),"$",sep="")
  info <- gdxInfo(file,dump=FALSE,returnDF=TRUE)
  
  #Only use types equations and variables if "level" is not part of the fields that should be adressed
  #(as all other types can only supply level values)
  if(field != "All" & field != "l") types <- intersect(c("equations","variables"),types)
  
  out <- list()
  for(t in types) {
    rownames(info[[t]]) <- info[[t]][,"name"]
    if(format=="detailed") out[[t]] <- list()
    tmp <- NULL
    for(n in name) tmp <- c(tmp,grep(n,info[[t]][,"name"],value=TRUE))
    for(i in tmp) {
      if(t=="variables"|t=="equations") l <- list(name=i,field=field,ts=TRUE)
      else l <- list(name=i,ts=TRUE)
      
      tmp2 <- rgdx(file,l,squeeze=FALSE,followAlias=FALSE)
      attr(tmp2,"description") <- tmp2$ts
      if(format!="raw" & t!="aliases") {
        if(format=="compact") {
          tmp2 <- .rgdx2dataframe(tmp2)
        } else {
          tmp2 <- .rgdx2array(tmp2) 
        }
      }
      if(format!="detailed") out[[i]] <- tmp2
      else out[[t]][[i]] <- tmp2
    }
  }
  if(format=="simplest" & length(out)==1) return(out[[1]])
  else return(out)
}
