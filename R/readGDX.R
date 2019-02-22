#' readGDX
#' 
#' Function to read gdx files in R. It is partly a reimplementation of readGDX
#' which is now based on magclass structures rather than array structures.
#' 
#' 
#' @param gdx Either file name of a gdx file or an already read in gdx (in the
#' latter case readGDX just acts as a filter. This can be useful if you want to
#' apply several functions on the same gdx file. In that case you could read in
#' the gdx first and then filter the data you need using readGDX.)
#' @param ... search strings defining the objects that should be read from gdx
#' file, with *-autocompletion. Can also be vectors containing more than one
#' search strings
#' @param types Types of objects that should be extracted. Available options
#' are "sets", "equations", "parameters", "variables" and "aliases".
#' @param field Defining what kind of information should be returned. "All"
#' means all available data. Other options are "l" (level value), "m"
#' (marginal), "lo" (lower bound), "up" (upper bound) and "s" (scaling factor).
#' In the case that the level value is not part of the field value (all options
#' other than "All" and "l") only data for equations and variables are returned
#' as all other types do not have this kind of information. WARNING: field has to
#' be set to "All" if the data is planned to be written back to a GDX. Otherwise
#' writeGDX will not work!
#' @param format Output format. Five choices are currently available
#' \code{detailed}, \code{simple}, \code{simplest}, \code{raw} and
#' \code{first_found}. Instead of writing the full format name each format has
#' its own abbreviation as shown below.  \describe{
#' \item{list("detailed")}{This is the old default which returns a list of
#' lists separating the outputs first in type and afterwards in variable
#' names.}\item{ (d)}{This is the old default which returns a list of lists
#' separating the outputs first in type and afterwards in variable names.}
#' \item{list("simple")}{This returns a list of outputs.}\item{ (s)}{This
#' returns a list of outputs.} \item{list("simplest")}{Behaves like "simple" if
#' more than one object is returned. However, if only one object is read from
#' gdx file the magpie object itself is returned getting rid of the surrounding
#' list structure. This is the recommended format for interactive use.}\item{
#' (st - default setting)}{Behaves like "simple" if more than one object is
#' returned. However, if only one object is read from gdx file the magpie
#' object itself is returned getting rid of the surrounding list structure.
#' This is the recommended format for interactive use.} \item{list("raw")}{This
#' returnes the data as it comes from \code{rgdx}. This is especially useful
#' the data should be written again to a gdx file without having much
#' transformations in between.  }\item{ (r)}{This returnes the data as it comes
#' from \code{rgdx}. This is especially useful the data should be written again
#' to a gdx file without having much transformations in between.  }
#' \item{list("first_found")}{This is a special format for the case that you
#' would like to read in exactly one object but you do not know exactly what
#' the name of the object is. Here, you can list all possible names of the
#' object and the function will return the first object of the list which is
#' found. This is especially useful writing read functions for gdx outputs of
#' models in which the names of a data object might change over time but the
#' function itself should work for all model versions. Having this format helps
#' to make your gdx-based functions backwards compatible to older versions of a
#' gdx file with different naming.}\item{ (f)}{This is a special format for the
#' case that you would like to read in exactly one object but you do not know
#' exactly what the name of the object is. Here, you can list all possible
#' names of the object and the function will return the first object of the
#' list which is found. This is especially useful writing read functions for
#' gdx outputs of models in which the names of a data object might change over
#' time but the function itself should work for all model versions. Having this
#' format helps to make your gdx-based functions backwards compatible to older
#' versions of a gdx file with different naming.} \item{list("name")}{In this
#' case the function returns the name of all objects found in the gdx which fit
#' to the given search pattern and the given type as vector.}\item{ (n)}{In
#' this case the function returns the name of all objects found in the gdx
#' which fit to the given search pattern and the given type as vector.} }
#' @param restore_zeros Defines whether 0s, which are typically not stored in a
#' gdx file, should be restored or ignored in the output. By default they will
#' be restored. If possible, it is recommended to use restore_zeros=TRUE. It is
#' faster but more memory consuming. If you get memory errors you should use
#' restore_zeros=FALSE
#' @param react determines the reaction, when the object you would like to read
#' in does not exist. Available options are "warning" (NULL is returned and a
#' warning is send that the object is missing), "silent" (NULL is returned, but
#' no warning is given) and "error" (The function throws out an error)
#' @param select preselection of subsets in the data coming from the gdx using
#' the function \code{\link[magclass]{mselect}}. Information has to be provided
#' as a list of selections (e.g. \code{select=list(type="level")}). See
#' \code{\link[magclass]{mselect}} for more information.
#' @param collapseNames Boolean which determines whether collapseNames should
#' be applied in \code{\link[magclass]{mselect}} or not.
#' @param magpie_cells (boolean) determines whether a set "j" gets special treatment
#' by replacing underscores in the set elements with dots. Active by default for
#' historical reasons. Can be ignored in most cases. Makes only a difference, if 
#' 1) GDX element depends on set "j", 2) set "j" contains underscores.
#' @return The gdx objects read in the format set with the argument
#' \code{format}.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{writeGDX}}, \code{\link[magclass]{mselect}}
#' @importFrom gdxrrw gdxInfo rgdx
#' @importFrom magclass as.magpie mselect is.magpie as.data.frame
#' @examples
#' 
#' \dontrun{readGDX("bla.gdx","blub*")}
#' 
#' 
#' 
readGDX <- function(gdx,...,types=c("sets","equations","parameters","variables","aliases"),field="All",format="simplest",restore_zeros=TRUE, react="warning", select=NULL, collapseNames=TRUE, magpie_cells=TRUE) {
  
.rgdx2array <- function(x, magpie_cells=TRUE) {
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
  
  #special treatment of set "j" -> replace underscores with dots!
  if(magpie_cells) {
    elem_j <- which(names(dim(out))=="j")
    if(length(elem_j)==1) dimnames(out)[[elem_j]] <- sub("_",".",dimnames(out)[[elem_j]])
  }
  
  #add additional information as attribute
  attr(out,"gdxdata") <- x[!names(x)%in%c("val","uels","dim","ts")] 
  attr(out,"description") <- x$ts
  return(out)
}

.rgdx2dataframe <- function(x,restore_zeros=FALSE, magpie_cells=TRUE) {
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
  
  #special treatment of set "j" -> replace underscores with dots!
  if(magpie_cells && length(out$j)>0) {
    out$j <- sub("_",".",out$j)
  }
  
  #add additional information as attribute
  attr(out,"gdxdata") <- x[!names(x)%in%c("val","uels","dim","ts")] 
  attr(out,"description") <- x$ts
  return(out)
}

  tmp <- switch(format,s="simple",st="simplest",d="detailed",r="raw",f="first_found",n="name")
  if(!is.null(tmp)) format <- tmp 
  
  types <- match.arg(types,several.ok=TRUE)
  
  allnames <- c(...)
  if(length(allnames)==0) {
    if(format=="first_found") stop("For format \"first_found\" you have to explicitly give all possible names of the object you would like to read in!")
    name <- "*"
  } else {
    name <- allnames
  }
  
  #translate name in standard regular expression syntax
  name <- paste("^",gsub("*",".*",name,fixed=TRUE),"$",sep="")
  
  #Only use types equations and variables if "level" is not part of the fields that should be adressed
  #(as all other types can only supply level values)
  if(field != "All" & field != "l") types <- intersect(c("equations","variables"),types)
  
  if(is.character(gdx)) {
    info <- gdxInfo(path.expand(gdx),dump=FALSE,returnDF=TRUE)
    
    if(format=="name") {
      i <- unlist(info[types])
      i <- i[grep(".name",names(i),fixed=TRUE,ignore.case = TRUE)]
      out <- NULL
      for(n in name) {
        out <- c(out,grep(n,i,value=TRUE,ignore.case = TRUE))
      }
      names(out) <- sub("\\..*$","",names(out))
      return(out)
    }
    
    # gdx is a path to a file that should be read
    out <- list()
    for(t in types) {
    rownames(info[[t]]) <- info[[t]][,"name"]
    if(format=="detailed") out[[t]] <- list()
    tmp <- NULL
    for(n in name) tmp <- c(tmp,grep(n,info[[t]][,"name"],value=TRUE,ignore.case = TRUE))
    for(i in tmp) {
      if(t=="variables"|t=="equations") l <- list(name=i,field=field,ts=TRUE)
      else l <- list(name=i,ts=TRUE)
      
      tmp2 <- rgdx(path.expand(gdx),l,squeeze=FALSE,followAlias=FALSE)
      attr(tmp2,"description") <- tmp2$ts
      if(format!="raw" & t!="aliases") {
        if(restore_zeros & t!="sets") {
          tmp2 <- .rgdx2array(tmp2, magpie_cells=magpie_cells)
        } else {
          tmp2 <- .rgdx2dataframe(tmp2, magpie_cells=magpie_cells)
        }
        if(t!="sets") {
          tmp2 <- as.magpie(tmp2,tidy=TRUE)
          if(!is.null(select)) {
            tmp2 <- mselect(tmp2,select,collapseNames=collapseNames)
          }
        } else {
          if(dim(tmp2)[2]==1) {
            tmp3 <- tmp2[[1]]
            attr(tmp3,"gdxdata") <- attr(tmp2,"gdxdata") 
            attr(tmp3,"description") <- attr(tmp2,"description")
            tmp2 <- tmp3
          }
        }
      }
      if(format!="detailed") out[[i]] <- tmp2
      else out[[t]][[i]] <- tmp2
    }
  }
  
  } else {
     # "gdx" is an already read in GDX object
     # in that case format data correctly and
     # apply a name filter on it
     if(format=="detailed") stop("Format \"detailed\" does not support a \"gdx\" argument which is not a path to a gdx file! If you want to apply readGDX on a already read-in GDX please use another format!")
     if(format=="name") stop("Format \"name\" does not support a \"gdx\" argument which is not a path to a gdx file! If you want to apply readGDX on a already read-in GDX please use another format!")
     
     if(is.list(gdx)) {
       if(all(names(gdx)%in%c("sets","equations","parameters","variables","aliases"))) {
         # detailed format
         out <- list()
         for(n in names(gdx)) out <- c(out,gdx[[n]])
       } else {
        # simple, simplest with more than one element or raw format 
        out <- gdx
       }
     } else if(is.magpie(gdx)) {
       # simplest with one elememt or first_found
       out <- list(gdx)
       names(out) <- attributes(gdx)$gdxdata$name
     }
     # now the data is formated according to either raw or simple format
     if(all(sapply(out,is.list))) {
       #data is raw format
       if(format!="raw") stop("Data supplied as read-in raw data but should be returned in another format. This does not work at the moment!")
     } else {
       if(format=="raw") stop("Data supplied as read-in and processed data but should be returned in raw format. This does not work!")
     }
     
     #apply name filter on data
     for(n in name) tmp <- c(tmp,grep(n,names(out),value=TRUE,ignore.case = TRUE))
     out <- out[unique(tmp)]
     
     #apply type filter on data
     alltypes <- c("sets","equations","parameters","variables","aliases")
     if(!all(alltypes %in% types)) {  
       stop("Restriction of types does not work at the moment for data which was already read in beforehand!")           
     }    
     
  }
  if(length(out)==0) {
    if(react=="error") stop("No corresponding object found in the GDX!")
    if(react=="warning") warning("No corresponding object found in the GDX. NULL is returned!")
    return(NULL)
  }
  if(format=="simplest" & length(out)==1) {
    return(out[[1]]) 
  } else if(format=="first_found") {
    assigned=FALSE
    first_name <- allnames[1]
    for(n in allnames){
      if(n %in% names(out)){
        x <- out[[n]]
        assigned=TRUE
        if(n != first_name & react!="silent") warning(first_name," not found in GDX! ", n , " returned")
        break
      }
      if(assigned==TRUE) break
    }
    
    if(assigned==FALSE) {
      if(react=="warning") warning("No element of ", paste(allnames,collapse=", ")," found in GDX! NULL returned")
      if(react=="error") stop("No element of ", paste(allnames,collapse=", ")," found in GDX!")
      return(NULL)
    }
    return(x)
  } else {
    return(out)
  }
}
