#' writeGDX
#' 
#' Function to write gdx files in R.
#' 
#' 
#' @usage writeGDX(x,file,period_with_y = TRUE)
#' @param x A list of objects or a single object that should be written to a
#' gdx file. The format is identical to what you get as output by using
#' \code{\link{readGDX}} (all formats provided by readGDX are supported by
#' writeGDX as well).
#' @param file File name of the gdx file
#' @param period_with_y Keep "y" in period dimension
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{readGDX}}
#' @importFrom gdxrrw wgdx
#' @importFrom magclass is.magpie ncells nregions as.data.frame
#' @examples
#' 
#' \dontrun{writeGDX(readGDX("input.gdx"),"test.gdx")}
#' 
#' 
#' 
writeGDX <- function(x,file,period_with_y = TRUE) {

.data2wgdx <- function(x,period_with_y,...) {
  #if x is a list it is assumed that x is already in the right output format
  if(is.list(x) & !is.data.frame(x)) return(x)
  
  #remove empty entries
  if(any(dim(x)==0)) return(NULL)
  
  #read gdx settings from attribute gdxdata
  out <- attr(x,"gdxdata")
  
  #add description
  out$ts <- attr(x,"description")
  
  #set value settings based on internal calculations
  out$form <- "sparse"

  #overwrite settings by additional arguments of the function call
  inarg <- list(...)
  for(i in names(inarg)) out[[i]] <- inarg[[i]]
  
  #calculate out$val, out$uels, out$dim and out$domains
  if(out$type=="set") {
    if(is.null(ncol(x))) {
      tmp <- as.data.frame(x)
      attr(tmp,"gdxdata") <- attr(x,"gdxdata") 
      attr(tmp,"description") <- attr(x,"description")
      x <- tmp
    }
    if(is.null(out$domains)) out$domains <- colnames(x)
    out$dim <- ncol(x)
    if(out$dim==1) {
      if(nrow(x)>0) {
        out$val <- matrix(1:nrow(x))
        out$uels <- list(as.vector(as.matrix(x)))
      } else {
        out$val <- matrix(1,0,1)
      }
    } else {
      out$uels <- list()
      out$val  <- NULL
      for(i in 1:ncol(x)) {
        out$uels[[i]] <- unique(x[,i])
        out$val <- cbind(out$val,match(x[,i],out$uels[[i]]))
      }
    }
  } else {
    if(is.magpie(x)) {
      out <- .magpie2wgdx(out,x,period_with_y)
    } else {
      out <- .array2wgdx(out,x)
    }
  }
  if(!is.null(out$uels)) {
    .tmp <- function(x) { if(all(is.null(x))) x <- character(0); return(x)}
    out$uels <- lapply(out$uels,.tmp)
  }
  return(out)
}

.array2wgdx <- function(out,x) {
  out$dim <- length(dim(x))
  out$uels <- dimnames(x)
  if(is.null(out$domains)) out$domains <- names(dimnames(x))
  out$val <- array(NA,dim=c(length(x),out$dim+1))
  if(out$dim>0) out$val[,1:out$dim] <- arrayInd(1:length(x),dim(x))
  out$val[,ncol(out$val)] <- x
  if(out$type=="variable"|out$type=="equation") {
    #if data is a variable or equation and field is not set yet, try to set it
    if(is.null(out$field)) {
      if("_field"%in%out$domains) out$field <- "all"
      else out$field <- "l"
    }
    #correct dim value if out$field="All"
    if(tolower(out$field)=="all") out$dim <- out$dim-1
  }
  return(out)
}

.magpie2wgdx <- function(out,x,period_with_y) {
  dx <- as.data.frame(x)
  if((ncells(x)!=nregions(x))) {
    dx$Region <- paste(dx$Region,dx$Cell,sep="_")
    names(dimnames(x))[1] <- sub("^[^\\.]\\.","",names(dimnames(x)[1]))
  }
  dx <- dx[-which(colnames(dx)=="Cell")]
  
  #kick out spatial and/or temporal dimension if not needed
  #rename columns based on set names
  if(all(dx$Region=="GLO")) {
    dx <- dx[-which(colnames(dx)=="Region")]
  } else {
    dx$Region <- as.factor(dx$Region)
    colnames(dx)[which(colnames(dx)=="Region")] <- names(dimnames(x))[1]
  }
  if(all(dx$Year==0)) {
    dx <- dx[-which(colnames(dx)=="Year")]
  } else {
    dx$Year <- as.factor(dx$Year)
    if(period_with_y) {
      levels(dx$Year) <- paste0("y",levels(dx$Year))
    } else {
      levels(dx$Year) <- gsub("^y","",levels(dx$Year))
    }
    colnames(dx)[which(colnames(dx)=="Year")] <- names(dimnames(x))[2]
  }
  
  if(all(dx$Data1=="NA")) {
    dx <- dx[-which(colnames(dx)=="Data1")]
  } else {
    if(is.null(names(dimnames(x))[3])) {
      datanames <- out$domains[!(out$domains %in% colnames(dx))]
    } else {
      datanames <- strsplit(names(dimnames(x))[[3]],"\\.")[[1]]
    }
    for(i in 1:length(datanames)) {
      dx[[paste0("Data",i)]] <- as.factor(dx[[paste0("Data",i)]])
      colnames(dx)[which(colnames(dx)==paste0("Data",i))] <- datanames[i]
    }
  }

  #reorder based on domain information (if available)
  if(is.null(out$domains)) {
    out$domains <- colnames(dx)[colnames(dx)!="Value"]
  } else {
    if(length(out$domains)>0) dx <- dx[c(out$domains,"Value")]
  }
    
  out$dim <- ncol(dx)-1
  out$uels <- lapply(dx[colnames(dx)!="Value"],levels)
  for(i in 1:ncol(dx)) dx[[i]] <- as.numeric(dx[[i]])
  out$val <- as.matrix(dx)
  
  if(out$type=="variable"|out$type=="equation") {
    #if data is a variable or equation and field is not set yet, try to set it
    if(is.null(out$field)) {
      if("_field"%in%out$domains) out$field <- "all"
      else out$field <- "l"
    }
    #correct dim value if out$field="All"
    if(tolower(out$field)=="all") out$dim <- out$dim-1
  }
  return(out)
}


  if(!is.list(x)) x <- list(x)
  if(all(c("val","dim","uels","domains") %in% names(x))) x <- list(x)
  if(!is.null(names(x)) & all(names(x) %in% c("aliases","sets","equations","parameters","variables"))) x <- unlist(x,recursive=FALSE)
  len <- sapply(x,length)
  if(any(len==0)) {
    w <- which(len==0)
    warning("Empty objects are ignored by writeGDX and not written to gdx file (",paste0(names(x)[w],collapse=", "),")")
    x <- x[-w]
  }
  x <- lapply(x,.data2wgdx,period_with_y)
  names(x) <- NULL
  trash <- wgdx(path.expand(file),x,squeeze='n')
}