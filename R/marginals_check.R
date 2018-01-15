#' marginals_check
#' 
#' This function returns a list of the highest marginals found in gdx file
#' allowing to detect the most critical constraints in a given solution.
#' 
#' 
#' @usage marginals_check(gdx, ifilter=NULL, efilter=NULL, limit=1e5,
#' scientific=TRUE, unlist=FALSE, name="o(v|q)*")
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx file
#' @param ifilter Inclusion filter. A vector of strings containing regular
#' expressions which describe strings that have to be part of the found
#' marginal names. Otherwise they will be excluded.
#' @param efilter Exclusion filter. A vector of strings containing regular
#' expressions which describe strings must not be part of the found marginal
#' names. Otherwise they will be excluded.
#' @param limit Lower limit for the absolute value of the marginal in order to
#' be part of the output.
#' @param scientific Boolean which decides whether the marginals should be
#' written scientfically (e.g. 1e+2) or the default output scheme should be
#' used.
#' @param unlist If TRUE a vector sorted by the rank of the marginal will be
#' returned, otherwise a structured list of outputs will be returned.
#' @param name search string defining the objects that should be read from gdx
#' file, with *-autocompletion. Can also be a vector containing more than one
#' search strings
#' @return A vector or list containing all marginals which absolute values are
#' above the given limit and which agree with the given filters.
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom magclass wrap as.data.frame
#' @seealso \code{\link{readGDX}}
#' @examples
#' 
#' \dontrun{marginals_check("fulldata.gdx")}
#' 

marginals_check <- function(gdx,ifilter=NULL,efilter=NULL,limit=1e5,scientific=TRUE,unlist=FALSE,name="o(v|q)*"){
  gdx <- readGDX(gdx,name,types="parameters",format="detailed")
  tmp <- unlist(lapply(gdx,lapply,wrap))
  marginals <- tmp[grep("\\.marginal$",names(tmp))]
  if(all(abs(marginals)<=limit)) {
    cat("No marginals found above given limit!\n")
    return(NULL)
  }
  marginals <- marginals[abs(marginals)>=limit]
  unlink(tmp)
  if(!is.null(ifilter)){
    for(f in ifilter) marginals <- marginals[grep(f,names(marginals))]
  }
  if(!is.null(efilter)){
    for(f in efilter) marginals <- marginals[-grep(f,names(marginals))]
  }
  if(length(marginals)==0) {
    cat("No marginals found above given limit for given filters!\n")
    return(NULL) 
  }
  names <- names(sort(abs(marginals)))
  out <- marginals[names[length(names):1]]
  names(out) <- sub("^(parameters|variables|equations)\\.","",names(out))
  names(out) <- sub("\\.marginal$","",names(out))
  out <- round(out)
  if(scientific) out <- format(out,scientific=TRUE,digits=1)
  
  if(!unlist) {
    obj <- unique(sub("(^[^\\.]*)\\..*$","\\1",names(out)))
    outlist <- list()
    for(o in obj){
      ranks <- grep(paste("^",o,"\\.",sep=""),names(out))
      value <- out[ranks]
      names(value) <- sub(paste("^",o,"\\.",sep=""),"",names(value))
      tmp2 <- t(matrix(unlist(strsplit(names(value),"\\.")),ncol=length(value)))
      tmp2 <- cbind(tmp2,value)
      dimnames(tmp2)[[1]] <- ranks
      outlist[[o]] <- as.data.frame(tmp2) 
    }
    out <- outlist
  }
  cat(length(marginals),"marginals with abs >",limit,"found!\n")
  return(out)
}

