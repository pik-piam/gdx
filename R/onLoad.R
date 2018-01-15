#' @importFrom gdxrrw igdx 
.onLoad <- function(libname, pkgname){
  #Set path to GAMS installation
  delimiter <- ifelse(Sys.info()["sysname"]=="Windows",";",":")
  gamspath <- grep("gams",strsplit(Sys.getenv("PATH"),delimiter)[[1]],value=TRUE,ignore.case=TRUE)
	gamspath <- grep("%",gamspath,value=TRUE,invert=TRUE)
	tmp <- NULL
	ok <- FALSE
	sink(textConnection("tmp","w",local=TRUE))
  for(path in gamspath){
	  if(igdx(path)==1) {
	    ok <- TRUE
	    break
	  }
  }
	sink()
	if(!ok) warning(paste(tmp,collapse = "\n"))
}