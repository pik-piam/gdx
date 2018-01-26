#' inp - \bold{DEPRECEATED!}
#' 
#' This function is depreceated, please do not use it! Please see the note
#' below for more information. Function to savely read parameters and sets from
#' GDX file. The function creates a warning if the parameter does not exist in
#' the gdx file. Please use this function when you write own GDX output
#' functions.
#' 
#' 
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx file
#' (file name is recommended as this speeds up the code)
#' @param name name of the parameter or set that should be read
#' @param ... Additional names. If the attempt to read 'name' fails, these will
#' be tried in the order given by the user until one is successfully read.
#' @param react determines the reaction, when the parameter or set does not
#' exist. Available options are "warning" (NULL is returned and a warning is
#' send that the parameter or set is missing), "silent" (NULL is returned, but
#' no warning is given) and "error" (The function throws out an error)
#' @param as.magpie If TRUE the content is returned as a MAgPIE object,
#' otherwise as array
#' @return The parameter if it exists, otherwise NULL
#' @note \bold{This function is depreceated! Please use
#' \code{\link{readGDX}(...,format="first_found")} instead! This will basically
#' give you the same functionality with the only difference that it will return
#' a magpie object instead of an array.}
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{out}}, \code{\link{readGDX}}
#' @examples
#' 
#' \dontrun{inp("fulldata.gdx","x_modelstat")}
#' 
inp <- function(gdx,name,...,react="warning",as.magpie=FALSE) {
  warning("Functions \"inp\" is depreceated. Please use readGDX(...,format=\"first_found\") instead!")
  
  allnames<-c(name,c(...))

  if(!is.list(gdx)) {
    gdx <- old_readGDX(gdx,name=c(name,...),format="simple")
  }

  assigned=FALSE
  for(n in allnames){
    if(n %in% names(gdx)){
      x <- gdx[[n]]
      assigned=TRUE
      if(n!= name & react!="silent") warning(name," not found in GDX file! ", n , " returned")
      break
    }
    if(assigned==TRUE) break
  }

  if(assigned==FALSE) {
    if(react=="warning") warning("No element of ", allnames," found in GDX file! NULL returned")
    if(react=="error") stop("No element of ", allnames," found in GDX file!")
    return(NULL)
  }
  if(as.magpie) {
    gdxdata <- attr(x,"gdxdata")
    description <- attr(x,"description")
    x <- as.magpie(x)
    attr(x,"description") <- description
    attr(x,"gdxdata") <- gdxdata
  }
  return(x)
}
