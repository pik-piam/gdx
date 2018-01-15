#' expand.set
#' 
#' Function which expands a GAMS set based on a comparison set. If strings in
#' the set, which should be expanded, exist in the fullset, they are used
#' directly, otherwise it is searched for a set in the gdx file having that
#' name
#' 
#' 
#' @usage expand.set(gdx,x,fullset=NULL)
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx file
#' (file name is recommended as this speeds up the code)
#' @param x A vector of strings which should be used as a set. Strings are
#' either set element names or names of whole sets.
#' @param fullset a vector of strings used as comparison. If only a single
#' string is supplied it is checked whether this string exists as set in the
#' given gdx file and expanded. In all other cases it is assumed that the
#' strings are single set elements. x must be a subset of fullset! If fullset
#' is NULL it is just assumed that x contains only set names which all have to
#' be expanded.
#' @return The expanded set vector.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{readGDX}}
#' @examples
#' 
#' \dontrun{expand.set("fulldata.gdx","kbe","kcr")}
#' 
expand.set <- function(gdx,x,fullset=NULL) {
  if(length(fullset)==1) {
    tmp <- readGDX(gdx,fullset,format="first_found")
    if(!is.null(tmp)) fullset <- as.matrix(tmp)
  }
  if(any(!(x %in% fullset))) {
      tmp <- x[!(x %in% fullset)]
      x <- x[(x %in% fullset)]
      for(t in tmp) {
        tmp2 <- readGDX(gdx,t,format="first_found")
        if(!is.null(tmp2)) x <- c(x,as.matrix(tmp2))
      }
    }
  return(x)
}

