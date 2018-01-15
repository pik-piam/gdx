#' calc_scaling
#' 
#' This function creates a GAMS file with scaling of variables. The scaling is
#' calculated based on a gdx file containing all variables of a run.
#' 
#' 
#' @usage calc_scaling(gdx, file=NULL, magnitude=2)
#' @param gdx a GDX list as created by readGDX, or the file name of a gdx file
#' @param file A file name the scaling GAMS code should be written to. If NULL
#' the code is returned by the function
#' @param magnitude The order of magnitude for which variables should be
#' scaled. All variables with average absolute values which are either below
#' 10e(-magnitude) or above 10e(magnitude) will be scaled.
#' @return A vector with the scaling GAMS code if file=NULL, otherwise nothing
#' is returned.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{out}},\code{\link{readGDX}}
#' @examples
#' 
#' \dontrun{calc_scaling("fulldata.gdx")}
#' 
calc_scaling <- function(gdx,file=NULL,magnitude=2) {
  if(is.character(gdx)) {
    v <- readGDX(gdx,types="variables",field="l")
  } else {
    if("variables" %in% names(gdx)) v <- gdx$variables
    else v <- gdx
  }
  out <- NULL
  for(x in names(v)) {
    #calculate order of magnitude (oof)
    oof <- round(log10(mean(abs(v[[x]]))))
    if(is.nan(oof)) oof <- 0
    cat("\n oof =",oof,"  ",x)
    if(length(attr(v[[x]],"gdxdata")$domains)==0) {
      sets <- ""
    } else {
      sets <- paste("(",paste(attr(v[[x]],"gdxdata")$domains,collapse=","),")",sep="")
    }
    if(oof!=-Inf & (oof < -1*magnitude | oof > 1*magnitude)) out <- c(out,paste(x,".scale",sets," = 10e",oof,";",sep=""))
  }
  cat("\n\n")

  if(!is.null(file)) {
    writeLines(out,file)
  } else {
    return(out)
  }
}