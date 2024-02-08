#' @importFrom gdxrrw igdx
#' @importFrom withr with_output_sink
.onLoad <- function(libname, pkgname) {
  tmp <- NULL
  with_output_sink(new = textConnection("tmp", "w", local = TRUE),
                   code = {
                     # make igdx try an empty path to load GDX libraries, which
                     # will fail and igdx will try the path and library search
                     # mechanisms in turn
                     ok <- as.logical(igdx(""))
                   })

  if (!ok) {
    # truncate igdx output to 132 characters per line
    tmp <- paste0(strtrim(tmp, 129), c('', '...')[(nchar(tmp) > 132) + 1])
    packageStartupMessage(paste(tmp, collapse = "\n"))
  }
}
