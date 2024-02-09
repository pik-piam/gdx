#' @importFrom gdxrrw igdx
#' @importFrom utils capture.output
.onLoad <- function(libname, pkgname) {
  path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  path <- grep("gams", path, ignore.case = TRUE, value = TRUE)
  # disregard variables on the Windows path
  path <- grep("%", path, value = TRUE, fixed = TRUE, invert = TRUE)

  # append GAMSROOT (or empty if that does not exist) to make sure igdx is
  # called at least once
  path <- c(path, Sys.getenv("GAMSROOT"))

  for (p in path) {
    msg <- capture.output(ok <- as.logical(igdx(p)))
    if (ok)
      break
  }

  if (!ok) {
    # truncate igdx output to 132 characters per line
    msg <- paste0(strtrim(msg, 129), c("", "...")[(nchar(msg) > 132) + 1])
    packageStartupMessage(paste(msg, collapse = "\n"))
  }
}
