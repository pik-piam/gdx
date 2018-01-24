# gdx package

## Purpose and Functionality

The gdx package is a wrapper package for the gdxrrw R package distributed with GAMS. It tries to extend and simplify the access to gdx files from the original package.

## Installation

Before the `gdx` package can be installed `gdxrrw` needs to be installed. 

```r
download.file("https://support.gams.com/_media/gdxrrw:gdxrrw_1.0.2.zip",
              "gdxrrw_1.0.2.zip")
install.packages("gdxrrw_1.0.2.zip",repos = NULL)
```

For installation of the most recent package version an additional repository can be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```

The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("gdx")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.


## Citation 

[![DOI](https://zenodo.org/badge/117549288.svg)](https://zenodo.org/badge/latestdoi/117549288)


