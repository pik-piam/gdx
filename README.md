# Interface package for GDX files in R

R package **gdx**, version **1.53.1**

[![CRAN status](https://www.r-pkg.org/badges/version/gdx)](https://cran.r-project.org/package=gdx) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158598.svg)](https://doi.org/10.5281/zenodo.1158598) [![R build status](https://github.com/pik-piam/gdx/workflows/check/badge.svg)](https://github.com/pik-piam/gdx/actions) [![codecov](https://codecov.io/gh/pik-piam/gdx/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/gdx) [![r-universe](https://pik-piam.r-universe.dev/badges/gdx)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A wrapper package for the gdxrrw extending its functionality
    and allowing to read and write GDX files directly in R.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("gdx")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **gdx** in publications use:

Dietrich J, Giannousakis A, Bonsch M (2024). _gdx: Interface package for GDX files in R_. doi: 10.5281/zenodo.1158598 (URL: https://doi.org/10.5281/zenodo.1158598), R package version 1.53.1, <URL: https://github.com/pik-piam/gdx>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {gdx: Interface package for GDX files in R},
  author = {Jan Philipp Dietrich and Anastasis Giannousakis and Markus Bonsch Bonsch},
  year = {2024},
  note = {R package version 1.53.1},
  doi = {10.5281/zenodo.1158598},
  url = {https://github.com/pik-piam/gdx},
}
```
