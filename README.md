# Input data generation for the EDGE-Transport model

R package **mrtransport**, version **0.11.7**

[![CRAN status](https://www.r-pkg.org/badges/version/mrtransport)](https://cran.r-project.org/package=mrtransport) [![R build status](https://github.com/pik-piam/mrtransport/workflows/check/badge.svg)](https://github.com/pik-piam/mrtransport/actions) [![codecov](https://codecov.io/gh/pik-piam/mrtransport/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrtransport) [![r-universe](https://pik-piam.r-universe.dev/badges/mrtransport)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

The mrtransport package contains data preprocessing for the
    EDGE-Transport model.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrtransport")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Johanna Hoppe <johanna.hoppe@pik-potsdam.de>.

## Citation

To cite package **mrtransport** in publications use:

Hoppe J, Muessel J, Hagen A, Dirnaichner A (2025). "mrtransport: Input data generation for the EDGE-Transport model." Version: 0.11.7, <https://github.com/pik-piam/mrtransport>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrtransport: Input data generation for the EDGE-Transport model},
  author = {Johanna Hoppe and Jarusch Muessel and Alex K. Hagen and Alois Dirnaichner},
  date = {2025-07-24},
  year = {2025},
  url = {https://github.com/pik-piam/mrtransport},
  note = {Version: 0.11.7},
}
```
