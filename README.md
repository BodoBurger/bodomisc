# bodomisc

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


# Installation of the package

Install the development version from GitHub (using `devtools`):

```r
devtools::install_github("BodoBurger/bodomisc")
```

I put this package in my [.Rprofile](https://csgillespie.github.io/efficientR/set-up.html#rprofile) file using the following lines:

```
if (interactive()) {
  suppressMessages(require(bodomisc))
}
```

# Documentation

- [vignette on Github](https://github.com/BodoBurger/bodomisc/blob/master/vignettes/vignette.md)

# Links
- [When to use print(), cat(), message(), warning(), stop()](https://stackoverflow.com/questions/36699272/why-is-message-a-better-choice-than-print-in-r-for-writing-a-package)
- [colored console output](https://github.com/r-lib/crayon)
