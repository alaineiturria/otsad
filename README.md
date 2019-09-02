<!-- README.md is generated from README.Rmd. Please edit that file -->
otsad
=====

*Online Time Series Anomaly Detectors*

This package provides anomaly detectors in the context of online time series and their evaluation with the Numenta score.

Installation
------------

### Dependencies

CAD-OSE algorithm is implemented in Python. It uses [bencode](https://pypi.org/project/bencode-python3/) library in the hashing step. This dependency can be installed with the Python package manager *pip*.

``` sh
$ sudo pip install bencode-python3
```

### otsad package

You can install the released version of otsad from [CRAN](https://CRAN.R-project.org) with:

``` r
# Get the released version from CRAN
install.packages("otsad")

# Get the latest development version from GitHub
devtools::install_github("alaineiturria/otsad")
```

Example
-------

This is a basic example of the use of otsad package:

``` r
library(otsad)

## basic example code
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE) # distributional shift
x[25] <- 200 # abrupt transient anomaly
x[320] <- 170 # abrupt transient anomaly
df <- data.frame(timestamp = 1:n, value = x)


result <- CpSdEwma(data = df$value, n.train = 5, threshold = 0.01, l = 3)
```

``` r
res <- cbind(df, result)
PlotDetections(res, title = "SD-EWMA ANOMALY DETECTOR", return.ggplot = TRUE)
```

<img src="man/figures/README-plot.Anomaly-1.png" width="100%" />

For more details, see otsad documentation and vignettes.
