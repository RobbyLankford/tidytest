
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytest

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The goal of tidytest is to provide a tidy unified interface to common
statistical tests, specifically those used when developing and
validating statistical models.

## Installation

You can install the development version of tidytest from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RobbyLankford/tidytest")
```

## Usage

### Stationarity

tidytest currently contains the following stationarity tests:

-   Augmented Dickey-Fuller (ADF)
-   Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

``` r
library(tidytest)

set.seed(123)
x <- runif(n = 1000)

adf_test(x)
kpss_test(x)
```
