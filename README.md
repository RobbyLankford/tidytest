
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

``` r
library(dplyr)
library(parsnip)
library(tidytest)

mod_fit <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::fit(mpg ~ disp + wt + hp, data = mtcars)
```

### Linearity

``` r
ramsey_reset(mod_fit)
```

### Independence

``` r
durbin_watson_test(mod_fit)
ljung_box_test(mod_fit)
```

### Normality

``` r
anderson_darling_test(mod_fit)
shapiro_wilk_test(mod_fit)
```

### Equal Variance

``` r
bruesch_pagan_test(mod_fit)
```
