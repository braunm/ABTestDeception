
<!-- README.md is generated from README.Rmd. Please edit that file -->

`{r, include = FALSE} knitr::opts_chunk$set(   collapse = TRUE,   comment = "#>",   fig.path = "man/figures/README-",   out.width = "100%" )`

# adTargApp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/adTargApp)](https://CRAN.R-project.org/package=adTargApp)
<!-- badges: end -->

The goal of ABTestDeception is to illustrate the effects of divergent
delivery and response heterogeneity on bias in results of online
advertising A/B tests, as described in Braun and Schwartz (2021).

## Installation

You can install the released version of ABTestDeception from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("adTargApp")
```

## Example

This is a basic example which shows you how to solve a common problem:

`{r example} library(ABTestDeception) ## basic example code`

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

`{r cars} summary(cars)`

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

`{r pressure, echo = FALSE} plot(pressure)`

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
