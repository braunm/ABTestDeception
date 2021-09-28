
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ```{r, include = FALSE} -->
<!-- knitr::opts_chunk$set( -->
<!--   collapse = TRUE, -->
<!--   comment = "#>", -->
<!--   fig.path = "man/figures/README-", -->
<!--   out.width = "100%" -->
<!-- ) -->
<!-- ``` -->

# The official site of Braun and Schwartz (2021) “The A/B Test Deception…”

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/adTargApp)](https://CRAN.R-project.org/package=adTargApp)
<!-- badges: end -->

## News

The working paper is available on
[SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3896024)

The goal of ABTestDeception is to illustrate the effects of divergent
delivery and response heterogeneity on bias in results of online
advertising A/B tests, as described in Braun and Schwartz (2021).

## Now, with a Shiny App!!!!

To avoid lots of issues with hosting, authentication, credentials, etc.,
we’re just going to share the app as an R package. Once installed and
loaded, you can run the app with just one command.

### Installation

You can install the released version of ABTestDeception from github

``` r
install_github("braunm/ABTestDeception")
```

### Running the app

Once you have installed the package, load the package and launch the app

``` r
library(ABTestDeception)
ABTestDeception::launch()
```
