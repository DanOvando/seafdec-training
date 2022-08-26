
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seafdec-training

<!-- badges: start -->
<!-- badges: end -->

A repository for SEAFDEC training

## Setup

This project is set up with
[`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage
package dependencies. Inside R (and with your working directory set
correctly) run `renv::restore()`. Follow all prompts. This will install
the correct versions of all the packages needed to replicate our
results. Packages are installed in a stand-alone project library for
this paper, and will not affect your installed R packages anywhere else.

## Data

You can place relatively small (\< 10s of MB) data in the “data” folder
to share.

``` r
library(sraplus)
library(LBSPR)
library(JABBA)
```
