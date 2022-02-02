
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FCM

<!-- badges: start -->
<!-- badges: end -->

FCM package contains functions for matrix based computations of the
Fuzzy Cognitive Map application to solve decision making problems,
modeling and simulating complex systems. Package consists of R scripts
each of one containing sets of functions in order to compute system
parameters and reverse task solution of the fuzzy map.

## Requirements

To see an FCM package in action follow the instructions in the README
for the [FCM app](https://github.com/alinapetukhova/FCM.git).

## Installation

You can install the released version of FCM by:

``` r
# install.packages("devtools")
devtools::install_github("alinapetukhova/FCM_package")
```

## Usage

The example usage of the eigen_module function is as follows:

``` r
library(FCM)
eigen_module(matrix) # the function has a matrix as an argument and returns an eigen module of the matrix
```
The example usage of the einstein_product_tnorm function is as follows:

``` r
library(FCM)
einstein_product_tnorm(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) 
# the function has elements and t-norms as arguments and returns the t-norm based on Einstein product

