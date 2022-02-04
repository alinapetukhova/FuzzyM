# FuzzyM

<!-- badges: start -->
<!-- badges: end -->

'FuzzyM' package contains functions for matrix based computations of the
Fuzzy Cognitive Map application to solve decision making problems,
modeling and simulating complex systems. Package consists of R scripts
each of one containing sets of functions in order to compute system
parameters and reverse task solution of the fuzzy map.

## Requirements

To see an 'FuzzyM' package in action follow the instructions in the README
and see the examples in the "examles" folder.

## Installation

You can install the released version of FuzzyM by:

``` r
# install.packages("devtools")
devtools::install_github("alinapetukhova/FuzzyM")
```

## Usage

The example usage of the eigen_module function is as follows:

``` r
library(FuzzyM)
eigen_module(matrix) # the function has a matrix as an argument and returns an eigen module of the matrix
```
The example usage of the einstein_product_tnorm function is as follows:

``` r
library(FuzzyM)
einstein_product_tnorm(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) 
# the function has elements and t-norms as arguments and returns the t-norm based on Einstein product

