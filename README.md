
# NetRecover

<!-- badges: start -->
<!-- badges: end -->

NetRecover is an R package for estimating and evaluating Gaussian graphical models 
using the Graphical LASSO algorithm. It provides functions for simulating Gaussian 
data, estimating sparse precision matrices, and evaluating network recovery 
performance. Users can also provide their own data to investigate different scenarios.

## Installation

You can install the development version of NetRecover from [GitHub](https://github.com/chyang23/NetRecover) with:

``` r
# install.packages("pak")
pak::pak("chyang23/NetRecover")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(NetRecover)
## basic example code will be here later
```

## Remaining work
The remaining work includes completing the Graphical LASSO function in R and 
move this part of the algorithm to C++ using RcppArmadillo. Also I need to 
complete the plotting functions plot_sim() and plot_network(). After this part,
I will improve documentations such as the incomplete roxygen skeleton 
and some comments, also I will create a vignette.
