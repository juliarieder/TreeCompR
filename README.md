
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TreeCompR

<!-- badges: start -->

<img src="man/figures/hex-TreeCompR.png" height="180" alt="Hexsticker" style="float: right;">
R Package for calculating distance-dependent individual Tree Competition
on Plot level <!-- badges: end -->

This R package is designed for the quantitative analysis of individual
tree competition within forest ecosystems. It accepts inputs such as
LiDAR point clouds (as .txt or las/laz files) of forest plots or
individual target trees, and inventory tables (including tree ID, X, Y,
DBH, H). By leveraging a range of tools and algorithms, it allows to
assess and measure tree-to-tree competition, providing valuable insights
for forest ecology, inventory assessments, and forest management
strategies. This package facilitates a systematic approach to
understanding the interactions and resource competition among trees,
enabling informed decision-making in forest management.

## Installation

You can install the development version of TreeCompR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juliarieder/TreeCompR")
```

## Example

This is how to calculate the Hegyi Index

``` r
library(TreeCompR)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
## basic example code
```
