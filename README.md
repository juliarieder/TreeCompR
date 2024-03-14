
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TreeCompR

<!-- badges: start -->

<img src="man/figures/logo.png" height="180" alt="Hexsticker" style="float: right;">
R Package for calculating distance-dependent individual Tree Competition
on Plot level

<!-- badges: end -->

This R package is designed for the quantitative analysis of individual
tree competition within forest ecosystems. It accepts inputs such as
LiDAR point clouds (as dataframe with xyz columns or las/laz files) of
forest plots and individual target trees, or inventory tables (including
tree ID, x, y, dbh, h). Through the use of various competition indices,
it enables the assessment and measurement of competition between trees,
providing valuable insights for forest ecology, inventories, and forest
management strategies. This package facilitates a systematic approach to
understanding interactions and resource competition among trees and
enables informed decision making in forest management.

## Installation

You can install the development version of TreeCompR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juliarieder/TreeCompR")
```

## Methodology

LiDAR point clouds can be used directly to quantify the competition
exerted by neighboring trees on the target tree. For this purpose, the
target tree for which the competition is to be determined should be
segmented beforehand. Since it is crucial whether a part of the point
cloud is classified as “competing” or as part of the target tree itself,
a manual segmentation (e.g. in CloudCompare) is most accurate.

<img src="man/figures/cone_cyl_method.jpg" alt="Methods Workflow" width="900">

## Example

This is how to quantify the competition for one target tree using the
cone method:

``` r
library(TreeCompR)
## insert path to point cloud of the forest plot and to the target tree point cloud
compete_pc("tests/testthat/testdata/neighborhood.txt", "tests/testthat/testdata/tree.txt", "cone")
#> ----- Processing competition indices for: tree -----
#>  ------------------------------------------------------------------
#>  Point cloud based competition indices for 'tree' 
#>  ------------------------------------------------------------------
#>   target height_target center_position CI_cone h_cone
#> 1   tree          22.8    crown center   16046    0.6
#>  ------------------------------------------------------------------
#>  Point cloud based competition indices for 'tree' 
#>  ------------------------------------------------------------------
#>   target height_target center_position CI_cone h_cone
#> 1   tree          22.8    crown center   16046    0.6
```
