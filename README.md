
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TreeCompR

<!-- badges: start -->

<img src="man/figures/logo.png" height="180" alt="Hexsticker" style="float: right;">
R Package for calculating distance-dependent or point cloud-based
individual tree competition indices

<!-- badges: end -->

This R package is designed for the quantitative analysis of individual
tree competition within forest ecosystems. It accepts inputs such as
LiDAR point clouds (as dataframe with xyz columns or las/laz files) of
forest plots and individual target trees, or inventory tables (including
tree ID, x, y, dbh and/or h). Through the use of various competition
indices, it enables the assessment and measurement of competition
between trees, providing valuable insights for forest ecology,
inventories, and forest management strategies. This package facilitates
a systematic approach to understanding interactions and resource
competition among trees and enables informed decision making in forest
management.

## Installation

You can install the development version of TreeCompR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juliarieder/TreeCompR", dependencies = TRUE)

# or with vignettes on your computer
devtools::install_github("juliarieder/TreeCompR", 
            dependencies = TRUE, build_vignettes = TRUE)
```

## Overview

TreeCompR can read point clouds or inventory tables and quantifies tree
competition in different ways:

- Functions for **point-cloud based methods**
  - `compete_pc()` quantifies tree competition from point clouds using
    the cone or cylinder method (see [Metz et
    al. 2013](https://doi.org/10.1016/j.foreco.2013.08.014);[Seidel et
    al. 2015](http://dx.doi.org/10.1016/j.foreco.2014.10.020))
  - `read_pc()` reads 3D point clouds from .las/.laz files, .ply files
    and text-only file formats such as .txt or .csv, and converts LAS
    objects and data.frames to a TreeCompR compatible format
  - `tree_pos()` computes the base position and height of target tree
    point clouds
- Functions for **inventory-based methods**
  - `compete_inv()` quantifies size- and distance-dependent competition
    using inventory data based on a series of published indices
  - `read_inv()` reads and validates inventory tables from data.frames,
    or reads tabular data directly from text-only file sources such as
    .txt or .csv
  - `define_target()` can be used to define for which trees within a
    plot the competition should be quantified in `compete_inv()`
  - `plot_target()` check and validate the tree position(s) and the
    surrounding trees after defining them with `define_target()`

## Get started

Have a look at some examples to check out what our package can do on
[Get
started](https://juliarieder.github.io/TreeCompR/articles/TreeCompR.html).

## Workflows and Tutorials

- [Inventory-based
  approach](https://juliarieder.github.io/TreeCompR/articles/competition-inventory.html)
  Overview on inventory based competition indices
  - [ALS
    workflow](https://juliarieder.github.io/TreeCompR/articles/ALS_inventory.html)
    learn how to pre-process and use ALS data (airborne laser scanning)
    to derive size-distance-based competition indices
  - [TLS
    workflow](https://juliarieder.github.io/TreeCompR/articles/TLS_inventory.html)
    learn how to pre-process and use TLS/MLS data (terrestrial or mobile
    laser scanning) to derive size-distance-based competition indices
- [Point-cloud-based
  approach](https://juliarieder.github.io/TreeCompR/articles/competition-pointcloud.html)
  Overview on quantifying competition directly from point clouds from
  ground-based laser scans and how to pre-process the data for the cone
  or cylinder method
