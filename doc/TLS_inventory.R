## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE,warning=FALSE---------------------------------------
library(TreeCompR)
library(TreeLS)

## ----segment-TLS--------------------------------------------------------------
    file <- system.file("extdata", "pine_plot.laz", package="TreeLS")
    tls <- readTLS(file)
    tls_norm <- tlsNormalize(tls, keep_ground = F)
    thin <- tlsSample(tls_norm, smp.voxelize(0.01))
    map <- treeMap(thin, map.hough(min_density = 0.1), 0)
    tls_t <- treePoints(tls_norm, map, trp.crop())
    tls_stem <- stemPoints(tls_t, stm.hough())
    inv <- tlsInventory(tls_stem, d_method=shapeFit(shape='circle', algorithm = 'ransac'))
    inv$dbh <- (inv$Radius *2)

## ----calculate-CI, message=FALSE,warning=FALSE, fig.height= 4, fig.width=7----
inventory <- read_inv(inv_source = inv, dbh_unit = "m", height_unit = "m")
targets <- define_target(inv = inventory, target_source = "buff_edge", radius = 2)
plot_target(targets)
compete_inv(inventory, target_source = "buff_edge", radius = 2, method = "all_methods")

