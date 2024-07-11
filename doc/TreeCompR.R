## ----example1, message=FALSE, warning=FALSE, eval=FALSE-----------------------
#  library(TreeCompR)
#  ## insert path to point cloud of the forest plot and to the target tree point cloud
#  compete_pc(forest_source = "../tests/testthat/testdata/neighborhood.txt",
#             tree_source = "../tests/testthat/testdata/tree.txt",
#             comp_method = "cone",
#             h_cone = 0.6)

## ----example2, message=FALSE, warning=FALSE, eval=FALSE-----------------------
#  ## or the cylinder method with radius 5 m
#  compete_pc(forest_source = "../tests/testthat/testdata/neighborhood.txt",
#             tree_source = "../tests/testthat/testdata/tree.txt",
#             comp_method = "cylinder", cyl_r = 5)
#  

## ----example3, message=FALSE, warning=FALSE, eval=FALSE-----------------------
#  ## check or define target trees: read inventory file, define targets, plot results
#  plot <- read_inv("../tests/testthat/testdata/inventory.csv", verbose = FALSE)
#  targets <- define_target(plot,target_source = "buff_edge", radius = 10)

## ----fig1, fig.height= 3, fig.width=6, eval=FALSE-----------------------------
#  ## plot the positions of the target trees and trees at the border
#  plot_target(targets)

## ----example4, message=FALSE, warning=FALSE, eval=FALSE-----------------------
#  ## insert path to inventory table or insert dataframe object
#  compete_inv(inv_source = plot,
#              target_source = targets,
#              radius = 10, method = "all")
#  

