## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(lidR)
library(TreeCompR)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
r3dDefaults = rgl::r3dDefaults
m = structure(c(0.921, -0.146, 0.362, 0, 0.386, 0.482, -0.787, 0, 
                -0.06, 0.864, 0.5, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
r3dDefaults$FOV = 50
r3dDefaults$userMatrix = m
r3dDefaults$zoom = 0.75

library(sf)
library(ggplot2)

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile)
chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))


options(crayon.enabled = TRUE)
rgl::setupKnitr(autoprint = TRUE)
    

## ----plot-las, echo = FALSE, rgl = TRUE, fig.width = 5, fig.height = 4--------
plot(las, size = 3, bg = "white", legend = TRUE)

## -----------------------------------------------------------------------------
# create CHM raster from point cloud with 0.5 m resolution (adjust values if needed)
chm_p2r_05 <- rasterize_canopy(las, 0.5, p2r(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)

#locate tree tops
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))

## ----plot-chm, fig.height=5, fig.width=5--------------------------------------
col <- height.colors(50)
plot(chm_p2r_05_smoothed, main = "CHM P2R 0.5 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_05_smoothed), add = T, pch =3)

## ----plot-las-its, rgl = TRUE, fig.width = 4, fig.height = 4, warning = FALSE----
algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(las, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

## ----plot-tree-hulls-its, fig.height=4, fig.width=4---------------------------
crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

## ----plot-trees, fig.height=4, fig.width=4------------------------------------
trees <- crown_metrics(las, func = .stdtreemetrics, geom = "point")
plot(trees["Z"], main = "Tree heights", pch = 16)

## ----print-inventory, message=FALSE, warning=FALSE----------------------------
library(dplyr)
inventory <- trees %>%
  mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)
print(inventory)

## ----read-inv, fig.height= 4, fig.width=7-------------------------------------
inv_trees <- read_inv(inventory, height = Z, height_unit = "m")
targets_buff <- define_target(inv_trees, target_source = "buff_edge", radius = 10)
plot_target(targets_buff)

## ----calc-CI------------------------------------------------------------------
CI <- compete_inv(inv_source = inv_trees, target_source = "buff_edge", radius = 10, method = "all")
CI

## ----plot-CI, fig.height= 5, fig.width=6--------------------------------------
library(ggplot2)
ggplot(CI, aes(x = x, y = y, color = CI_Braathe)) +
  geom_point(size = 2, alpha = 0.7) + # Adjust point size and transparency
  scale_color_gradient(low = "yellow", high = "darkred", name = "CI Braathe") + # Customizing color scale
  theme_classic() + # Change the theme
  labs(title = "Competition based on tree heights and distance", x = "X", y = "Y") + # Add title and axis labels
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
    legend.position = "right" # Position of the legend
  )

## ----itclidar, echo = TRUE, eval=FALSE----------------------------------------
#  library(itcSegment)
#  
#  
#  # create a lookup table with common height-crown diameter-relations (example from itcSegment)
#  lut <- data.frame(
#    H = c(2, 10, 15, 20, 25, 30),
#    CD = c(0.5, 1, 2, 3, 4, 5))
#  
#  #create a digital terrain model
#  dtm <- grid_terrain(las = las, res = 0.5, algorithm = knnidw(k=10L, p=2))
#  plot(dtm)
#  #normalize the height of the las data by terrain
#  nlas <- las - dtm
#  
#  # segment the trees (adjust epsg according to your coordinate reference system)
#  se<-itcLiDARallo(nlas$X,nlas$Y,nlas$Z,epsg=32632,lut=lut)
#  summary(se)
#  plot(se,axes=T)
#  
#  #validate the output in TreeCompR
#  inv_trees <- read_inv(se, height = Height_m, height_unit = "m")
#  #quantify tree competition (adjust radius)
#  compete_inv(inv_source = inv_trees, target_source = "buff_edge", radius = 13.5, method = "all")

