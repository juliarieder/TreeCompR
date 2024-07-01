#' @title Position and Height of a Tree from Point Cloud
#'
#' @description Compute the stem base position, metroid of the crown projected
#'   area and the stem height of a tree
#'
#' @param tree tree_pc or data.frame (x, y, z) of the tree point cloud.
#'   Coordinates have to be in metric system!
#' @param z_min integer of length 1 describing the the minimum number of points
#'   needed in the lowermost 0.1 m Z layer to consider it part of the tree.
#'   Default is 100.
#' @param h_xy numeric of length 1 describing the height range in m over the
#'   stem base over which the x and y positions are used to calculate the x and
#'   y coordinates of the stem base. Default is 0.3 m.
#'
#' @details Calculates the stem base position, metroid of the crown projected
#'   area and height of a tree from a data.frame with  a point cloud as created
#'   with [read_pc()].
#'
#'   For the *stem base*, the z position is taken as the z position of the first
#'   voxel layer containing at least z_min points of the point cloud (the
#'   standard value is 100). This is done to avoid outlying values affecting the
#'   calculated stem base height and should usually be consistent with taking
#'   the median z value of the lowermost group of points, but with a lower
#'   sensitivity to outliers.
#'   The x and y position of the stem base are calculated as the median of the
#'   voxelized x and y coordinates of all voxels in the first h_xy m over the
#'   stem base (on default: first 0.3 m - values that are too low should be
#'   avoided here as the stem shape may be irregular close to the ground).
#'
#'   The *metroid of the crown projected area* is the median x and y position of
#'   all x and y positions attributed to the tree.
#'
#'   The *height* is based on difference between the z value of the highest
#'   voxel belonging to the tree over and the z value of the stem base.
#'
#'   The calculation is done on voxels to ensure consistency with downstream
#'   analyses and to reduce bias caused by inhomogeneous scanning coverage on
#'   different sides of the stem.
#'
#' @return object of class "tree_pos" containing the following components:
#'  \itemize{
#'     \item{base_pos}{numeric vector of length 3 with the x, y, z coordinates
#'                     of the tree base position.}
#'     \item{crown_pos}{numeric vector of length 3 with the x, y, z coordinates
#'                     of the position of the centroid of the tree crown.}
#'     \item{height}{numeric of length one containing the tree height in m.}
#'     \item{tree_name}{name of the object used as the "tree" argument.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' # Get the position of this tree (x, y, z)
#' pos <- tree_pos(tree)
#' pos
#' }
tree_pos <- function(tree, z_min = 100L, h_xy = 0.3){
  # get name of object for output
  tree_name <- deparse(substitute(tree))
  # ensure consistency of tree object
  tree <- .validate_pc(tree)
  # voxelize tree
  tree_v <- VoxR::vox(tree, res = 0.1)
  # get basal z position
  ztab <- tapply(tree_v$npts, tree_v$z, sum)
  # get first height where there are at least z_min points
  zpos <- as.numeric(names(ztab)[which(ztab >= z_min)[1]])
  # get voxels in the desired stem base range
  stem_base <- tree_v[tree_v$z - zpos < h_xy, ]
  # compute base position
  base_pos <- c(
    x = stats::median(stem_base$x),
    y = stats::median(stem_base$y),
    z = zpos)
  # get coordinates in projected area of the tree
  cpa <- unique(tree[, c("x", "y")])
  # compute centroid of crown projected area
  crown_pos <- c(
    x = stats::median(cpa$x),
    y = stats::median(cpa$y),
    z = zpos)
  # compute height
  height <- max(tree_v$z - zpos)
  # prepare output
  output <- list(base_pos = base_pos, crown_pos = crown_pos, height = height,
                 tree_name = tree_name)
  class(output) <- c("tree_pos", class(output))
  # return output
  return(output)
}


# Define printing method for tree_pos objects:
#' @rdname tree_pos
#' @format NULL
#' @usage NULL
#' @export
print.tree_pos <- function(x, ...){
  cat(" ------------------------------------------------------------------\n",
      "Computed tree position information for ",
      paste0("'", x$tree_name, "'"), "\n",
      "------------------------------------------------------------------\n",
      "Tree base position:              (", paste(
        paste(c("x", "y", "z"), "=",
              round(x$base_pos, 1)), collapse = ", "),")\n",
      "Metroid of crown projected area: (", paste(
        paste(c("x", "y", "z"), "=",
              round(x$crown_pos, 1)), collapse = ", "),")\n",
      "Tree height:                    ", x$height, "m\n",
      "------------------------------------------------------------------\n"
  )
  # return object invisibly
  invisible(x)
}
