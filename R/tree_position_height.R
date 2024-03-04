#' @title Position and Height of a Tree from Point Cloud
#'
#' @param tree tree_pc or data.frame (x, y, z) of the tree point cloud.
#'   Coordinates have to be in metric system!
#' @param z_min integer of length 1 describing the the minimum number of points
#'   needed in the lowermost 0.1 m Z layer to consider it part of the tree.
#'   Default is 100.
#' @param h_xy numeric of length 1 describing the height range in m over the
#'   stem base over which the x and y positions are used to calculate the x and
#'   y coordinates of the stem base. Default is 0.3 m.
#' @param include_height logical - should the height of the tree be computed as
#'   well? Defaults to FALSE.
#'
#' @details Calculates the stem base position of a tree from a data.frame with
#'   a point cloud as created with [read_tree()]. The z position is taken as the
#'   z position of the first voxel layer containing at least z_min points of the
#'   point cloud (the standard value is 100). This is done to avoid outlying
#'   values affecting the calculated stem base height and should usually be
#'   consistent with taking the median z value of the lowermost 100 points.
#'
#'   The x and y position of the stem base are calculated as the median of the
#'   voxelized x and y coordinates of all voxels in the first h_xy m over the
#'   stem base (on default: first 0.3 m - values that are too low should be
#'   avoided here as the stem shape may be irregular close to the ground).
#'
#'   If a height is calculated, it is based on the position of the heighest
#'   voxel of the tree over the stem base.
#'
#'   The calculation is done on voxels to ensure consistency with downstream
#'   analyses and to reduce bias caused by inhomogeneous scanning coverage on
#'   different sides of the stem.
#'
#' @return named numeric vector of length 3 with the x, y, z coordinates of the
#'   tree's position, or named numeric vector of length 4 with the x, y, and z
#'   coordinates and the tree height.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' # Get the position of this tree (x, y, z)
#' pos <- tree_pos(tree)
#' # get the position and height
#' pos <- tree_pos(tree, height  = TRUE)
#' }
tree_pos <- function(tree, z_min = 100L, h_xy = 0.3, include_height = FALSE){
  # ensure consistency of tree object
  tree <- .validate_tree(tree)
  # get number of points in voxelized height layers
  ztab <- Rfast::Table(Rfast::Round(tree$z, 1))
  # get first height where there are at least z_min points
  zpos <- as.numeric(names(ztab)[which(ztab >= z_min)[1]])
  # voxelize tree
  tree_v <- VoxR::vox(tree, res = 0.1)
  # get voxels in the desired stem base range
  stem_base <- tree_v[tree_v$z - zpos < h_xy, ]
  # compute output
  output <- c(
    x = stats::median(stem_base$x),
    y = stats::median(stem_base$y),
    z = zpos)
  # append height if desired
  if(include_height) output["height"] <- max(tree_v$z - zpos)
  # return position
  return(output)
}
