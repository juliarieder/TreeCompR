#' @title Position and Height of a Tree from Point Cloud
#'
#' @description Compute the stem base position, metroid of the crown projected
#'   area and the stem height of a tree
#'
#' @param tree tree_pc or data.frame (x, y, z) of the tree point cloud.
#'   Coordinates have to be in metric system!
#' @param z_min integer of length 1 describing the the minimum number of points
#'   needed in the lowermost 1 voxel depth Z layer to consider it part of the
#'   tree. Default is 100. If changing the voxel resolution (res) from the
#'   default value of 0.1, different settings may be necessary.
#' @param h_xy numeric of length 1 describing the height range in m over the
#'   stem base over which the x and y positions are used to calculate the x and
#'   y coordinates of the stem base. Default is 0.3 m.
#' @param res res numeric of length 1 defining the resolution of a voxel as
#'   passed on to [VoxR::vox()]. Defaults to 0.1 (10 cm voxel size).
#' @param tree_name character vector of length 1 with the name of the tree as
#'   returned with the output object. Defaults to NULL (take name from `tree`
#'   argument).
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
#'   The *central point of the crown projected area* is computed as the median
#'   of the x and y position of the projected area of the tree.
#'
#'   The *height* is based on difference between the z value of the highest
#'   voxel belonging to the tree over and the z value identified for the stem
#'   base.
#'
#'   The calculation is done on voxels to ensure consistency with downstream
#'   analyses and to reduce bias caused by inhomogeneous point densities on
#'   different sides of the stem.
#'
#'   In our opinion, in most cases it makes more sense to calculate competition
#'   indices based on the central point of the tree crown than based on the
#'   stem base because for strongly inclined trees a cylinder constructed around
#'   its base may contain no voxels of the crown of the central tree at all.
#'
#' @return object of class "tree_pos" containing the following components:
#'  \itemize{
#'     \item{`base_pos`} {numeric vector of length 3 with the x, y, z
#'                        coordinates of the tree base position.}
#'     \item{`crown_pos`} {numeric vector of length 3 with the x, and y
#'                      coordinates of the central point of the crown projected
#'                      area and the z position of the stem base identified as
#'                      above.}
#'     \item{`height`} {numeric of length one containing the tree height in m.}
#'     \item{`cpa_m2`} {numeric of length one containing the crown projected area
#'                   in m².}
#'     \item{`cpa_xy`} {`data.table` with the x and y positions of the
#'                           crown.}
#'     \item{`tree_name`} {name of the object used as the "tree" argument, or
#'                         name specified by the user.}
#'   }
#'
#' @seealso [read_pc()] for details on reading point clouds,
#'  [compete_pc()] for computing tree competition from point cloud objects.
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
tree_pos <- function(tree, z_min = 100L, h_xy = 0.3, res = 0.1,
                     tree_name = NULL){
  # get name of object for output
  if (is.null(tree_name)) tree_name <- deparse(substitute(tree))
  if (length(tree_name) > 1)
    stop("tree_name has to be a character vector of length 1.")
  # ensure consistency of tree object
  tree <- .validate_pc(tree)
  # voxelize tree
  tree_v <- VoxR::vox(tree, res = res)
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
  cpa <- unique(tree_v[, c("x", "y")])
  # compute centroid of crown projected area
  crown_pos <- c(
    x = stats::median(cpa$x),
    y = stats::median(cpa$y),
    z = zpos)
  # compute height
  height <- max(tree_v$z - zpos)
  # prepare output
  output <- list(base_pos = base_pos, crown_pos = crown_pos, height = height,
                 cpa_m2 = sum(cpa) * res^2,
                 cpa_xy = cpa,
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
      "Tree base position:             (", paste(
        paste(c("x", "y", "z"), "=",
              round(x$base_pos, 1)), collapse = ", "),")\n",
      "Center of crown projected area: (", paste(
        paste(c("x", "y", "z"), "=",
              round(x$crown_pos, 1)), collapse = ", "),")\n",
      "Tree height:                   ", x$height, "m\n",
      "Crown projected area:          ", round(x$cpa_m2, 2), "m\n",
      "------------------------------------------------------------------\n"
  )
  # return object invisibly
  invisible(x)
}
