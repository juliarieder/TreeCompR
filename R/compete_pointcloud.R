#' @title Quantify Tree Competition from Point Clouds (Cone or cylinder method)
#' @description Counts the voxels of neighboring trees that intersect a
#'   search cone or search cylinder around the target tree according to Seidel
#'   et al. (2015) and Metz et al. (2013).
#' @param forest_source data.frame with neighborhood point cloud or path to file
#'   of neighborhood point cloud in tabular or las/laz format which is passed on
#'   to [read_tree()]. The neighborhood has to include the target tree and its
#'   neighbors, not height normalized, and can include ground points).
#'   Coordinates have to be in metric system in m!
#' @param tree_source data.frame with target tree point cloud or path to file of
#'   target tree point cloud in tabular or las/laz format which is passed on to
#'   [read_tree()]. Coordinates have to be in metric system in m and same number
#'   of decimal places as the neighborhood point cloud
#' @param comp_method character string with competition method. Allowed values
#'   are "cone" for the cone method, "cylinder" for the cylinder method or
#'   "both" for both methods. See details for computation
#' @param cyl_r (optional) only needed when using comp_method "cylinder";
#'   numeric value of cylinder radius in m. Default is 5 m.
#' @param h_cone (optional) only when using comp_method "cone"; numeric value
#'   describing the fraction of the height of the tree where the tip of the
#'   search cone is located. For example, values of 0.5 or 0.6 specify that the
#'   cone opens in 50 or 60 % of target tree's height, respectively. Default is
#'   0.6 as proposed by Seidel et al. (2015).
#' @param z_min integer of length 1 describing the minimum number of points
#'   needed in the lowermost 0.1 m Z layer to consider it part of the target
#'   tree. Default is 100. Used to calculate the stem base position of the
#'   target tree. For details see [tree_pos()].
#' @param h_xy numeric of length 1 describing the height range in m over the
#'   stem base over which the x and y positions are used to calculate the x and
#'   y coordinates of the stem base of the target tree. Default is 0.3 m. Used
#'   to calculate the stem base position of the target tree. For details see
#'   [tree_pos()].
#' @param ... additional arguments passed on to [data.table::fread()]

#' @return data frame with tree ID and of log of counted voxels of neighborhood
#'   point cloud that reach into the cone/cylinder spanned over/around target
#'   tree.
#'
#' @details `compete_pc()` computes competition indices based on voxel counts of
#'   neighbor trees that intersect a search cone or search cylinder around
#'   the target tree.
#'   ## Cone Method
#'   Based on a search cone with an opening angle of 60 degrees,
#'   by default opening from a basal point situated at 60 % of the
#'   height of the target tree. The competition index is defined as the number
#'   of voxels (0.1 m res.) of neighboring trees situated within the cone
#'   spanned around the target tree (cf. Metz et al 2013; Seidel et al., 2015).
#'   The standard value of `h_cone = 0.6` can be adjusted, for instance if no
#'   neighbor trees at all intersect the cone of the target tree. However, be
#'   careful with adjusting this parameter, as competition indices computed with
#'   different `h_cone` cannot easily be compared among each other.
#'   ## Cylinder Method
#'   Based on a  search cylinder with a pre-defined radius `cyl_r` around the
#'   target tree (5 m by default). The competition index is defined as the
#'   number of the voxels (0.1 m res.) of neighboring trees situated within the
#'   cylinder around the target tree (cf. Seidel et al., 2015). The index is
#'   sensitive to the choice of the cylinder radius, so be careful when
#'   comparing competition indices computed with different values of `cyl_r`.
#'
#' @section Literature:
#' * Metz, J., Seidel, D., Schall, P., Scheffer, D., Schulze, E.-D. & Ammer,
#'   C. (2013). Crown modeling by terrestrial laser scanning
#'   as an approach to assess the effect of aboveground intra- and interspecific
#'   competition on tree growth. Forest Ecology and Management,310:275-288.
#'   https://doi.org/10.1016/j.foreco.2013.08.014
#' * Pretzsch, H., Biber, P. & Dursky, J. (2002). The single tree-based stand
#'   simulator SILVA: construction, application and evaluation. For. Ecol.
#'   Manage. 162, 3-21. https://doi.org/10.1016/S0378-1127(02)00047-6
#' * Seidel, D., Hoffmann, N., Ehbrecht, M., Juchheim, J. & Ammer, C. (2015).
#'   How neighborhood affects tree diameter increment - New insights from
#'   terrestrial laser scanning and some methodical considerations. Forest
#'   Ecology and Management, 336: 119-128.
#'   http://dx.doi.org/10.1016/j.foreco.2014.10.020
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Quantify crown competition for a single tree within a plot using cone
#' # method
#' CI_cone <- compete_pc("path/to/forest_pc.txt", "path/to/tree_pc.txt",
#'                           "cone", h_cone = 0.5)
#'
#' # Quantify competition for a single tree within a plot using cylinder
#' # method with radius 4 m
#' CI_cyl <- compete_pc("path/to/forest_pc.txt", "path/to/tree_pc.txt",
#' "cylinder", cyl_r = 4)
#' }
compete_pc <- function(forest_source, tree_source,
                       comp_method = c("cone", "cylinder"),
                       cyl_r = 5, h_cone = 0.6, z_min = 100, h_xy = 0.3,
                       ...){
  # stop execution if wrong method is specified
  if (!comp_method %in%  c("cone", "cylinder", "both"))
    stop("Invalid method. Use 'cone', 'cylinder' or 'both'.")

  # avoid errors with overriding global values
  x <- y <- z <- ID <- h <- dist <- r_cone <- NULL

  # read data for central tree
  tree <- read_tree(tree_source, ...)
  # get file name without extension
  filename <- tools::file_path_sans_ext(basename(tree_source))
  # get base position and height of central tree
  pos <- tree_pos(tree, z_min = z_min, h_xy = h_xy, include_height = TRUE)
  #  extract height of central tree
  h <- pos["height"]

  # read data for neighborhood
  hood <- read_tree(forest_source, ...)
  # remove points in the neighborhood that belong to the central tree
  neighbor <- dplyr::anti_join(hood, tree, by = c("x", "y", "z"))

  # prepare data.frame for results
  results <- data.frame(target = filename, height_target = h)

  # check if the tree is part of this neighborhood
  if (nrow(hood) == nrow(neighbor)) {
    stop("Tree is not within this plot! Do the input point clouds have the same number of decimal places?", call. = FALSE)
  } else {
    # voxelize neighborhood data
    voxel <- neighbor %>% VoxR::vox(res = 0.1)
    # compute competition indices for the cone method
    if (comp_method == "cone" | comp_method == "both") {
      # compute base height of the cone
      cone_h <- h_cone * h
      # filter voxels that are inside the cone
      voxel1 <- voxel %>%
        # remove voxels below critical height
        dplyr::filter(z - pos["z"] >= cone_h) %>%
        # compute cone radius (assuming an opening angle of pi / 3)
        dplyr::mutate(r_cone = abs(tan(pi / 6) * (z - pos["z"] - cone_h))) %>%
        # compute distance from tree position
        dplyr::mutate(dist = sqrt((x - pos["x"]) ^ 2 + (y - pos["y"]) ^ 2)) %>%
        # remove voxels outside of the cone
        dplyr::filter(dist <= r_cone)
      # get number of voxels inside the cone
      nvoxel_cone <- nrow(voxel1)
      # compute results
      results$CI_cone <- nvoxel_cone
      results$h_cone  <- h_cone
    }
    if (comp_method == "cylinder" | comp_method == "both"){
      # filter voxels that are inside the cylinder
      voxel2 <- voxel %>%
        # compute distance from central position
        dplyr::mutate(dist = sqrt((x - pos["x"]) ^ 2 + (y - pos["y"]) ^ 2)) %>%
        # remove voxels outside the cylinder
        dplyr::filter(dist <= cyl_r)
      # get number of voxels inside the cylinder
      nvoxel_cyl <- nrow(voxel2)
      # compute results
      results$CI_cyl <- nvoxel_cyl
      results$cyl_r <- cyl_r
    }
  }
  # remove row.names of results for proper printing
  row.names(results) <- NULL
  # set class of results
  class(results) <- c("compete_pc", class(results))
  # return results
  return(results)
}

# Define printing method for compete_pc objects:
#' @rdname compete_pc
#' @format NULL
#' @usage NULL
#' @export
print.compete_pc <- function(x, ...){
  # if people put together more than one object of class compete_pc, treat as
  # normal data.frame (just to avoid wrongly formatted output, workaround that
  # can be improved later)
  if(nrow(x) > 1)
    print(as.data.frame(x))
  # else print formatted output
  else{
    cat(" ------------------------------------------------------------------\n",
        "Pointcloud based competition index for",
        paste0("'", x$target, "'"), "\n",
        "------------------------------------------------------------------\n",
        "Target tree height:", x$height_target, "m\n",
        "------------------------------------------------------------------\n"
    )
    if ("CI_cone" %in% names(x)){
      cat(" Cone-based competition index using a cone base height of",
          round(x$h_cone * x$height_target, 2), "and\n",
          "an opening angle of 60 degrees:\n",
          "CI_cone = ", x$CI_cone, "\n",
          "------------------------------------------------------------------\n"
      )
    }
    if ("CI_cyl" %in% names(x)){
      cat(" Cylinder-based competition index using a cylinder radius of",
          round(x$cyl_r, 1), "\n around the target tree:\n",
          "CI_cyl = ", x$CI_cyl, "\n",
          "------------------------------------------------------------------\n"
      )
    }
  }
}
