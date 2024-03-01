#' Quantify Tree Competition from Point Clouds (Cone or cylinder method)
#' @description Counting the points or voxels of neighboring trees that reach
#'   into search cone or cylinder for the target tree
#' @param forest_path character path to file of neighborhood point cloud in
#'   tabular or las/laz format which is passed on to [read_tree()]. The
#'   neighborhood has to include the target tree and its neighbors, not height
#'   normalized, and can include ground points). Coordinates have to be in
#'   metric system in m!
#' @param tree_path character path to file of target tree point cloud in tabular
#'   or las/laz format which is passed on to [read_tree()]. Coordinates have to
#'   be in metric system in m and same number of decimal places as the
#'   neighborhood point cloud
#' @param comp_method character string with competition method. Allowed values
#'   are "cone", "cylinder" or "both" for both methods. See details for
#'   computation.
#' @param cyl_r (optional) only needed when using comp_method "cylinder";
#'   numeric value of cylinder radius in m. Default is 5 m.
#' @param h_cone (optional) only when using comp_method "cone"; numeric value
#'   0.5 or 0.6 --> cone opens in 50 or 60 % of target tree's height
#' @param ... additional arguments passed on to [data.table::fread()]

#' @return data frame with tree ID and of log of counted voxels of neighborhood
#'   point cloud that reach into the cone/cylinder spanned over/around target
#'   tree.
#'
#' @details
#' * Cone Method: Based on a search cone with an opening angle of 60 degrees,
#'   by default opening from a basal point situated at 50 % (or 60 %) of the
#'   height of the target tree. The competition index is number of voxels (
#'   0.1 m res.) of neighboring trees situated within the cone spanned around
#'   the target tree.
#' * Cylinder Method: Based on a  search cylinder with a pre-defined radius
#'   cyl_r around the target tree (5 m by default). The competition index is the
#'   number of the voxels (0.1 m res.) of neighboring trees situated within the
#'   cylinder around the target tree.
#'
#' Check the Literature:
#' * Metz, J., Seidel, D., Schall, P., Scheffer, D., Schulze, E.-D. & Ammer,
#'   C. (2013). Crown modeling by terrestrial laser scanning
#' as an approach to assess the effect of aboveground intra- and interspecific
#'    competition on tree growth. Forest Ecology and Management,310:275-288.
#'    https://doi.org/10.1016/j.foreco.2013.08.014
#' * Pretzsch, H., Biber, P. & Dursky, J. (2002). The single tree-based stand
#'   simulator SILVA: construction, application and evaluation. For. Ecol.
#'   Manage. 162, 3-21. https://doi.org/10.1016/S0378-1127(02)00047-6
#' * Seidel, D., Hoffmann, N., Ehbrecht, M., Juchheim, J. & Ammer, C. (2015).
#'   How neighborhood affects tree diameter increment - New insights from
#' terrestrial laser scanning and some methodical considerations. Forest
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
compete_pc <- function(forest_path, tree_path,
                       comp_method = c("cone", "cylinder"),
                       cyl_r = 5, h_cone = 0.5, no_tree_value = NA_real_,
                       ...){
  # stop execution if wrong method is specified
  if (!comp_method %in%  c("cone", "cylinder", "both"))
    stop("Invalid method. Use 'cone', 'cylinder' or 'both'.")

  # avoid errors with overriding global values
  X <- Y <- Z <- ID <- H <- dist <- r_cone <- NULL

  # read data for central tree
  tree <- read_tree(tree_path, ...)
  # get file name without extension
  filename <- file_path_sans_ext(basename(tree_path))
  # get base position of central tree
  pos <- position(tree)
  #  get height of central tree
  h <- height(tree)

  # read data for neighborhood
  hood <- read_tree(forest_path, ...)
  # remove points in the neighborhood that belong to the central tree
  neighbor <- dplyr::anti_join(hood, tree, by = c("X", "Y", "Z"))

  # prepare data.frame for results
  results <- data.frame(target = filename, height_target = h)

  # check if the tree is part of this neighborhood
  if (nrow(hood) == nrow(neighbor)) {
    stop("Tree is not within this plot! Do the input point clouds have the same number of decimal places?", call. = FALSE)
  } else {
    # voxelize neighborhood data
    voxel <- neighbor %>% VoxR::vox(res = 0.1)
    # get correct column names
    colnames(voxel) <- c("X", "Y", "Z", "npts")

    # compute competition indices for the cone method
    if (comp_method == "cone" | comp_method == "both") {
      # compute base height of the cone
      cone_h <- h_cone * h
      # filter voxels that are inside the cone
      voxel1 <- voxel %>%
        # remove voxels below critical height
        dplyr::filter(Z >= cone_h) %>%
        # compute cone radius (assuming an opening angle of pi / 3)
        dplyr::mutate(r_cone = abs(tan(pi / 6) * (Z-cone_h))) %>%
        # compute distance from tree position
        dplyr::mutate(dist = sqrt((X-pos[1])^2 + (Y-pos[2])^2)) %>%
        # remove voxels outside of the cone
        dplyr::filter(dist <= r_cone)
      # get number of voxels inside the cone
      nvoxel_cone <- nrow(voxel1)
      # compute results
      results$CI_cone <- nvoxel_cone
      results$h_cone = h_cone
      # print informational message
      cat("Competition was quantified using the cone method with cone opening in", h_cone, "* target tree's height with 60 degree opening angle. \n")

    }
    if (comp_method == "cylinder" | comp_method == "both"){
      # filter voxels that are inside the cylinder
      voxel2 <- voxel %>%
        # compute distance from central position
        dplyr::mutate(dist = sqrt((X-pos[1])^2 + (Y-pos[2])^2)) %>%
        # remove voxels outside the cylinder
        dplyr::filter(dist <= cyl_r)
      # get number of voxels inside the cylinder
      nvoxel_cyl <- nrow(voxel2)
      # compute results
      results$CI_cyl <- nvoxel_cyl
      results$cyl_r <- cyl_r
      # print informational message
      cat("Competition was quantified using the cylinder method with radius", cyl_r, "m. \n")
    }
  }
  # print and return results
  print(results)
  return(results)
}
