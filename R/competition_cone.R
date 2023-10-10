#' Quantify Tree Crown Competition (Cone Method or cylinder method)
#' @description opening angle 60°, spanned in 50 % of target tree's height
#' @param forest_path character path to file of neighborhood point cloud (including the target tree and neighbors, not height normalized, can include ground points)
#' @param tree_path character path to file of target tree point cloud
#' @param comp_method character string with competition method "cone" or "cylinder"
#' @param cyl_r (Optional) only important when using comp_method "cylinder"; numeric value of cylinder radius in m. Default is 4 m.
#'
#' @return vector of counted points and voxels of neighborhood point cloud that reach into the cone spanned over the target tree
#' @importFrom magrittr %>%
#' @importFrom utils data
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom tools file_path_sans_ext
#' @importFrom VoxR vox
#' @export
#'
#' @examples
#' \dontrun{
#' #Quantify crown competition for a single tree within a plot using cone method
#' CI_cone <- competition_pc(forest_path = "path/to/forest_pc.txt", tree_path = "path/to/tree_pc.txt", comp_method = "cone")
#'
#' #Quantify competition for a single tree within a plot using cylinder method with radius 4 m
#' CI_cyl <- competition_pc(forest_path = "path/to/forest_pc.txt", tree_path = "path/to/tree_pc.txt", comp_method = "cylinder", cyl_r = 4)
#' }
competition_pc <- function(forest_path, tree_path, comp_method = c("cone", "cylinder"), cyl_r = 4){
  tree <- read_tree(tree_path)
  filename <- file_path_sans_ext(basename(tree_path))
  pos <- position(tree)
  h <- height(tree)
  cone_h <- 0.5 * h
  hood <- read_tree(forest_path)
  neighbor <- dplyr::anti_join(hood, tree, by = c("X", "Y", "Z"))


  if (nrow(hood) == nrow(neighbor)) {
    stop("Tree is not within this plot!", call. = FALSE)
  } else {

    if (comp_method == "cone") {

      points <- neighbor %>% dplyr::mutate(dist = sqrt((.data$X-pos[1])^2 + (.data$Y-pos[2])^2)) %>%
        dplyr::mutate(r_cone = abs(0.577 * (.data$Z-cone_h))) %>%
        dplyr::filter(.data$Z >= cone_h) %>%
        dplyr::filter(.data$dist <= .data$r_cone)
      points <- nrow(points)


      voxel <- neighbor %>% VoxR::vox(res = 0.1)
      colnames(voxel) <- c("X", "Y", "Z", "npts")
      voxel %>% dplyr::mutate(r_cone = abs(0.577 * (.data$Z-cone_h))) %>%
        dplyr::mutate(dist = sqrt((.data$X-pos[1])^2 + (.data$Y-pos[2])^2)) %>%
        dplyr::filter(.data$Z >= cone_h) %>%
        dplyr::filter(.data$dist <= .data$r_cone)
      voxel <- nrow(voxel)

    } else if (comp_method == "cylinder"){

      points <- neighbor %>% dplyr::mutate(dist = sqrt((.data$X-pos[1])^2 + (.data$Y-pos[2])^2)) %>% dplyr::filter(.data$dist <= cyl_r)
      points <- nrow(points)
      voxel <- neighbor %>% VoxR::vox(res = 0.1)
      colnames(voxel) <- c("X", "Y", "Z", "npts")
      voxel <- voxel %>% dplyr::mutate(dist = sqrt((.data$X-pos[1])^2 + (.data$Y-pos[2])^2)) %>% dplyr::filter(.data$dist <= .data$cyl_r)
      voxel <- nrow(voxel)
    } else {
      stop("Invalid method. Use 'cone' or 'cylinder'.")
    }

    result <- data.table::data.table(target = filename, CI_cone_pts = points, CI_cone_vox = voxel)
    return(result)


  }}
