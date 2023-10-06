#' Quantify Tree Crown Competition (Cone Method or cylinder method)
#'
#' @param forest_path character path to file of neighborhood point cloud (including the target tree and neighbors, not height normalized, can include ground points)
#' @param tree_path character path to file of target tree point cloud
#' @param comp_method character string with competition method "cone" or "cylinder"
#' @param cylinder_r (Optional) only important when using comp_method "cylinder"; numeric value of cylinder radius in m. Default is 4 m.
#'
#' @return vector of counted points and voxels of neighborhood point cloud that reach into the cone spanned over the target tree
#' @export
#'
#' @examples
#' \dontrun{
#' #Quantify crown competition for a single tree within a plot using cone method, opening angle 60°, spanned in 50 % of target tree's height
#' Comp_cone <- competition_pc(forest_path = "path/to/forest_pc.txt", tree_path = "path/to/tree_pc.txt", comp_method = "cone")
#'
#' #Quantify competition for a single tree within a plot using cylinder method with a cylinder radius of 4 m
#' Comp_cyl <- competition_pc(forest_path = "path/to/forest_pc.txt", tree_path = "path/to/tree_pc.txt", comp_method = "cylinder", cylinder_r = 4)
#' }
competition_pc <- function(forest_path, tree_path, comp_method = c("cone", "cylinder"), cylinder_r = 4){
  tree <- read_tree(tree_path)
  pos <- position(tree)
  h <- height(tree)
  cone_h <- 0.5 * h
  hood <- read_tree(forest_path)
  neighbor <- dplyr::anti_join(hood, tree, by = c("X", "Y", "Z"))
  if (nrow(hood) == nrow(neighbor)) {
    stop("Tree is not within this plot!")
  } else {

  #match arguments
  method <- rlang::arg_match(comp_method)

  if (method == "cone") {

  neighbor <- neighbor %>% dplyr::mutate(r_cone = abs(0.577 * (Z-cone_h)),
                                  d = sqrt((X-pos$X)^2 + (Y-pos$Y)^2))
  points <- neighbor %>% dplyr::filter(z >= cone_h) %>%
    filter(d <= r_cone) %>% nrow()
  neighbor <- neighbor %>% VoxR::vox(res = 0.1) %>%
    mutate(r_cone = abs(0.577 * (Z-cone_h)),
           d = sqrt((X-pos$X)^2 + (Y-pos$Y)^2))
  voxel <- neighbor %>% dplyr::filter(z >= cone_h) %>%
    filter(d <= r_cone) %>% nrow()

  } else if (method == "cylinder"){

    points <- neighbor %>% dplyr::mutate(d = sqrt((X-pos$X)^2 + (Y-pos$Y)^2)) %>% filter(d <= cylinder_r) %>% nrow()
    voxel <- neighbor %>% VoxR::vox(res = 0.1) %>% dplyr::mutate(d = sqrt((X-pos$X)^2 + (Y-pos$Y)^2)) %>% filter(d <= cylinder_r) %>% nrow()
  } else {
    stop("Invalid method. Use 'cone' or 'cylinder'.")
  }

  return(c(points, voxel))

}}
