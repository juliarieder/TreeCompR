#' Quantify Tree Competition from Point Clouds (Cone or cylinder method)
#' @description Counting the points or voxels of neighboring trees that reach into search cone or cylinder for the target tree
#' @param forest_path character path to file of neighborhood point cloud (including the target tree and neighbors, not height normalized, can include ground points)
#' @param tree_path character path to file of target tree point cloud
#' @param comp_method character string with competition method "cone" or "cylinder"
#' @param cyl_r (Optional) only important when using comp_method "cylinder"; numeric value of cylinder radius in m. Default is 4 m.
#'
#' @return vector of counted points and voxels of neighborhood point cloud that reach into the cone spanned over the target tree
#'
#' @details
#' * Cone Method: search cone with opening angle 60°, spanned in 50 % of target tree's height, counting the points and voxels of neighboring trees that reach within
#' * Cylinder Method: search cylinder with specific radius (target tree in center), counting the points and voxels of neighboring trees that reach within
#'
#' Check the Literature:
#' * Metz, J., Seidel, D., Schall, P., Scheffer, D., Schulze, E.-D. & Ammer, C. (2013). Crown modeling by terrestrial laser scanning
#' as an approach to assess the effect of aboveground intra- and interspecific competition on tree growth. Forest Ecology and Management,
#' 310: 275-288. https://doi.org/10.1016/j.foreco.2013.08.014
#' * Seidel, D., Hoffmann, N., Ehbrecht, M., Juchheim, J. & Ammer, C. (2015). How neighborhood affects tree diameter increment – New insights from
#' terrestrial laser scanning and some methodical considerations. Forest Ecology and Management, 336: 119-128. http://dx.doi.org/10.1016/j.foreco.2014.10.020
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Quantify crown competition for a single tree within a plot using cone method
#' CI_cone <- competition_pc("path/to/forest_pc.txt", "path/to/tree_pc.txt", "cone")
#'
#' #Quantify competition for a single tree within a plot using cylinder method with radius 4 m
#' CI_cyl <- competition_pc("path/to/forest_pc.txt", "path/to/tree_pc.txt", "cylinder", cyl_r = 4)
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

      points <- neighbor %>% dplyr::mutate(dist = sqrt((X-pos[1])^2 + (Y-pos[2])^2)) %>%
        dplyr::mutate(r_cone = abs(0.577 * (Z-cone_h))) %>%
        dplyr::filter(Z >= cone_h) %>%
        dplyr::filter(dist <= r_cone)
      points <- nrow(points)


      voxel <- neighbor %>% VoxR::vox(res = 0.1)
      colnames(voxel) <- c("X", "Y", "Z", "npts")
      voxel %>% dplyr::mutate(r_cone = abs(0.577 * (Z-cone_h))) %>%
        dplyr::mutate(dist = sqrt((X-pos[1])^2 + (Y-pos[2])^2)) %>%
        dplyr::filter(Z >= cone_h) %>%
        dplyr::filter(dist <= r_cone)
      voxel <- nrow(voxel)

    } else if (comp_method == "cylinder"){

      points <- neighbor %>% dplyr::mutate(dist = sqrt((X-pos[1])^2 + (Y-pos[2])^2)) %>% dplyr::filter(dist <= cyl_r)
      points <- nrow(points)
      voxel <- neighbor %>% VoxR::vox(res = 0.1)
      colnames(voxel) <- c("X", "Y", "Z", "npts")
      voxel <- voxel %>% dplyr::mutate(dist = sqrt((X-pos[1])^2 + (Y-pos[2])^2)) %>% dplyr::filter(dist <= cyl_r)
      voxel <- nrow(voxel)
    } else {
      stop("Invalid method. Use 'cone' or 'cylinder'.")
    }

    result <- data.table::data.table(target = filename, CI_cone_pts = points, CI_cone_vox = voxel)
    return(result)


  }}
