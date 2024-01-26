#' Quantify distance-height-dependent Competition using inventory data (from ALS)
#' @description
#' 'compete_dh()' returns a specific distance-height-dependent competition index (or group of indexes) for a list of target trees within a forest plot
#'
#' @details
#' Using an inventory table to easily quantify distance-dependent tree competition for a list of trees within a plot.
#' The input data can either be taken directly from field measurements or derived beforehand from LiDAR point clouds.
#' The function calculates 3 Competition indices, based on tree heights and distance to competitors.
#'
#' @section Methods:
#'  * CI_Braathe according to Braathe (1980)
#'    \eqn{\sum_{i=1}^{n} h_{i} / (h * dist_{i})}
#'  * CI_RK3 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} arctan(h_{i} / dist_{i})}
#'  * CI_RK4 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} (h_{i} / h) * arctan(h_{i} / dist_{i})}
#'
#'
#'
#' Tree Segmentation:
#'
#' Various approaches can be used to segment airborne laser scanning point clouds into single trees and to obtain inventory data based it. Existing R packages for this are for example:
#' * lidR package with different options to segment the point cloud or a Canopy Height Model (CHM)
#' * itcLiDARallo within the package itcSegment
#'
#' Be careful with low resolution/low density point clouds, as oversegmentation of trees is usually an issue!
#'
#' Literature:
#'
#'  * Braathe, P., 1980. Height increment of young single trees in relation to height and distance of neighboring trees. Mitt. Forstl. VersAnst. 130, 43–48.
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree crowns in relation to local competition in a natural mature Scot pine forest. Can. J. For. Res. 27, 890–902.
#'  * Contreras, M.A., Affleck, D. & Chung, W., 2011. Evaluating tree competition indices as predictors of basal area increment in western Montana forests. Forest Ecology and Management, 262(11): 1939-1949.
#'
#' @param plot_path character path to inventory table (.csv or .txt) of plot, with structure: ID, X, Y, H (in m). Coordinates have to be in metric system (in m)!
#' @param ttrees_path character path to table/list (.csv or .txt) of target trees within plot with ID_target, X, Y (does not have to be the same ID as in inventory table). Coordinates have to be in metric system!
#' @param radius numeric, Search radius (in m) around target tree, wherein all neighboring trees are classified as competitors
#' @param tolerance numeric. Tolerance for the match with the tree coordinates. If a field measurement value is used for target_tree, take a higher tolerance value (default=1 m), depending on the measurement accuracy
#'
#'
#' @return dataframe with ID_target, and one or more Indices depending on chosen method
#' @export
#'
#' @examples
#' \dontrun{
#' CI <- compete_ALS("path/to/invtable.csv", "path/to/target_trees.csv", radius = 10)
#' }
compete_dh <- function(plot_path, ttrees_path, radius, method = c("all", "CI_Braathe", "CI_RK3", "CI_RK4"), tolerance = 1) {
  segtrees <- fread(plot_path, header = T)
  colnames(segtrees) <- c("ID", "X_seg", "Y_seg", "H")
  segtrees_sf <- sf::st_as_sf(segtrees, coords = c("X_seg", "Y_seg"))
  ttrees <- fread(ttrees_path, header = T)
  colnames(ttrees) <- c("ID_target", "X", "Y")
  ttrees_sf <- sf::st_as_sf(ttrees, coords = c("X", "Y"))
  buffer <- sf::st_buffer(ttrees_sf, dist = (radius + 5),
                          nQuadSegs = 30)
  sf::st_agr(segtrees_sf) = "constant"
  sf::st_agr(buffer) = "constant"
  trees_competition <- sf::st_intersection(segtrees_sf, buffer)
  trees_competition1 <- sf::st_drop_geometry(trees_competition)
  trees_competition <- dplyr::left_join(trees_competition1, segtrees, by = c("ID","H"))
  trees_competition <- dplyr::left_join(trees_competition, ttrees, by = "ID_target")
  #calculate euclidean distance between competitor and target tree
  trees_competition <- trees_competition %>% mutate(euc_dist = sqrt((X - X_seg)^2 + (Y - Y_seg)^2)) %>%
    dplyr::mutate(status = ifelse(euc_dist == min(euc_dist), "target_tree", ifelse(euc_dist > min(euc_dist), "competitor", NA)))
  if (min(trees_competition$euc_dist) > tolerance) {
    stop("There was no tree found within the tolerance threshold. Check the coordinates again or, if the accuracy of GPS signal was low, set a new tolerance value.")
  } else {
    matching_rows <- subset(trees_competition, status == "target_tree")
  }
  matching_rows <- matching_rows %>% dplyr::rename(ID_t = ID, H_target = H, X_segt = X_seg, Y_segt = Y_seg) %>% dplyr::select(ID_t, ID_target, H_target, X_segt, Y_segt)
  trees_competition <- dplyr::left_join(trees_competition, matching_rows, by = "ID_target") %>% dplyr::mutate(euc_dist_comp = sqrt((X_segt - X_seg)^2 + (Y_segt - Y_seg)^2)) %>%
    dplyr::filter(euc_dist_comp <= radius)
  #calculate part of the Competition indices for each competitor
  trees_competition <- trees_competition %>% dplyr::mutate(CI_hd1_part = (H/ (H_target * euc_dist_comp)),
                                                           CI_hd2_part = atan(H / euc_dist_comp),
                                                           CI_hd3_part = (H / H_target) * atan(H / euc_dist_comp))
  #filter out the target tree itself
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate competition indices (height-distance dependent) for each target tree
  CIs <- trees_competition %>% dplyr::group_by(ID_target) %>% dplyr::summarize(sum(CI_hd1_part), sum(CI_hd2_part), sum(CI_hd3_part))
  colnames(CIs) <- c("ID_target", "CI_Braathe", "CI_RK3", "CI_RK4")

  if (method == "all") {
    cat("Distance-height-based competition was quantified with methods by Braathe and Rouvinen and Kuuluvainen. Search radius =", radius, ".\n")
    print(CIs)
    return(CIs)
  } else if (method == "CI_Braathe") {
    CI_Braathe <- CIs %>% dplyr::select(ID_target, CI_Braathe)
    cat("Distance-height-based Competition was quantified using", method,". Search radius =", radius,".\n")
    print(CI_Braathe)
    return(CI_Braathe)
  } else if (method == "CI_RK1") {
    CI_RK3 <- CIs %>% dplyr::select(ID_target, CI_RK3)
    cat("Distance-height-based Competition was quantified using", method,". Search radius =", radius,".\n")
    print(CI_RK3)
    return(CI_RK3)
  } else if (method == "CI_RK2") {
    CI_RK4 <- CIs %>% dplyr::select(ID_target, CI_RK4)
    cat("Distance-height-based Competition was quantified using", method,". Search radius =", radius,".\n")
    print(CI_RK4)
    return(CI_RK4)
  } else {
    stop("Invalid method. Supported methods: 'all', 'CI_Braathe', 'CI_RK3', 'CI_RK4'.")
}}



