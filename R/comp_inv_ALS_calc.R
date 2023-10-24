#' QUantify Competition using inventory data from ALS
#' @description
#' 'Compete_ALS()' returns a specific distance-dependent competition index (or group of indexes) for a target tree within a forest plot
#'
#' @details
#' Using an inventory table to easily quantify distance-dependant tree competition for row of trees within a plot.
#' The input data can either be taken directly from field measurements or derived beforehand from LiDAR point clouds.
#' It calculates two Competition indices, based on tree heights and distance to competitors.
#'
#' @section Methods:
#'  * CI12 according to Rouvinen & Kuuluvainen (1997)
#'  * CI13 according to Rouvinen & Kuuluvainen (1997)
#'  ...
#'
#' @section Literature:
#'
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree crowns in relation to local competition in a natural mature Scot pine forest. Can. J. For. Res. 27, 890–902.
#'
#' @param seg_path character path to inventory table (.csv or .txt) with structure: ID, X, Y, H
#' @param tree_path character path to table/list (.csv or .txt) of target trees within plot with ID_target, X, Y (does not have to be the same ID as in inventory table)
#' @param radius numeric, Search radius (in m) around target tree, wherein all neighboring trees are classified as competitors
#'
#' @return dataframe with ID_target, CI12, CI13
#' @export
#'
#' @examples
#' \dontrun{
#' CI <- compete_ALS("path/to/invtable.csv", "path/to/target_trees.csv", radius = 10)
#' }
compete_ALS <- function(seg_path, tree_path, radius) {
  segtrees <- fread(seg_path, header = T)
  colnames(segtrees) <- c("ID", "X_seg", "Y_seg", "H")
  segtrees_sf <- sf::st_as_sf(segtrees, coords = c("X_seg", "Y_seg"))
  ttrees <- fread(tree_path, header = T)
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
  matching_rows <- base::subset(trees_competition, status == "target_tree")
  matching_rows <- matching_rows %>% dplyr::rename(ID_t = ID, H_target = H, X_segt = X_seg, Y_segt = Y_seg) %>% dplyr::select(ID_t, ID_target, H_target, X_segt, Y_segt)
  trees_competition <- dplyr::left_join(trees_competition, matching_rows, by = "ID_target") %>% mutate(euc_dist_comp = sqrt((X_segt - X_seg)^2 + (Y_segt - Y_seg)^2)) %>%
    dplyr::filter(euc_dist_comp <= radius)
  #calculate part of the Competition indices for each competitor
  trees_competition <- trees_competition %>% dplyr::mutate(CI12_part = atan(H / euc_dist_comp), CI13_part = (H / H_target) * atan(H / euc_dist_comp))
  #filter out the target tree itself
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate competition index CI12 and CI13 for each target tree
  trees_competition <- trees_competition %>% dplyr::group_by(ID_target) %>% dplyr::summarize(sum(CI12_part), sum(CI13_part))
  colnames(trees_competition) <- c("ID_target", "CI12_ALS", "CI13_ALS")

  return(trees_competition)
}



