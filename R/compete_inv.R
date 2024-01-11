#' Calculate Tree Competition Indices From Inventory Data
#'
#' @description
#' 'compete_inv()' returns a specific distance-dependent competition index (or group of indexes) for a list of target trees within a forest plot
#'
#' @section Methods:
#'  * Hegyi Index introduced by Hegyi (1974)
#'    \eqn{\sum_{i=1}^{n} d_{i} / (d * dist_{i})}
#'  * CI_dd2 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} arctan(d_{i} / dist_{i})}
#'  * CI_dd3 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} (d_{i} / d) * arctan(d_{i} / dist_{i})}
#'  * CI_dh1 according to Braathe (1980)
#'    \eqn{\sum_{i=1}^{n} h_{i} / (h * dist_{i})}
#'  * CI_hd2 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} arctan(h_{i} / dist_{i})}
#'  * CI_hd3 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} (h_{i} / h) * arctan(h_{i} / dist_{i})}
#'
#'
#' @section Literature:
#'
#'  * Braathe, P., 1980. Height increment of young single trees in relation to height and distance of neighboring trees. Mitt. Forstl. VersAnst. 130, 43–48.
#'  * Contreras, M.A., Affleck, D. & Chung, W., 2011. Evaluating tree competition indices as predictors of basal area increment in western Montana forests. Forest Ecology and Management, 262(11): 1939-1949.
#'  * Hegyi, F., 1974. A simulation model for managing jackpine stands. In: Fries, J. (Ed.), Proceedings of IUFRO meeting S4.01.04 on Growth models for tree and stand simulation, Royal College of Forestry, Stockholm.
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree crowns in relation to local competition in a natural mature Scot pine forest. Can. J. For. Res. 27, 890–902.
#'
#'
#' @param seg_path character path to inventory table (.csv or .txt) with structure: ID, X, Y, H (in m). Coordinates have to be in metric system!
#'
#' @param tree_path character path to table/list (.csv or .txt) of target trees within plot with ID_target, X, Y (does not have to be the same ID as in inventory table). Coordinates have to be in metric system!
#' @param radius numeric, Search radius (in m) around target tree, wherein all neighboring trees are classified as competitors
#' @param method character string assigning the method for quantifying competition "Hegyi", "CI_dd2", "CI_dd3", "CI_hd1", "CI_hd2", "CI_hd3" or "all"
#'
#' @return dataframe with ID_target, and one or more indices per tree, which depends on the chosen method
#' @export
#'
#' @examples
#' \dontrun{
#' CI <- compete_inv("path/to/invtable.csv", "path/to/target_trees.csv", radius = 10, method = "all")
#' }
compete_inv <- function(seg_path, tree_path, radius, method = c("all", "Hegyi", "CI_dd2", "CI_dd3", "CI_hd1", "CI_hd2", "CI_hd3")) {
  segtrees <- fread(seg_path, header = T)
  colnames(segtrees) <- c("ID", "X_seg", "Y_seg", "DBH", "H")
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
  trees_competition <- dplyr::left_join(trees_competition1, segtrees, by = c("ID", "DBH", "H"))
  trees_competition <- dplyr::left_join(trees_competition, ttrees, by = "ID_target")
  #calculate euclidean distance between competitor and target tree
  trees_competition <- trees_competition %>% mutate(euc_dist = sqrt((X - X_seg)^2 + (Y - Y_seg)^2)) %>%
    dplyr::mutate(status = ifelse(euc_dist == min(euc_dist), "target_tree", ifelse(euc_dist > min(euc_dist), "competitor", NA)))
  matching_rows <- base::subset(trees_competition, status == "target_tree")
  matching_rows <- matching_rows %>% dplyr::rename(ID_t = ID, dbh_target = DBH, H_target = H, X_segt = X_seg, Y_segt = Y_seg) %>% dplyr::select(ID_t, ID_target, dbh_target, H_target, X_segt, Y_segt)
  trees_competition <- dplyr::left_join(trees_competition, matching_rows, by = "ID_target") %>% mutate(euc_dist_comp = sqrt((X_segt - X_seg)^2 + (Y_segt - Y_seg)^2)) %>%
    dplyr::filter(euc_dist_comp <= radius)
  #calculate part of the Competition indices for each competitor
  trees_competition <- trees_competition %>%
    dplyr::mutate(CI_h_part = DBH / (dbh_target * euc_dist_comp),
                  CI_dd2_part = atan(DBH / euc_dist_comp),
                  CI_dd3_part = (DBH / dbh_target) * atan(DBH / euc_dist_comp),
                  CI_hd1_part = (H / (H_target * euc_dist_comp)),
                  CI_hd2_part = atan(H / euc_dist_comp),
                  CI_hd3_part = (H / H_target) * atan(H / euc_dist_comp))
  #filter out the target tree(s) itself
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate competition index CI12 and CI13 for each target tree
  CIs <- trees_competition %>% dplyr::group_by(ID_target) %>% dplyr::summarise(
    CI_Hegyi = sum(CI_h_part),
    CI_dd2 = sum(CI_dd2_part),
    CI_dd3 = sum(CI_dd3_part),
    CI_hd1 = sum(CI_hd1_part),
    CI_hd2 = sum(CI_hd2_part),
    CI_hd3 = sum(CI_hd3_part))


if (method == "all") {
  return(CIs)
} else if (method == "Hegyi") {
  CI_Hegyi <- CIs %>% dplyr::select(ID_target, CI_Hegyi)
  return(CI_Hegyi)
} else if (method == "CI_dd2") {
  CI_dd2 <- CIs %>% dplyr::select(ID_target, CI_dd2)
  return(CI_dd2)
} else if (method == "CI_dd3") {
  CI_dd3 <- CIs %>% dplyr::select(ID_target, CI_dd3)
  return(CI_dd3)
} else if (method == "CI_hd1") {
  CI_hd1 <- CIs %>% dplyr::select(ID_target, CI_hd1)
  return(CI_hd1)
} else if (method == "CI_hd2") {
  CI_hd2 <- CIs %>% dplyr::select(ID_target, CI_hd2)
  return(CI_hd2)
} else if (method == "CI_hd3") {
  CI_hd3 <- CIs %>% dplyr::select(ID_target, CI_hd3)
  return(CI_hd3)
} else {
  stop("Invalid method. Supported methods: 'all', 'Hegyi', 'CI_dd2', 'CI_dd3', 'CI_hd1', 'CI_hd2', 'CI_hd3'.")
}
}
