#' Calculate Tree Competition Indices From Inventory Data
#'
#' @description
#' 'compete_inv()' returns a specific distance-dependent competition index (or group of indexes) for a list of target trees within a forest plot
#'
#' @param seg_path character path to inventory table (.csv or .txt) with structure: ID, X, Y, H
#' @param tree_path character path to table/list (.csv or .txt) of target trees within plot with ID_target, X, Y (does not have to be the same ID as in inventory table)
#' @param radius numeric, Search radius (in m) around target tree, wherein all neighboring trees are classified as competitors
#' @param method character string assigning the method for quantifying competition "Hegyi", "CI11", "CI12", "CI13" or "all"
#'
#' @return dataframe with ID_target, and one or more indices per tree
#' @export
#'
#' @examples
#' \dontrun{
#' CI <- compete_inv("path/to/invtable.csv", "path/to/target_trees.csv", radius = 10, method = "all")
#' }
compete_inv <- function(seg_path, tree_path, radius, method = c("all", "Hegyi","CI11", "CI12", "CI13")) {
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
                  CI11_part = (DBH / dbh_target) * atan(DBH / euc_dist_comp),
                  CI12_part = atan(H / euc_dist_comp),
                  CI13_part = (H / H_target) * atan(H / euc_dist_comp))
  #filter out the target tree(s) itself
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate competition index CI12 and CI13 for each target tree
  CIs <- trees_competition %>% dplyr::group_by(ID_target) %>% dplyr::summarise(
    CI_Hegyi = sum(CI_h_part),
    CI11 = sum(CI11_part),
    CI12 = sum(CI12_part),
    CI13 = sum(CI13_part))


if (method == "all") {
  return(CIs)
} else if (method == "Hegyi") {
  CI_Hegyi <- CIs %>% dplyr::select(ID_target, CI_Hegyi)
  return(CI_Hegyi)
} else if (method == "CI11") {
  CI_11 <- CIs %>% dplyr::select(ID_target, CI11)
  return(CI_11)
} else if (method == "CI12") {
  CI_12 <- CIs %>% dplyr::select(ID_target, CI12)
  return(CI_12)
} else if (method == "CI13") {
  CI_13 <- CIs %>% dplyr::select(ID_target, CI13)
  return(CI_13)
} else {
  stop("Invalid method. Supported methods: 'all', 'Hegyi', 'CI11', 'CI12', 'CI13'.")
}
}
