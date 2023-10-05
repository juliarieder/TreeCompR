#' Calculating distance-dependant Tree Competition Using Inventory Data (field or from LiDAR data)
#'
#' @param path character string path to .csv file with inventory data with structure (ID, X, Y, DBH, H)
#' @param radius numeric, Search radius around target tree, wherein all neighboring trees are classified as competitors
#' @param dbh_threshold numeric, DBH threshold for classifying the tree as a competitor (default is 0.1 m)
#' @param target_tree numeric (ID) or a vector of coordinates (X, Y)
#' @param type character string assigning the type of input of target_tree "ID" or "coordinates"
#' @param tolerance numeric. tolerance for matching the tree coordinates. If a field measured value is used for target_tree, take a higher tolerance value (default=0.1 m), depending on measurement accuracy
#'
#' @param method character string assigning the method for quantifying competition "Hegyi", "CI12", "CI13"
#'
#' @return numeric. Competition Index value
#' @export
#'
#' @examples
compete_calc <- function(path, radius = 10, dbh_threshold = 0.1, target_tree, type = c("ID", "coordinates"), tolerance = 0.1, method = c("all", "Hegyi", "CI12", "CI13")) {
  #match arguments
  #Meldung einbauen, falls invalid extension
  trees <- data.table::fread(path)
  trees <- data.frame(trees[, 1:3])
  colnames(trees) <- c("ID", "X", "Y", "DBH", "H")
  type_ttree <- rlang::arg_match(type)
  if (type_ttree == "ID") {
    #user provides the ID of the target tree
    #Look up the right coordinates
    matching_rows <- trees[ID == target_tree]
    if (nrow(matching_rows) == 0){
      stop("This Tree ID is not existing within this plot!")
    } else if(nrow(matching_rows) > 1) {
      stop("Warning: there are more than 1 Trees with this ID")
    } else if(nrow(matching_rows) == 1){
      trees <- trees %>% dplyr::mutate(status = ifelse(data$ID == target_tree, "target_tree", "competitor"))
    }
  else if(type_ttree == "coordinates"){
    #make sure that the coordinates are in the same coordinate system!
    X_pos = target_tree[1]
    Y_pos = target_tree[2]
    trees <- trees %>% dplyr::mutate(sqrt((X_pos - X)^2 + (Y_pos - Y)^2)) %>%
      dplyr::mutate(status = ifelse(euc_dist == min(euc_dist), "target_tree", ifelse(euc_dist > min(euc_dist),"competitor", NA)))
#hier funktion schreiben, die die angegebenen Koordinaten (ggf. aus Feld aufgenommen) mit dem Baum in Tabelle matched, der am nächsten liegt; !!!!!tolerance? bei Felddaten deutlich höher setzen!
    }
  }
  return(trees)
  trees <- trees %>% dplyr::filter(DBH >= dbh_threshold)
  tree <- trees %>% dplyr::filter(status == "target_tree") %>% mutate(H_target = H, dbh_target = DBH, X_target = X, Y_target = Y) %>% dplyr::select(H_target, dbh_target, X_target, Y_target)
  competition <- dplyr::full_join(trees, tree) %>% mutate(euc_dist_comp = sqrt((X_target - X)^2 + (Y_target - Y)^2)) %>% dplyr::filter(euc_dist_comp <= radius) %>%
    dplyr::mutate(CI_h_part = DBH / (dbh_target * euc_dist_comp)) %>%
    dplyr::mutate(CI12_part = atan(H / euc_dist_comp), CI13_part = (H / H_target) * atan(H / euc_dist_comp)) %>% dplyr::mutate(CI11_part = (DBH / dbh_target) * atan(DBH / euc_dist_comp)) %>%
    dplyr::filter(status == "competitor")
  CIs <- competition %>% dplyr::summarise(sum(CI_h_part), sum(CI11_part), sum(CI12_part), sum(CI13_part)) # Tree ID noch einbinden! oder anders vorgehen
  names(CIs) <- c('target_tree', 'CI_Hegyi', 'CI11', 'CI12', 'CI13')
  #Hegyi, C11, C12, C13...

  #provide different indices (all, or just some!)
  #Methoden gut beschreiben und Literatur einfügen
  #alle relevanten distance dependent competition indices einbinden
}

