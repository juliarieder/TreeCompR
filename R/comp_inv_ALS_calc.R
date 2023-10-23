#noch weiter schreiben; ALS batch processing
#' QUantify Competition using inventory data from ALS
#' @description
#' 'compete_calc()' returns a specific distance-dependent competition index (or group of indexes) for a target tree within a forest plot
#'
#' @details
#' Using an inventory table to easily quantify distance-dependant tree competition for a single tree within a plot.
#' The input data can either be taken directly from field measurements or derived beforehand from LiDAR point clouds.
#' It is possible to choose between certain Competition indices, based on tree heights and distance to competitors
#'
#' @section Methods:
#'  * CI11 according to Rouvinen & Kuuluvainen (1997)
#'  * CI12 according to Rouvinen & Kuuluvainen (1997)
#'  * CI13 according to Rouvinen & Kuuluvainen (1997)
#'  ...
#'
#' @section Literature:
#'
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree crowns in relation to local competition in a natural mature Scot pine forest. Can. J. For. Res. 27, 890–902.
#'
#' @param path character string path to folder with 1 or more.csv file(s) with inventory data with structure (ID, X, Y, DBH, H), DBH and H in m. Each row indicates one tree within the plot
#' @param radius numeric, Search radius (in m) around target tree, wherein all neighboring trees are classified as competitors
#' @param dbh_thr numeric, DBH threshold for classifying the tree as a competitor (default is 0.1 m)
#' @param tolerance numeric. Tolerance for the match with the tree coordinates. If a field measurement value is used for target_tree, take a higher tolerance value (default=0.1 m), depending on the measurement accuracy
#' @param target_tree numeric (ID) or a vector of coordinates c(X, Y)
#'
#' @return numeric. Competition Index value
#' @export
#'
#' @examples
#' #......
compete_ALS_calc <- function(path, radius = 10, dbh_thr = 0.1, target_tree, tolerance = 2) {
  trees <- data.table::fread(path)
  trees <- data.frame(trees[, 1:5])
  target_tree <- as.numeric(target_tree)
  colnames(trees) <- c("ID", "X", "Y", "H")

  CIs <- NULL  # Initialize CIs

  if (type == "ID") {
    matching_rows <- subset(trees, ID == target_tree)
    if (nrow(matching_rows) == 0) {
      stop("This Tree ID is not existing within this plot!")
    } else if (nrow(matching_rows) > 1) {
      stop("Warning: there are more than 1 Trees with this ID, please check!")
    } else if (nrow(matching_rows) == 1) {
      trees <- trees %>% dplyr::mutate(status = ifelse(ID == target_tree, "target_tree", "competitor"))
    }
  } else if (type == "coordinates") {
    X_pos = target_tree[1]
    Y_pos = target_tree[2]
    trees <- trees %>%
      dplyr::mutate(euc_dist = sqrt((X_pos - X)^2 + (Y_pos - Y)^2)) %>%
      dplyr::mutate(status = ifelse(euc_dist == min(euc_dist), "target_tree", ifelse(euc_dist > min(euc_dist), "competitor", NA)))

    if (min(trees$euc_dist) > tolerance) {
      stop("There was no tree found within the tolerance threshold. Check the coordinates again or, if the accuracy of field data was low, set a new tolerance value.")
    } else {
      matching_rows <- subset(trees, status == "target_tree")
    }
  } else {
    stop("This input format is not supported, please enter an existing Tree ID or the coordinates of the target tree.")
  }

  if (!is.null(matching_rows)) {
    H_target <- matching_rows$H
    dbh_target <- matching_rows$DBH
    X_target <- matching_rows$X
    Y_target <- matching_rows$Y
    target_ID <- matching_rows$ID

    trees <- trees %>%
      dplyr::mutate(
        H_target = H_target,
        dbh_target = dbh_target,
        X_target = X_target,
        Y_target = Y_target,
        target_ID = target_ID
      )

    CIs <- trees %>%
      dplyr::mutate(euc_dist_comp = sqrt((X_target - X)^2 + (Y_target - Y)^2)) %>%
      dplyr::filter(euc_dist_comp <= radius) %>%
      dplyr::mutate(CI12_part = atan(H / euc_dist_comp), CI13_part = (H / H_target) * atan(H / euc_dist_comp)) %>%
      dplyr::filter(status == "competitor")

    CIs <- CIs %>% dplyr::group_by(target_ID) %>% dplyr::summarise(
      CI12 = sum(CI12_part),
      CI13 = sum(CI13_part))
  }

  return(CIs)

}



