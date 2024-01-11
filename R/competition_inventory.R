#' Calculate Tree Competition Indices From Inventory Data
#'
#' @description
#' 'compete_dd()' returns a specific distance-dependent competition index (or group of indexes) for a target tree within a forest plot
#'
#' @details
#' Using an inventory table to easily quantify distance-dependant tree competition for a single tree within a plot.
#' The input data can either be taken directly from field measurements or derived beforehand from LiDAR point clouds.
#' It is possible to choose between certain Competition indices, as e.g. the Hegyi index (method = "Hegyi") according to Hegyi (1974).
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
#' @param path character string path to .csv file with inventory data with structure (ID, X, Y, DBH, H), DBH and H in m. Coordinates have to be in metric system! Each row indicates one tree within the plot.
#' @param radius numeric, Search radius (in m) around target tree, wherein all neighboring trees are classified as competitors
#' @param dbh_thr numeric, DBH threshold for classifying the tree as a competitor (default is 0.1 m)
#' @param target_tree numeric (ID) or a vector of coordinates c(X, Y)
#' @param type character string specifying the type of input of target_tree "ID" or "coordinates".
#' @param tolerance numeric. Tolerance for the match with the tree coordinates. If a field measurement value is used for target_tree, take a higher tolerance value (default=1 m), depending on the measurement accuracy
#'
#' @param method character string assigning the method for quantifying competition "Hegyi", "CI_dd2", "CI_dd3", "CI_hd1", "CI_hd2", "CI_hd3" or "all"
#'
#' @return numeric. Competition Index value
#'
#' @seealso [competition_pc()] to quantify competition directly from point clouds, or [compete_dh()] if you do not have DBH data
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate some distance-dependent CIs for one tree inside forest plot
#' # input coordinates target tree
#' ttree <- c(15, 9)
#' CI <- compete_dd("path/to/invtable.csv", dbh_thr = 0.1, ttree, "coordinates", 1, method = "all")
#'
#' # Calculate the Hegyi-Index for one tree inside a forest plot, giving the ID of the target tree
#' ID_tree <- 5
#' CI <- compete_dd("path/to/invtable.csv", dbh_thr = 0.1, ID_tree, "ID", 1, method = "Hegyi")
#' }
compete_dd <- function(path, radius = 10, dbh_thr = 0.1, target_tree, type = c("ID", "coordinates"), tolerance = 1, method = c("all", "Hegyi", "CI_dd2", "CI_dd3", "CI_hd1", "CI_hd2", "CI_hd3")) {

    trees <- data.table::fread(path)
    trees <- data.frame(trees[, 1:5])
    target_tree <- as.numeric(target_tree)
    colnames(trees) <- c("ID", "X", "Y", "DBH", "H")

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
        dplyr::mutate(CI_h_part = DBH / (dbh_target * euc_dist_comp),
                      CI_dd2_part = atan(DBH / euc_dist_comp),
                      CI_dd3_part = (DBH / dbh_target) * atan(DBH / euc_dist_comp),
                      CI_hd1_part = (H / (H_target * euc_dist_comp)),
                      CI_hd2_part = atan(H / euc_dist_comp),
                      CI_hd3_part = (H / H_target) * atan(H / euc_dist_comp)) %>%
        dplyr::filter(status == "competitor")

      CIs <- CIs %>% dplyr::group_by(target_ID) %>% dplyr::summarise(
        CI_Hegyi = sum(CI_h_part),
        CI_dd2 = sum(CI_dd2_part),
        CI_dd3 = sum(CI_dd3_part),
        CI_hd1 = sum(CI_hd1_part),
        CI_hd2 = sum(CI_hd2_part),
        CI_hd3 = sum(CI_hd3_part))
    }


    if (method == "all") {
      return(CIs)
    } else if (method == "Hegyi") {
      CI_Hegyi <- CIs %>% dplyr::select(target_ID, CI_Hegyi)
      return(CI_Hegyi)
    } else if (method == "CI_dd2") {
      CI_dd2 <- CIs %>% dplyr::select(target_ID, CI_dd2)
      return(CI_dd2)
    } else if (method == "CI_dd3") {
      CI_dd3 <- CIs %>% dplyr::select(target_ID, CI_dd3)
      return(CI_dd3)
    } else if (method == "CI_hd1") {
      CI_hd1 <- CIs %>% dplyr::select(target_ID, CI_hd1)
      return(CI_hd1)
    } else if (method == "CI_hd2") {
      CI_hd2 <- CIs %>% dplyr::select(target_ID, CI_hd2)
      return(CI_hd2)
    } else if (method == "CI_hd3") {
      CI_hd3 <- CIs %>% dplyr::select(target_ID, CI_hd3)
      return(CI_hd3)
    } else {
      stop("Invalid method. Supported methods: 'all', 'Hegyi', 'CI_dd2', 'CI_dd3', 'CI_hd1', 'CI_hd2', 'CI_hd3'.")
    }
}
