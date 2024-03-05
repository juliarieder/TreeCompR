#' Calculate Tree Competition Indices Using Distance and DBH Inventory Data
#'
#' @description
#' 'compete_dd()' returns a specific distance-DBH-dependent competition index
#' (or group of indices) for a list of target trees within a forest plot
#'
#' @param plot_path character path to inventory table (.csv or .txt) of the
#'   plot, with structure: ID, X, Y, DBH (in m). Coordinates have to be in
#'   metric system (in m)!
#' @param ttrees_path character path to table/list (.csv or .txt) of target
#'   trees within plot with ID_target, X, Y (does not have to be the same ID as
#'   in inventory table). Coordinates have to be in metric system!
#' @param radius numeric, Search radius (in m) around target tree, wherein all
#' neighboring trees are classified as competitors
#' @param method character string assigning the method for quantifying
#' competition "CI_Hegyi", "CI_RK1", "CI_RK2" or "all"
#' @param tolerance numeric. Tolerance for the match with the tree coordinates.
#' If a field measurement value is used for target_tree, take a higher tolerance
#' value (default=1 m), depending on the measurement accuracy
#' @param dbh_thr numeric, DBH threshold for classifying the tree as a
#'  competitor (default is 0.1 m; trees with DBH smaller 0.1 m are no
#'  competitors)
#' @param dbh_max numeric, DBH threshold (max) in m, that is realistic for the
#' trees. (default: 1 m). It causes a warning message, if one or more tree
#' within your plot shows higher DBH values.
#'
#' @details
#' Using an inventory table to easily quantify distance-dependent tree
#' competition for a single tree within a plot.
#' The input data can either be taken directly from field measurements or
#' derived beforehand from LiDAR point clouds.
#' It is possible to choose between certain Competition indices, as e.g.
#' the Hegyi index (method = "Hegyi") according to Hegyi (1974).
#'
#' @section Methods:
#'  * CI_Hegyi Index introduced by Hegyi (1974)
#'    \eqn{\sum_{i=1}^{n} d_{i} / (d \cdot dist_{i})}
#'  * CI_RK1 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} \mathrm{arctan}(d_{i} / dist_{i})}
#'  * CI_RK2 according to Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} (d_{i} / d) \cdot \mathrm{arctan}(d_{i} / dist_{i})}
#'
#'
#' @section Literature:
#'  * Contreras, M.A., Affleck, D. & Chung, W., 2011. Evaluating tree
#'  competition indices as predictors of basal area increment in western Montana
#'  forests. Forest Ecology and Management, 262(11): 1939-1949.
#'  * Hegyi, F., 1974. A simulation model for managing jackpine stands. In:
#'  Fries, J. (Ed.), Proceedings of IUFRO meeting S4.01.04 on Growth models for
#'  tree and stand simulation, Royal College of Forestry, Stockholm.
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree
#'  crowns in relation to local competition in a natural mature Scot pine
#'  forest. Can. J. For. Res. 27, 890–902.
#'
#' @return dataframe with ID_target, and one or more indices per tree,
#' depending on chosen method
#'
#'
#' @seealso [compete_pc()] to quantify competition directly from point clouds,
#' or [compete_dh()] if you do not have DBH data
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # CI_Hegyi for trees inside forest plot
#' CI <- compete_dd("path/to/plot.csv", "path/to/ttrees.csv",
#'  radius = 10, method = "CI_Hegyi")
#' }
compete_dd <- function(plot_path, ttrees_path, radius,
                       method = c("all", "CI_Hegyi", "CI_RK1", "CI_RK2"),
                       dbh_thr = 0.1, tolerance = 1, dbh_max = 1.0) {

  X <- Y <- X_seg <- Y_seg <- CI_h_part <- CI_RK1_part <- CI_RK2_part <-
    euc_dist_comp <- DBH <- euc_dist <- ID_target <- ID_t <- DBH_target <-
    status <- ID <- dbh_target <- X_segt <- Y_segt <- NULL

  #read segmented trees (whole plot)
  segtrees <- fread(plot_path, header = T)
  colnames(segtrees) <- c("ID", "X_seg", "Y_seg", "DBH")
  #filter out trees that are too small to be considered as competitor (dbh_thr default 0.1 m)
  segtrees <- segtrees %>% dplyr::filter(DBH >= dbh_thr)
  # Identify rows with DBH higher than dbh_max to check if there was a problem
  # with automated segmentation (in case laser scanning data was used).
  # dbh_max default 1.0 m, should be adjusted depending on tree species and age
  high_dbh_rows <- segtrees[segtrees$DBH > dbh_max]

  # generate warning message, in case DBH of a tree within the plot is higher than
  # dbh_max (segtrees also includes the target trees)
  if (nrow(high_dbh_rows) > 0) {
    cat("Warning: The following trees have a diameter above", dbh_max,"m\n")
    colnames(high_dbh_rows) <- c("ID", "X", "Y", "DBH")
    print((high_dbh_rows[, c("ID", "X", "Y", "DBH")]))
  }
  # convert segmented trees to sf object
  segtrees_sf <- sf::st_as_sf(segtrees, coords = c("X_seg", "Y_seg"))
  #read in the target trees
  ttrees <- fread(ttrees_path, header = T)
  colnames(ttrees) <- c("ID_target", "X", "Y")
  #convert target trees to sf object
  ttrees_sf <- sf::st_as_sf(ttrees, coords = c("X", "Y"))
  #create buffer around target trees;

  # radius + 5, so no tree is cut off accidentally;
  # but just radius could also work?
  buffer <- sf::st_buffer(ttrees_sf, dist = (radius + 5),
                          nQuadSegs = 30)
  sf::st_agr(segtrees_sf) = "constant"
  sf::st_agr(buffer) = "constant"
  # filters possible competitors around target trees and
  #creates automatically an attribute (ID of target) for each
  #possible competitor
  trees_competition <- sf::st_intersection(segtrees_sf, buffer)
  # drop geometry to get a dataframe with competitors for each target tree
  trees_competition1 <- sf::st_drop_geometry(trees_competition)
  #get information on x and y back from input table
  trees_competition <- dplyr::left_join(trees_competition1,
                                        segtrees, by = c("ID", "DBH"))
  trees_competition <- dplyr::left_join(trees_competition,
                                        ttrees, by = "ID_target")
  #calculate euclidean distance between competitor and target tree
  #nearest neighbor within segmented trees for target tree input is set
  #as a target tree
  trees_competition <- trees_competition %>%
    dplyr::mutate(
      euc_dist = sqrt((X - X_seg)^2 + (Y - Y_seg)^2),
      status = ifelse(euc_dist == min(euc_dist), "target_tree",
                      ifelse(euc_dist > min(euc_dist), "competitor", NA)))
  # if there is no tree within segmented trees found for a target tree x,y
  # within a certain threshold (depending on GPS accuracy e.g.): stop/warning
  if (min(trees_competition$euc_dist) > tolerance) {
    stop("There was no tree found within the tolerance threshold. Check the coordinates again or, if the accuracy of GPS signal was low, set a new tolerance value.")
  } else {
    #filter all target trees and make additional columns with the target tree values
    matching_rows <- subset(trees_competition, status == "target_tree")
  }
  matching_rows <- matching_rows %>%
    dplyr::rename(ID_t = ID, dbh_target = DBH, X_segt = X_seg, Y_segt = Y_seg) %>%
    dplyr::select(ID_t, ID_target, dbh_target, X_segt, Y_segt)
  # add target tree information to each segmented tree that intersected with
  # target tree buffer
  trees_competition <- dplyr::left_join(trees_competition,
                                        matching_rows, by = "ID_target") %>%
    dplyr::mutate(euc_dist_comp = sqrt((X_segt - X_seg)^2 + (Y_segt - Y_seg)^2)) %>%
    dplyr::filter(euc_dist_comp <= radius) #filter all competitors within search radius
  #calculate part of the Competition indices for each competitor
  trees_competition <- trees_competition %>%
    dplyr::mutate(CI_h_part = DBH / (dbh_target * euc_dist_comp),
                  CI_RK1_part = atan(DBH / euc_dist_comp),
                  CI_RK2_part = (DBH / dbh_target) * atan(DBH / euc_dist_comp))
  #filter out the target tree(s)
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate competition index CI12 and CI13 for each target tree
  CIs <- trees_competition %>% dplyr::group_by(ID_target) %>% dplyr::summarise(
    CI_Hegyi = sum(CI_h_part),
    CI_RK1 = sum(CI_RK1_part),
    CI_RK2 = sum(CI_RK2_part))

 #output depending on method and searchradius
  if (method == "all") {
    cat("DBH-distance-based competition was quantified with methods by Hegyi and Rouvinen and Kuuluvainen. Search radius =", radius, ".\n")
    print(CIs)
    return(CIs)
  } else if (method == "CI_Hegyi") {
    CI_Hegyi <- CIs %>% dplyr::select(ID_target, CI_Hegyi)
    cat("Distance-DBH-based Competition was quantified using", method,".
        Search radius =", radius,".\n")
    print(CI_Hegyi)
    return(CI_Hegyi)
  } else if (method == "CI_RK1") {
    CI_RK1 <- CIs %>% dplyr::select(ID_target, CI_RK1)
    cat("Distance-DBH-based Competition was quantified using", method,".
        Search radius =", radius,".\n")
    print(CI_RK1)
    return(CI_RK1)
  } else if (method == "CI_RK2") {
    CI_RK2 <- CIs %>% dplyr::select(ID_target, CI_RK2)
    cat("Distance-DBH-based Competition was quantified using", method,".
        Search radius =", radius,".\n")
    print(CI_RK2)
    return(CI_RK2)
  } else {
    stop("Invalid method. Supported methods: 'all', 'CI_Hegyi', 'CI_RK1', 'CI_RK2'.")
  }
}
