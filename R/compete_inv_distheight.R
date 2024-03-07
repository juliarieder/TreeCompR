#' Quantify distance-height-dependent Competition using ALS inventory data
#' @description
#' 'compete_dh()' returns a specific distance-height-dependent competition index
#' (or group of indices) for a list of target trees within a forest plot
#'
#' @param plot_source dataframe or path to inventory table of the
#'   plot, with structure: ID, x, y, h (in m). Cartesian coordinates have to
#'   be in metric system (in m)!
#' @param target_source dataframe or path to table of target trees within plot
#' with ID_target, x, y (does not have to be the same ID as in inventory table).
#'  Cartesian coordinates have to be in metric system!
#' @param radius numeric, Search radius (in m) around target tree, wherein all
#'   neighboring trees are classified as competitors
#' @param method character string assigning the method for quantifying
#'   competition "CI_Braathe", "CI_RK3", "CI_RK4" or "all"
#' @param tolerance numeric. Tolerance for the match with the tree coordinates.
#'   If a field measurement value is used for target_tree, take a higher
#'   tolerance value (default=1 m), depending on the measurement accuracy
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details
#' Using an inventory table to easily quantify distance-dependent tree
#' competition for a list of trees within a plot.
#' The input data can either be taken directly from field measurements or
#' derived beforehand from LiDAR point clouds.
#' The function calculates 3 Competition indices, based on tree heights and
#' distance to competitors.
#'
#' @section Methods:
#'  * CI_Braathe according to Braathe (1980)
#'    \eqn{\sum_{i=1}^{n} h_{i} / (h \cdot dist_{i})}
#'  * CI_RK3 according to CI5 in Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} \mathrm{arctan}(h_{i} / dist_{i}), h_{i} > h}
#'  * CI_RK4 based on CI3 in Rouvinen & Kuuluvainen (1997) and
#'    Contreras et al. (2011)
#'    \eqn{\sum_{i=1}^{n} (h_{i} / h) \cdot \mathrm{arctan}(h_{i} / dist_{i})}
#'
#' @section Tree Segmentation:
#' Various approaches can be used to segment airborne laser scanning point
#' clouds into single trees and to obtain inventory data based it. Existing R
#' packages for this are for example:
#' * lidR package with different options to segment the point cloud or a Canopy
#'    Height Model (CHM)
#' * itcLiDARallo within the package itcSegment
#'
#' Be careful with low resolution/low density point clouds, as oversegmentation
#' of trees is usually an issue!
#'
#' @section Literature:
#'  * Braathe, P., 1980. Height increment of young single trees in relation to
#'     height and distance of neighboring trees. Mitt. Forstl. VersAnst. 130,
#'      43–48.
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree
#'   crowns in relation to local competition in a natural mature Scot pine
#'   forest. Can. J. For. Res. 27, 890–902.
#'  * Contreras, M.A., Affleck, D. & Chung, W., 2011. Evaluating tree
#'  competition indices as predictors of basal area increment in western
#'  Montana forests. Forest Ecology and Management, 262(11): 1939-1949.
#'
#'
#' @return dataframe with ID_target, and one or more Indices depending on
#'   chosen method
#' @export
#'
#' @examples
#' \dontrun{
#' CI <- compete_ALS("path/to/invtable.csv",
#'   "path/to/target_trees.csv", radius = 10)
#' }
compete_dh <- function(plot_source, target_source, radius,
                       method = c("all", "CI_Braathe", "CI_RK3", "CI_RK4"),
                       tolerance = 1, ...) {

  # match arguments against the allowed values
  method <- match.arg(method)

  x <- y <- ID <- h <- ID_t <- ID_target <- euc_dist_comp <-
    CI_Braathe_part <- CI_RK3_part <- CI_RK4_part <- dist <- euc_dist <-
    x_seg <- y_seg <- x_segt <- y_segt <- status <- h_target <- NULL


  #read dataframe and .validate_inv or .validate_target still needed!
  #structure should be like this:
  colnames(plot_source) <- c("ID", "x_seg", "y_seg", "h")
  colnames(target_source) <- c("ID_target", "x", "y")

  # define competitors for target trees using internal function
  trees_competition <- .define_comp(plot_source, target_source,
                                    radius=radius, tolerance = tolerance)


  #calculate part of the Competition indices for each competitor
  trees_competition <- trees_competition %>%
    dplyr::mutate(CI_Braathe_part = (h/ (h_target * euc_dist_comp)),
                  CI_RK3_part = ifelse(h > h_target, atan(h / euc_dist_comp),
                                       0), #only include trees >  target tree
                  CI_RK4_part = (h / h_target) * atan(h / euc_dist_comp))
  #filter out the target tree itself
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate CIs (height-distance dependent) for each target tree
  CIs <- trees_competition %>%
    dplyr::group_by(ID_target) %>%
    dplyr::summarize(sum(CI_Braathe_part), sum(CI_RK3_part), sum(CI_RK4_part))
  colnames(CIs) <- c("ID_target", "CI_Braathe", "CI_RK3", "CI_RK4")

  if (method == "all") {
    cat("Distance-height-based competition was quantified with methods by
        Braathe and Rouvinen and Kuuluvainen. Search radius =", radius, ".\n")
    print(CIs)
    return(CIs)
  } else if (method == "CI_Braathe") {
    CI_Braathe <- CIs %>% dplyr::select(ID_target, CI_Braathe)
    cat("Distance-height-based Competition was quantified using",
        method,". Search radius =", radius,".\n")
    print(CI_Braathe)
    return(CI_Braathe)
  } else if (method == "CI_RK3") {
    CI_RK3 <- CIs %>% dplyr::select(ID_target, CI_RK3)
    cat("Distance-height-based Competition was quantified using",
        method,". Search radius =", radius,".\n")
    print(CI_RK3)
    return(CI_RK3)
  } else if (method == "CI_RK4") {
    CI_RK4 <- CIs %>% dplyr::select(ID_target, CI_RK4)
    cat("Distance-height-based Competition was quantified using",
        method,". Search radius =", radius,".\n")
    print(CI_RK4)
    return(CI_RK4)
  } else {
    stop("Invalid method. Supported methods: 'all', 'CI_Braathe',
         'CI_RK3', 'CI_RK4'.")
  }}


#' @keywords internal
#' internal function for defining the competitors for each target tree
.define_comp <- function(segtrees, ttrees, radius, tolerance, verbose = TRUE){
  # convert segmented trees to sf object
  segtrees_sf <- sf::st_as_sf(segtrees, coords = c("x_seg", "y_seg"))
  #convert target trees to sf object
  ttrees_sf <- sf::st_as_sf(ttrees, coords = c("x", "y"))
  #create buffer
  buffer <- sf::st_buffer(ttrees_sf, dist = radius,
                          nQuadSegs = 30)
  #get or set relation_to_geometry attribute of an sf object
  sf::st_agr(segtrees_sf) = "constant"
  sf::st_agr(buffer) = "constant"
  #make intersection to define if tree is within a radius or not
  trees_competition <- sf::st_intersection(segtrees_sf, buffer)
  #extract x,y again from geometry and convert matrix to dataframe
  coords <- sf::st_coordinates(trees_competition)
  coords <- data.frame(
    x_seg = coords[, 1],
    y_seg = coords[, 2]
  )
  #drop geometry to work with dataframe part
  trees_competition1 <- sf::st_drop_geometry(trees_competition)

  #link the x,y coordinates as columns to dataframe
  trees_competition <- cbind(trees_competition1, coords)

  #add x,y of target trees
  trees_competition <- dplyr::left_join(trees_competition,
                                        ttrees, by = "ID_target")
  #calculate euclidean distance between target trees and plot trees to
  #match/identify target trees within the plot by nearest neighbor
  #(not possible via ID, since they could have different IDs if you work
  #with lidar & field data)
  trees_competition <- trees_competition %>%
    dplyr::mutate(
      euc_dist = sqrt((x - x_seg)^2 + (y - y_seg)^2),
      status = ifelse(euc_dist == min(euc_dist), "target_tree",
                      ifelse(euc_dist > min(euc_dist), "competitor", NA)))
  #check if there is no tree within the tolerance threshold
  if (min(trees_competition$euc_dist) > tolerance) {
    stop("There was no tree found within the tolerance threshold.
         Check the coordinates again or, if the accuracy of GPS signal was low,
         set a new tolerance value.")
  } else {
    matching_rows <- subset(trees_competition, status == "target_tree")
  }
  matching_rows <- matching_rows %>%
    dplyr::rename(ID_t = ID,
                  dbh_target = dbh,
                  x_segt = x_seg,
                  y_segt = y_seg) %>%
    dplyr::select(ID_t, ID_target, dbh_target, x_segt, y_segt)
  trees_competition <- dplyr::left_join(trees_competition,
                                        matching_rows, by = "ID_target") %>%
    dplyr::mutate(euc_dist_comp = sqrt((x_segt - x_seg)^2 + (y_segt - y_seg)^2))

  return(trees_competition)
}



