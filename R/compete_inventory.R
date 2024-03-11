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



#' Calculate Tree Competition Indices Using Distance and DBH Inventory Data
#'
#' @description
#' 'compete_dd()' returns a specific distance-DBH-dependent competition index
#' (or group of indices) for a list of target trees within a forest plot
#'
#' @param plot_source dataframe or path to inventory table of the
#'   plot, with structure: ID, x, y, dbh (in m, cm or mm; specify in dbh_unit!).
#'   Cartesian coordinates have to be in metric system (in m)!
#' @param target_source dataframe or path to table of target trees within plot
#' with ID_target, x, y (does not have to be the same ID as in inventory table).
#'  Cartesian coordinates have to be in metric system!
#' @param radius numeric, Search radius (in m) around target tree, wherein all
#' neighboring trees are classified as competitors
#' @param edge_trees FALSE (default): just uses trees within the plot that are
#' not at the edge of the plot to prevent wrong CIs; if TRUE: calculates CIs
#' for all trees within the plot, even those at the edge
#' @param method character string assigning the method for quantifying
#' competition "CI_Hegyi", "CI_RK1", "CI_RK2" or "all"
#' @param tolerance numeric. Tolerance for the match with the tree coordinates.
#' If a field measurement value is used for target_tree, take a higher tolerance
#' value (default=1 m), depending on the GPS accuracy
#' @param dbh_unit character string for used unit of dbh in inventory data
#' "m", "cm" or "mm"
#' @param dbh_thr numeric, dbh threshold for classifying the tree as a
#'  competitor (default is 10 cm; trees with dbh smaller 10 cm are no
#'  competitors)
#' @param dbh_max numeric, dbh threshold (max) in cm, that is realistic for the
#' trees. (default: 100 cm). It causes a warning message, if one or more tree
#' within your plot shows higher dbh values.
#' @param ... additional arguments passed on to [data.table::fread()]
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
#'  * CI_RK1 according to CI1 Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} \mathrm{arctan}(d_{i} / dist_{i})}
#'  * CI_RK2 according to CI3 in Rouvinen & Kuuluvainen (1997)
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
compete_dd <- function(plot_source, target_source, radius, edge_trees = FALSE,
                       method = c("all", "CI_Hegyi", "CI_RK1", "CI_RK2"),
                       tolerance = 1, dbh_unit = c("m", "cm", "mm"),
                       dbh_thr = 10, dbh_max = 100, ...) {


  # match arguments against the allowed values
  method <- match.arg(method)

  # avoid errors with undefined global values in CMD check
  x <- y <- x_seg <- y_seg <- CI_h_part <- CI_RK1_part <- CI_RK2_part <-
    euc_dist_comp <- dbh <- euc_dist <- ID_target <- ID_t <- dbh_target <-
    status <- ID <- dbh_target <- x_segt <- y_segt <- NULL

  #read inventory table
  segtrees <- read_inv(plot_source)

  # specify target trees and
  #define competitors for target trees using internal function
  trees_competition <- .define_comp(plot_source, target_source,
                                    radius=radius, tolerance = tolerance)
  #check for dbh unit in inventory data
  dbh_unit = match.arg(dbh_unit)
  #multiplier needed to always use cm within the CIs
  mult <- c(m = 100, cm = 1, mm = 0.1)[dbh_unit]

  #convert dbh to unit cm if needed
  trees_competition <- trees_competition %>% dplyr::mutate(dbh = dbh * mult)

  #filter out trees that are too small to be considered as competitor
  #(dbh_thr default 10 cm)
  trees_competition <- trees_competition %>% dplyr::filter(dbh >= dbh_thr)
  # Identify rows with DBH higher than dbh_max to check if there was a problem
  # with automated segmentation (in case laser scanning data was used).
  # dbh_max default 100 cm, should be adjusted depending on tree species and age

  #select trees with higher dbh than threshold and generate message
  high_dbh_rows <- trees_competition %>% dplyr::filter(dbh > dbh_max) %>%
    select(ID, x_seg, y_seg, dbh)

  # generate warning message, in case dbh of a tree within the plot is
  #higher than dbh_max (segtrees also includes the target trees)
  if (nrow(high_dbh_rows) > 0) {
    cat("The following trees have a diameter above", dbh_max,"m\n")
    colnames(high_dbh_rows) <- c("ID", "x", "y", "dbh")
    print((high_dbh_rows[, c("ID", "x", "y", "dbh")]))
  }


  #calculate part of the Competition indices for each competitor
  trees_competition <- trees_competition %>%
    dplyr::mutate(CI_h_part = dbh / (dbh_target * euc_dist_comp),
                  CI_RK1_part = atan(dbh / euc_dist_comp),
                  CI_RK2_part = (dbh / dbh_target) * atan(dbh / euc_dist_comp))
  #filter out the target tree(s) itself
  trees_competition <- trees_competition %>% dplyr::filter(euc_dist_comp > 0)
  #calculate competition indices distance-height based:
  CIs <- trees_competition %>% dplyr::group_by(ID_target) %>% dplyr::summarise(
    CI_Hegyi = sum(CI_h_part),
    CI_RK1 = sum(CI_RK1_part),
    CI_RK2 = sum(CI_RK2_part))


  #output depending on method and searchradius
  if (method == "all") {
    cat("DBH-distance-based competition was quantified with methods
        by Hegyi and Rouvinen and Kuuluvainen. Search radius =", radius, ".\n")
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
    stop("Invalid method. Supported methods: 'all', 'CI_Hegyi',
         'CI_RK1', 'CI_RK2'.")
  }
}


#' @keywords internal
#' internal function for defining the competitors for each target tree
#' #still just working for inventory with dbh
#'
.define_comp <- function(segtrees, ttrees, edge_trees, radius,
                        thresh = 1, tolerance) {

  segtrees_sf <- sf::st_as_sf(segtrees, coords = c("x", "y"))
  sf::st_agr(segtrees_sf) = "constant"
  # If no target trees are defined (standard) and edge_trees is FALSE,
  # simply set ttrees_sf <- segtrees_sf
  if (is.null(ttrees)) {
    ttrees <- segtrees %>% rename(ID_target = ID, dbh_t = dbh)
    ttrees_sf <- sf::st_as_sf(ttrees, coords = c("x", "y"))
    # Define search radius
    thresh <- thresh

    # Get polygon for concave hull
    conc <- st_polygon(
      list(
        as.matrix(
          concaveman(cbind(segtrees$x, segtrees$y),
                     length_threshold = 2 * thresh)
        )
      )
    )
    # Find trees that are safely within the polygon (center)
        #dist = -radius
    buf_conc <- sf::st_buffer(conc, dist = -radius, singleSide = TRUE)

    #attribute variables are assumed to be spatially constant
    #throughout all geometries
    sf::st_agr(ttrees_sf) = "constant"

    # Check for intersections between points and buffer
    intersection <- st_intersection(ttrees_sf, buf_conc) %>% select(ID_target)

    # Update the location to "center" for intersecting points
    #if there are any intersections
    ttrees_sf <- ttrees_sf %>%
      mutate(tree_loc = ifelse(ID_target %in% intersection$ID_target, "center",
                               "edge"))
    #only use trees within center
    if (!edge_trees){
      ttrees_sf <- subset(ttrees_sf, tree_loc == "center")

    } else {
      ttrees_sf <- ttrees_sf
    }

  } else if (!is.null(target_trees)) {
    ttrees <- read_inv(target_source)
    ttrees <- ttrees %>% rename(ID_target = ID, dbh_t = dbh)
    ttrees_sf <- sf::st_as_sf(ttrees, coords = c("x", "y"))

  } else {
    stop("Invalid input")
  }


  #when target trees are set and validated: search for competitors

  #create buffer around target trees with dist = search radius
  buffer <- sf::st_buffer(ttrees_sf, dist = radius,
                          nQuadSegs = 30)
  #get or set relation_to_geometry attribute of an sf object
  #attribute variables are assumed to be spatially constant
  #throughout all geometries
  # convert segmented trees to sf object

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

  #link the x,y of plot tree coordinates as columns to dataframe
  segtrees1 <- segtrees %>% select(ID, x, y)
  trees_competition <- left_join(trees_competition1, segtrees1, by = "ID") %>%
    rename(x_seg = x, y_seg = y)

  #add x,y of target trees
  ttrees1 <- ttrees %>% select(ID_target, x, y)
  trees_competition <- dplyr::left_join(trees_competition,
                                        ttrees1, by = "ID_target")
  #calculate euclidean distance between target trees and plot trees to
  #match/identify target trees within the plot by nearest neighbor
  #(not possible via ID, since they could have different IDs if you
  #work with lidar & field data)


  #check for each target tree which tree it is within segtrees
  #and which trees are competitors
  trees_competition <- trees_competition %>%
    group_by(ID_target) %>%
    mutate(
      euc_dist = sqrt((x - x_seg)^2 + (y - y_seg)^2),
      is_exact_match = all(x == x_seg, y == y_seg)
    ) %>%
    mutate(
      status = case_when(
        is_exact_match ~ "target_tree",
        TRUE ~ ifelse(euc_dist == min(euc_dist), "target_tree", "competitor")
      )
    ) %>%
    select(-is_exact_match)  # Drop the intermediate column
  if (min(trees_competition$euc_dist) > tolerance) {
    #or generate warning instead of stop?
    stop("There was no tree found within the tolerance threshold.
         Check the coordinates again or, if the accuracy of GPS signal was low,
         set a new tolerance value.")
  }

  return(trees_competition)
  #output should be:
  # ID, dbh, ID_target, dbh_t, (tree_loc), x_seg, y_seg, x, y, euc_dist, status
}

