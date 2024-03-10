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
compete_dd <- function(plot_source, target_source, radius,
                       method = c("all", "CI_Hegyi", "CI_RK1", "CI_RK2"),
                       tolerance = 1, dbh_unit = c("m", "cm", "mm"),
                       dbh_thr = 10, dbh_max = 100, ...) {


  # match arguments against the allowed values
  method <- match.arg(method)

  # avoid errors with undefined global values in CMD check
  x <- y <- x_seg <- y_seg <- CI_h_part <- CI_RK1_part <- CI_RK2_part <-
    euc_dist_comp <- dbh <- euc_dist <- ID_target <- ID_t <- dbh_target <-
    status <- ID <- dbh_target <- x_segt <- y_segt <- NULL

  # read data of whole plot
  #plot_source <- read_inv(plot_source, verbose = print_progress == "full", ...)
  #read dataframe and .validate_inv or .validate_target still needed!
  #structure should be like this:
  colnames(plot_source) <- c("ID", "x_seg", "y_seg", "dbh")
  colnames(target_source) <- c("ID_target", "x", "y")


  # define competitors for target trees using internal function
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
