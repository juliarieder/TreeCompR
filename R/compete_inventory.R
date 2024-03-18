#' Quantify distance-dependent Competition using inventory data
#' @description
#' 'compete_inv()' returns a specific distance-height-dependent or
#' distance-dbh-dependent competition index (or group of indices) for all trees
#' within a forest plot or specified target trees
#'
#' @param inv_source dataframe or path to inventory table of the plot, with
#' structure: ID, x, y, dbh (in cm) and/or h (in m). Cartesian coordinates have
#' to be in metric system (in m)!
#' @param target_source dataframe or path to table of target trees within plot
#' with ID_target, x, y (does not have to be the same ID as in inventory table).
#'  Cartesian coordinates have to be in metric system!
#'  (and same coordinate system as inv_source!)
#' @param radius numeric, Search radius (in m) around target tree, wherein all
#'   neighboring trees are classified as competitors
#' @param method character string assigning the method for quantifying
#'   competition dbh-distance-dependent: "CI_Hegyi", "CI_RK1", "CI_RK2",
#'   height-distance-dependent: "CI_Braathe", "CI_RK3", "CI_RK4" or "all"
#'   for all the indices
#' @param tolerance numeric. Tolerance for the match with the tree coordinates.
#'   If coordinates are measured in the field with GPS, but inv_source contains
#'   x,y from segmentation, allow for tolerance to match the target tree
#'   positions. (default = 1 m), depending on the measurement accuracy of GPS
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details
#' Using an inventory table to easily quantify distance-dependent tree
#' competition for a list of trees within a plot or all the trees.
#' The input data can either be taken directly from field measurements or
#' derived beforehand from LiDAR point clouds.
#' The function calculates 6 different Competition indices, based on tree
#' heights or dbh and distance to competitors.
#'
#' @section Methods:
#'  * CI_Hegyi Index introduced by Hegyi (1974)
#'    \eqn{\sum_{i=1}^{n} d_{i} / (d \cdot dist_{i})}
#'  * CI_RK1 according to CI1 Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} \mathrm{arctan}(d_{i} / dist_{i})}
#'  * CI_RK2 according to CI3 in Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} (d_{i} / d) \cdot \mathrm{arctan}(d_{i} / dist_{i})}
#'  * CI_Braathe according to Braathe (1980)
#'    \eqn{\sum_{i=1}^{n} h_{i} / (h \cdot dist_{i})}
#'  * CI_RK3 according to CI5 in Rouvinen & Kuuluvainen (1997)
#'    \eqn{\sum_{i=1}^{n} \mathrm{arctan}(h_{i} / dist_{i}), h_{i} > h}
#'  * CI_RK4 based on CI3 in Rouvinen & Kuuluvainen (1997) and
#'    Contreras et al. (2011)
#'    \eqn{\sum_{i=1}^{n} (h_{i} / h) \cdot \mathrm{arctan}(h_{i} / dist_{i})}
#'
#' @section Tree Segmentation:
#' Various approaches can be used to segment (airborne) laser scanning point
#' clouds into single trees and to obtain inventory data based it. Existing R
#' packages for this are for example:
#' * TreeLS package for automated segmentation of terrestrial/mobile laser scans
#' * lidR package with different options to segment the point cloud or a Canopy
#'    Height Model (CHM)
#' * itcLiDARallo within the package itcSegment
#'
#' Be careful with low resolution/low density point clouds, as oversegmentation
#' of trees is usually an issue!
#'
#' @section Literature:
#'  * Hegyi, F., 1974. A simulation model for managing jackpine stands. In:
#'    Fries, J. (Ed.), Proceedings of IUFRO meeting S4.01.04 on Growth models
#'    for tree and stand simulation, Royal College of Forestry, Stockholm.
#'  * Braathe, P., 1980. Height increment of young single trees in relation to
#'    height and distance of neighboring trees. Mitt. Forstl. VersAnst. 130,
#'    43–48.
#'  * Rouvinen, S., Kuuluvainen, T., 1997. Structure and asymmetry of tree
#'    crowns in relation to local competition in a natural mature Scot pine
#'    forest. Can. J. For. Res. 27, 890–902.
#'  * Contreras, M.A., Affleck, D. & Chung, W., 2011. Evaluating tree
#'    competition indices as predictors of basal area increment in western
#'    Montana forests. Forest Ecology and Management, 262(11): 1939-1949.
#'
#'
#' @return dataframe with ID_target, and one or more Indices depending on
#'   chosen method
#' @export
#'
#' @examples
#' \dontrun{
#' CI <- compete_inv("path/to/invtable.csv",
#'   "path/to/target_trees.csv", radius = 10, method = "CI_Hegyi")
#' }
compete_inv <- function(inv_source, target_source, radius,
                        method = c("all", "CI_Hegyi", "CI_Braathe",
                                   "CI_RK1", "CI_RK2", "CI_RK3", "CI_RK4"),
                        x = NULL, y = NULL,
                        dbh = NULL, height = NULL, id = NULL,
                        dbh_unit = c("cm", "m", "mm"),
                        height_unit = c("m", "cm", "mm"),
                        verbose = TRUE,
                        tolerance = 1,
                        ...) {

  # match arguments against the allowed values
  method <- match.arg(method)

  # if target trees are already defined in inv_source, bypass the following
  # steps and directly compute competition indices
  if(!inherits(inv_source, "target_inv")){
    # catch and validate variable names (treated as character if not NULL,
    # else, NULL is passed on to read_inv())
    if (!is.null(substitute(x)))      x      <- as.character(substitute(x))
    if (!is.null(substitute(y)))      y      <- as.character(substitute(y))
    if (!is.null(substitute(dbh)))    dbh    <- as.character(substitute(dbh))
    if (!is.null(substitute(height))) height <- as.character(substitute(height))
    if (!is.null(substitute(id)))     id     <- as.character(substitute(id))

    # read and validate forest inventory dataset
    inv <- read_inv(
      inv_source, x = x, y = y, dbh = dbh, height = height, id = id,
      dbh_unit = dbh_unit, height_unit = height_unit, verbose = verbose, ...)

    # check if data required to calculate indices are available
    if (method %in%  c("CI_Hegyi", "CI_RK1", "CI_RK2") &&
        !"dbh" %in% names(inv)) {
      stop("Diameter at breast height is required for ", method, ".")
    }
    if (method %in%  c("CI_Braathe", "CI_RK3", "CI_RK4") &&
        !"dbh" %in% names(inv)) {
      stop("Tree height of all trees is required for ", method, ".")
    }

    # check if target_source is a length 1 character
    if(inherits(target_source, "character") && length(target_source) == 1){
      # if it is a valid file path, read and validate target tree dataset
      if (file.exists(target_source)){
        target_source <- read_inv(
          target_source, x = x, y = y, dbh = dbh, height = height, id = id,
          dbh_unit = dbh_unit, height_unit = height_unit, verbose = verbose,
          ...)
        # else it is passed to define_target as a character string
      }
    }
    # validate target source in the same way if it is an unformatted table
    if(inherits(target_source, "data.frame")){
      target_source <- read_inv(
        target_source, x = x, y = y, dbh = dbh, height = height, id = id,
        dbh_unit = dbh_unit, height_unit = height_unit, verbose = verbose, ...)
    }
    # define target trees
    inv <- define_target(inv = inv, target_source = target_source,
                         radius = radius, thresh = thresh, tol = tol)
  }
  # test if there are any duplicated coordinates (resulting in 0 distance -->
  # singularity in inverse distance weighting)
  if (any(!duplicated(inv[,c("x", "y")]))) {
    stop("Dataset contains duplicate coordinates. ",
         "Please revise tree positions.")
  }
  # define methods to calculate if method == "all"
  if (method == "all"){
    method <- c(
      ifelse("dbh" %in% names(inv),
             c("CI_Hegyi", "CI_RK1", "CI_RK2"), NULL),
      ifelse("height" %in% names(inv),
             c("CI_Braathe", "CI_RK3", "CI_RK4"), NULL)
    )
  }
  # compute matrices required for calculation
  # Euclidean distance matrix
  distance <- with(inv, sqrt(outer(x, x, "-") ^ 2 + outer(y, y, "-") ^ 2))
  # get inverse distance matrix
  inv_distance  <- 1 / distance # careful: diagonal becomes infinite
  diag(inv_distance) <- 0 # focal tree taken out of the calculation and
  # singularity resolved by setting diagonal to zero
  # get trees within critical distance
  trees_in_radius <- distance <= radius
  # trees outside radius are taken out of the summation by setting to zero

  # get matrix with focal trees (for row-wise summation of indices)
  target_matrix <- matrix(rep(ifelse(inv$type == "target", 1, NA),
                              nrow(inv)), ncol = nrow(inv))
  if (any(method %in%  c("CI_Hegyi", "CI_RK1", "CI_RK2"))) {
    # get matrix with focal tree dbh
    focal_dbh <- matrix(rep(inv$dbh, nrow(inv)), ncol = nrow(inv))
    # get matrix with neighbor tree dbh
    neighbor_dbh <- t(focal_dbh) # calculation per tree is row-wise so neighbor
    # matrix has to be column-wise
  }
  if (any(method %in%  c("CI_Braathe", "CI_RK3", "CI_RK4"))) {
    # get matrix with focal tree height
    focal_height <- matrix(rep(inv$height, nrow(inv)), ncol = nrow(inv))
    # get matrix with neighbor tree height
    neighbor_height <- t(focal_height) # caculation per tree is row-wise
    #so neighbor
    # matrix has to be column-wise
  }
  # compute Hegyi index for all target trees
  if("CI_Hegyi" %in% method){
    inv$CI_Hegyi <- rowSums(
      target_matrix * trees_in_radius *
        inv_distance * neighbor_dbh / focal_dbh)
  }
  # compute Braathe index for all target trees
  if("CI_Braathe" %in% method){
    inv$CI_Braathe <- rowSums(
      target_matrix * trees_in_radius *
        inv_distance * neighbor_height / focal_height)
  }
  # compute RK1 index for all target trees
  if("CI_RK1" %in% method){
    inv$CI_RK1 <- rowSums(
      target_matrix * trees_in_radius *
        atan(inv_distance * neighbor_dbh))
  }
  # compute RK2 index for all target trees
  if("CI_RK2" %in% method){
    inv$CI_RK2 <- rowSums(
      target_matrix * trees_in_radius *
        atan(inv_distance * neighbor_dbh) * neighbor_dbh / focal_dbh)
  }
  # compute RK3 index for all target trees
  if("CI_RK3" %in% method){
    inv$CI_RK3 <- rowSums(
      target_matrix * trees_in_radius *
        atan(inv_distance * neighbor_height))
  }
  # compute RK4 index for all target trees
  if("CI_RK4" %in% method){
    inv$CI_RK4 <- rowSums(
      target_matrix * trees_in_radius *
        atan(inv_distance * neighbor_height) * neighbor_height / focal_height)
  }
  # format and return output
  output <- list(
    inventory = inv,
    radius = radius,
    method = method,
    target_type = attr(inv, "target_type"),
    vars = list(id = id, x = x, y = y, dbh = dbh, height = height)
  )
  # define class
  class(output) <- c("compete_inv", class(output))
  # return output
  return(output)
}
