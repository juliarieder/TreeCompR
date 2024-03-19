#' Quantify size- and distance-dependent competition using inventory data
#' @description
#' 'compete_inv()' returns a specific distance-height-dependent or
#' distance-dbh-dependent competition index (or group of indices) for all trees
#' within a forest plot or specified target trees.
#'
#' @param inv_source either an object of class `target_inv`, or any object that
#'   can be imported by [read_inv()] (in this case, `x`, `y`, `id`,
#'   `dbh`, and/or `height` can be specified as in this function -- see the
#'   corresponding documentation for details).
#'   If provided with a `target_inv` object, the function overrides further
#'   arguments passed to [read_inv()] and [define_target()].
#' @param target_source  one of the following:
#'   1. a path to an any object that can be imported by [read_inv()] (in this
#'   case, column specifications have to be the same as in `inv_source` - if
#'   this is not possible, load outside of `compete_inv()`).
#'   2. a vector of class `"character"` containing the tree IDs identifying the
#'   target trees in the  same format as in the `id` column of `inv`,
#'   3. a vector of class `logical` specifying for each row of `inv` whether
#'   the corresponding tree is a target tree,
#'   4. another object of class `forest_inv` containing the coordinates of the
#'   target trees. In this case, the coordinates are matched against the
#'   coordinates in `inv` and IDs may differ (useful e.g. when target trees
#'   are defined based on GPS coordinates and matched against an airborne laser
#'   scanning dataset).
#'   5. a character vector of length 1 defining the method by which the target
#'   trees should be determined. Allowed are `"buff_edge"` for excluding all
#'   trees that are at least one search radius from the forest edge,
#'   `"exclude_edge"` for only excluding edge trees or `"all"` for including
#'   all trees of the dataset (which is hardly ever a good idea unless all
#'   trees in the entire forest are in the dataset). The standard is
#'   `"buff_edge"`. See [define_target()] for details.
#' @param radius numeric of length 1. Search radius (in m) around the target
#'   tree. All neighboring trees within this radius are classified as
#'   competitors.
#' @param method character string assigning the method for quantifying
#'   competition. dbh-distance-dependent methods are "CI_Hegyi", "CI_RK1",
#'   and "CI_RK2".Height-distance-dependent methods are "CI_Braathe", "CI_RK3",
#'   "CI_RK4". "all" can be specified to compute all the indices that can be
#'   calculated with the available data.
#' @param tol numeric of length 1. Tolerance for the match with the tree
#'   coordinates. If coordinates are measured in the field with GPS, but
#'   `inv_source` contains x and y coordinates from a larger number of trees
#'   obtained by segmentation, this is the tolerance for the matching the
#'   forest inventory against the target tree positions. If no forest tree is
#'   within the tolerance to a target tree, no competition indices will be
#'   calculated for this tree and the function will return a warning.
#'   `tol` defaults to 1 m, but should be chosen depending on the measurement
#'   accuracy of the GPS coordinates.
#' @param ... additional arguments passed on to [data.table::fread()].
#' @inheritParams read_inv
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
#' The available competition indices are computed according to the following
#' equations, where \eqn{d_i} and \eqn{h_i} are the dbh and height of neighbor
#' tree \eqn{i}, \eqn{d} and \eqn{h} are dbh and height of the focal tree, and
#' \eqn{dist_i} is the distance of neighbor tree \eqn{i}.
#'
#' ### Diameter-based competition indices
#'  * CI_Hegyi introduced by Hegyi (1974): \cr
#'    \eqn{CI_{Hegyi} = \sum_{i=1}^{n} d_{i} / (d \cdot dist_{i})}
#'  * CI_RK1 according to CI1 Rouvinen & Kuuluvainen (1997):\cr
#'    \eqn{CI_{RK1} = \sum_{i=1}^{n} \mathrm{arctan}(d_{i} / dist_{i})}
#'  * CI_RK2 according to CI3 in Rouvinen & Kuuluvainen (1997): \cr
#'    \eqn{CI_{RK2} =\sum_{i=1}^{n} (d_{i} / d) \cdot \mathrm{arctan}(d_{i}
#'    / dist_{i})}
#'
#'  ### Height-based competition indices
#'  * CI_Braathe according to Braathe (1980): \cr
#'    \eqn{CI_{Braathe} = \sum_{i=1}^{n} h_{i} / (h \cdot dist_{i})}
#'  * CI_RK3 according to CI5 in Rouvinen & Kuuluvainen (1997): \cr
#'    \eqn{CI_{RK3} = \sum_{i=1}^{n} \mathrm{arctan}(h_{i} / dist_{i})}
#'    for all trees with \eqn{h_{i} > h}
#'  * CI_RK4 based on CI3 in Rouvinen & Kuuluvainen (1997) and
#'    Contreras et al. (2011): \cr
#'    \eqn{CI_{RK4} = \sum_{i=1}^{n} (h_{i} / h) \cdot
#'    \mathrm{arctan}(h_{i} / dist_{i})}
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
#' @return `compete_inv` object: a modified data.frame with the forest
#'   invetory data, target tree specifications and or more competition indices
#'   depending on chosen method.
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
                        tol = 1,
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
      dbh_unit = dbh_unit, height_unit = height_unit, verbose = verbose,
      names_as_is = TRUE, ...)

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
          names_as_is = TRUE, ...)
        # else it is passed to define_target as a character string
      }
    }
    # validate target source in the same way if it is an unformatted table
    if(inherits(target_source, "data.frame") &&
       !inherits(target_source, "forest_inv")){
      target_source <- read_inv(
        target_source, x = x, y = y, dbh = dbh, height = height, id = id,
        dbh_unit = dbh_unit, height_unit = height_unit, verbose = verbose,
        names_as_is = TRUE, ...)
    }
    # define target trees
    inv <- define_target(inv = inv, target_source = target_source,
                         radius = radius, tol = tol)
  } else { # warn if radii are different
    if (attr(inv, "target_method") %in% c("buff_edge", "exclude_edge")){
      if(attr(inv, "spatial_radius") != radius){
        warning("Radius used to determine target trees differs from search",
                " radius for competition indices.")
      }
    }
  }
  # test if there are any duplicated coordinates (resulting in 0 distance -->
  # singularity in inverse distance weighting)
  if (any(duplicated(inv[,c("x", "y")]))) {
    stop("Dataset contains duplicate coordinates. ",
         "Please revise tree positions.")
  }
  # define methods to calculate if method == "all"
  if (method == "all"){
    # preallocate empty vector
    method <- vector("character")
    # get names with available data
    if("dbh" %in% names(inv))
      method <- c("CI_Hegyi", "CI_RK1", "CI_RK2")
    if("height" %in% names(inv))
      method <- c(method, c("CI_Braathe", "CI_RK3", "CI_RK4"))
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
  target_matrix <- matrix(rep(ifelse(inv$target, 1, NA),
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
      target_matrix * trees_in_radius * (neighbor_height > focal_height) *
        atan(inv_distance * neighbor_height))
  }
  # compute RK4 index for all target trees
  if("CI_RK4" %in% method){
    inv$CI_RK4 <- rowSums(
      target_matrix * trees_in_radius *
        atan(inv_distance * neighbor_height) * neighbor_height / focal_height)
  }
  # add radius and method as attributes
  attr(inv, "radius") <- radius
  attr(inv, "method") <- method
  # add tree coordinates as attributes
  attr(inv, "target_trees") <- as.matrix(inv[inv$target, c("x", "y")])
  attr(inv, "edge_trees") <- as.matrix(inv[!inv$target, c("x", "y")])
  # filter out edge trees
  inv <- inv[inv$target, ]
  # remove target column (table only contains target trees)
  inv$target <- NULL
  # define class
  class(inv) <- c("compete_inv", class(inv))
  # return output
  return(inv)
}



# Define printing method for target_pc objects:
#' @rdname compete_inv
#' @format NULL
#' @usage NULL
#' @export
print.compete_inv <- function(x, ...){
  # get description of target source from lookup table
  target <- as.character(
    c(inventory = "second inventory",
      character = "character vector",
      logical   = "logical vector",
      exclude_edge = "excluding edge",
      buff_edge = "excluding buffer around edge"
    )[ attr(x, "target_type")]
  )
  # print header
  cat("---------------------------------------------------------------------",
      "\n'compete_inv' class inventory with distance-based competition indices",
      "\nCollection of data for", nrow(x),"target and",
        nrow(attr(x, "edge_trees")), "edge trees.",
      "\nSource of target trees:",target, "\t Search radius:", attr(x, "radius"),
      "\n---------------------------------------------------------------------\n"
  )
  if (ncol(x) >= 10) names(x) <- gsub("CI_", "", names(x))

  if (nrow(x) < 6) {
    # if there are almost no observations, print the entire dataset
    print(as.data.frame(x), digits = 3)
  } else {
    # else print beginning and end of the data.frame
    temp <- x[1,]
    row.names(temp) <- " "
    for(i in 1:ncol(temp)) temp[, i] <- "..."
    x[, sapply(x, is.numeric)] <- round(x[, sapply(x, is.numeric)], 3)
    print(
      rbind(head(as.data.frame(x), 3),
            temp,
            tail(as.data.frame(x), n = 3)
      )
    )
  }
}



