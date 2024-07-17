#' Quantify size- and distance-dependent competition using inventory data
#' @description `compete_inv()` computes one or several distance-height- or
#'  distance-dbh-dependent competition indices based on forest inventory data.
#'
#' @param inv_source either an object of class `target_inv`, or any object that
#'   can be imported by [read_inv()] (in this case, `x`, `y`, `id`,
#'   `dbh`, and/or `height` can be specified as in this function -- see the
#'   corresponding documentation for details).
#'   If provided with a `target_inv` object, the function ignores
#'   `target_source` and overrides further arguments passed to [read_inv()] and
#'   [define_target()].
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
#'   `"exclude_edge"` for only excluding edge trees or `"all_trees"` for
#'   including all trees of the dataset (which is hardly ever a good idea
#'   unless all trees in the entire forest are in the dataset). The standard is
#'   `"buff_edge"`. See [define_target()] for details.
#' @param radius numeric of length 1. Search radius (in m) around the target
#'   tree. All neighboring trees within this radius are classified as
#'   competitors.
#' @param method character string assigning the competition index functions to
#' calculate. Can either be a vector with one or several of `"CI_Hegyi"`,
#'    `"CI_RK1"`,`"CI_RK2"`, `"CI_Braathe"`, `"CI_RK3"`, `"CI_RK4"`,
#'    `"CI_size"` and/or the names of user-specified functions, or
#'    `"all_methods"` (the default).`"all_methods"` computes all built-in
#'   indices that can be calculated with the available data.
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
#' @details `compete_inv()` calculates one or several distance-dependent tree
#'   competition indices based on forest inventory data. These can be obtained
#'   either with classical forest inventory methods, or be derived from LiDAR
#'   point clouds (see below).
#'
#'   Inventory data can be either loaded from source, imported from an object
#'   inheriting from class `data.frame` (i.e., data.frames, tibbles,
#'   data.table objects etc.) or a `forest_inv` type object created with
#'   [read_inv()]. `compete_inv()` takes the same arguments for reading
#'   inventory data and has the same flexibility as [read_inv()].
#'
#'   To compute competition indices for trees from an inventory dataset, it is
#'   necessary to decide on the target trees for the analysis. While it is also
#'   possible to calculate competition indices for all trees in the inventory,
#'   this is almost never a good idea because unless the dataset covers all
#'   trees in the entire forest, there will be intense edge effects for the
#'   trees at the edge of the spatial extent of the dataset.
#'   `compete_inv()` allows to define target trees in a number of different ways
#'   based on the function [define_target()] that is called internally.
#'
#'
#'   ## Available competition indices
#'   The competition indices are computed according to the following
#'   equations, where \eqn{d_i} and \eqn{h_i} are the dbh and height of neighbor
#'   tree \eqn{i}, \eqn{d} and \eqn{h} are dbh and height of the target tree,
#'   and \eqn{dist_i} is the distance from neighbor tree \eqn{i} to the target
#'   tree.
#'
#'  _Diameter-based competition indices_
#'  * CI_Hegyi introduced by Hegyi (1974): \cr
#'    \eqn{CI_{Hegyi} = \sum_{i=1}^{n} d_{i} / (d \cdot dist_{i})}
#'  * CI_RK1 according to CI1 Rouvinen & Kuuluvainen (1997):\cr
#'    \eqn{CI_{RK1} = \sum_{i=1}^{n} \mathrm{arctan}(d_{i} / dist_{i})}
#'  * CI_RK2 according to CI3 in Rouvinen & Kuuluvainen (1997): \cr
#'    \eqn{CI_{RK2} =\sum_{i=1}^{n} (d_{i} / d) \cdot \mathrm{arctan}(d_{i}
#'    / dist_{i})}
#'
#'  _Height-based competition indices_
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
#'  _Generic size-based Hegyi-type competition indices_
#'  * CI_size based on Hegyi (1974), but with a user-specified size-related
#'    variable: \cr
#'    \eqn{CI_{size} = \sum_{i=1}^{n} size_{i} / (size \cdot dist_{i})}
#'
#'  Further indices can be user-specified by developing a corresponding
#'  function (see [competion_indices] for details).
#'
#'  As all these indices are distance-weighted sums of the relative size of all
#'  competitor trees within the search radius compared to the target tree (or a
#'  sum of transformations thereof), they are very sensitive to the choice of
#'  the search radius. It is not generally possible to meaningfully compare
#'  competition indices computed with different search radii. Over which
#'  distance competitors affect the growth of the central tree is certainly
#'  specific and likely also depends on the average size of trees of the same
#'  species as the target tree and how far its roots spread under the local
#'  conditions. Lorimer (1983) recommends to use 3.5 times the mean crown radius
#'  of the target trees, but it is likely that there is no single value that
#'  works well under all conditions, and possible that the same values of
#'  competition indices calculated with the same radius have different meanings
#'  for different species.
#'
#'  ## Tree Segmentation
#'   Various approaches can be used to segment (airborne) laser scanning point
#'   clouds into single trees and to obtain inventory data based it. Existing R
#'   packages for this are for example:
#'   * `TreeLS` package for automated segmentation of terrestrial/mobile laser
#'      scans
#'   * `lidR` package with different options to segment the point cloud or a
#'     Canopy Height Model (CHM)
#'   * `itcLiDARallo()` from the package `itcSegment`
#'
#'   Be careful with low resolution/low density point clouds, as
#'   oversegmentation of trees is usually an issue!
#'
#'   For examples of workflows to obtain inventory dator from airborne
#'   laser scanning data or terrestrial/mobile laser scanning data, see
#'   [ALS
#'   workflow](https://juliarieder.github.io/TreeCompR/articles/ALS_inventory.html)
#'   and [TLS
#'   workflow](https://juliarieder.github.io/TreeCompR/articles/TLS_inventory.html),
#'   respectively.
#'
#' ## Literature
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
#'  * Lorimer, C.G., 1983. Tests of age-independent competition indices for
#'    individual trees in natural hardwood stands. For. Ecol. Manage. 6,
#'    343–360.
#'
#' @return object of class`compete_inv`: a modified data.table with the
#'   position and size of the designated target tree(s) and one or more
#'   competition indices depending on chosen method(s).
#'
#' @seealso [read_inv()] to read forest inventory data,
#'   [define_target()] for designating target trees,
#'   [competition_indices] for a list of available indices,
#'   [plot_target()] to plot target tree positions in `target_inv` and
#'   `compete_inv` objects.
#' @export
#'
#' @examples
#' \dontrun{
#' # Quantify the Hegyi index for specified target trees with search radius 10m
#' CI <- compete_inv("path/to/invtable.csv",
#'   "path/to/target_trees.csv", radius = 10, method = "CI_Hegyi")
#' # Quantify the Braathe index for specified target trees with search radius
#' #10m and adjust
#' CI <- compete_inv("path/to/invtable.csv",
#'   "path/to/target_trees.csv", radius = 10, method = "CI_Braathe")
#' # Specify the units of dbh or height of your input data
#' CI <- compete_inv("path/to/invtable.csv",
#'   "path/to/target_trees.csv", radius = 10, method = "CI_Hegyi",
#'   dbh_unit = "m", height_unit = "m")
#' # Quantify all available indices for all trees within the plot that are one
#' #search radius away from plot edge
#' CI <- compete_inv("path/to/invtable.csv", target_source = "buff_edge",
#'         radius = 12, method = "all_methods")
#' }
compete_inv <- function(inv_source, target_source = "buff_edge", radius,
                        method = "all_methods",
                        x = NULL, y = NULL,
                        dbh = NULL, height = NULL, id = NULL,
                        dbh_unit = c("cm", "m", "mm"),
                        height_unit = c("m", "cm", "mm"),
                        verbose = TRUE,
                        tol = 1,
                        ...) {
  # validate vector of specified CI methods
  method <- .validate_method(method)

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

    # check if method requirements are met
    method <- .validate_reqs(method, names(inv))

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
                         radius = radius, tol = tol, verbose = verbose,
                         crop_to_target = TRUE)
  } else {
    # if inv_source is a target_inv object, use it as inv directly
    inv <- inv_source
    # warn if radii are different
    if (attr(inv, "target_type") %in% c("buff_edge", "exclude_edge",
                                        "inventory")){
      if(attr(inv, "spatial_radius") != radius){
        warning(
          .wr("Radius used to determine target trees / filter surrounding",
              "trees differs from search radius for competition indices.")
        )
      }
    }
  }

  # compute matrices required for calculation
  # Euclidean distance matrix
  distance <- with(inv, sqrt(outer(x[target], x, "-") ^ 2 +
                               outer(y[target], y, "-") ^ 2))
  # get inverse distance matrix
  inv_distance  <- 1 / distance # careful: diagonal becomes infinite
  inv_distance[with(inv, outer(id[target], id, "=="))] <- 0
  # singularity resolved by setting to zero for identical values
  # get trees within critical distance
  trees_in_radius <- distance <= radius
  # trees outside radius are taken out of the summation by setting to zero

  # get matrix with focal trees (for row-wise summation of indices)
  if (any(method %in%  c("CI_Hegyi", "CI_RK1", "CI_RK2"))) {
    # get matrix with focal tree dbh
    focal_dbh <- matrix(rep(inv$dbh[inv$target], nrow(inv)), ncol = nrow(inv))
    # get matrix with neighbor tree dbh
    neighbor_dbh <-  matrix(rep(inv$dbh, sum(inv$target)), ncol = nrow(inv),
                            byrow = TRUE)
    # calculation per tree is row-wise so neighbor matrix has to be column-wise
  }
  if (any(method %in%  c("CI_Braathe", "CI_RK3", "CI_RK4"))) {
    # get matrix with focal tree height
    focal_height <-matrix(rep(inv$height[inv$target], nrow(inv)),
                          ncol = nrow(inv))
    # get matrix with neighbor tree height
    neighbor_height <- matrix(rep(inv$height, sum(inv$target)),
                              ncol = nrow(inv),
                              byrow = TRUE)
    # caculation per tree is row-wise so neighbor matrix has to be column-wise
  }
  # prepare output file (remove edge trees and target column)
  out <- inv[inv$target, ]
  out$target <- NULL

  # compute Hegyi index for all target trees
  if("CI_Hegyi" %in% method){
    out$CI_Hegyi <- rowSums(trees_in_radius * inv_distance *
                              neighbor_dbh / focal_dbh)
  }
  # compute Braathe index for all target trees
  if("CI_Braathe" %in% method){
    out$CI_Braathe <- rowSums(trees_in_radius * inv_distance *
                                neighbor_height / focal_height)
  }
  # compute RK1 index for all target trees
  if("CI_RK1" %in% method){
    out$CI_RK1 <- rowSums(trees_in_radius * atan(inv_distance * neighbor_dbh))
  }
  # compute RK2 index for all target trees
  if("CI_RK2" %in% method){
    out$CI_RK2 <- rowSums(trees_in_radius * atan(inv_distance * neighbor_dbh)
                          * neighbor_dbh / focal_dbh)
  }
  # compute RK3 index for all target trees
  if("CI_RK3" %in% method){
    out$CI_RK3 <- rowSums(trees_in_radius * (neighbor_height > focal_height) *
                            atan(inv_distance * neighbor_height))
  }
  # compute RK4 index for all target trees
  if("CI_RK4" %in% method){
    out$CI_RK4 <- rowSums(trees_in_radius * atan(inv_distance * neighbor_height)
                          * neighbor_height / focal_height)
  }
  # add radius and method as attributes
  attr(out, "radius") <- radius
  attr(out, "method") <- method
  # pass on target type from original inventory
  attr(out, "target_type") <- attr(inv, "target_type")
  # add tree coordinates as attributes
  attr(out, "target_trees") <- as.matrix(inv[inv$target, c("x", "y")])
  attr(out, "edge_trees") <- as.matrix(inv[!inv$target, c("x", "y")])
  # define class
  class(out) <- c("compete_inv", class(inv))
  # return output
  return(out)
}


#' @keywords internal
#' internal function for validating a list of CI methods
.validate_method <- function(method){
  # list built-in methods
  meths <-c("all_methods", "CI_Hegyi", "CI_RK1", "CI_RK2",
            "CI_Braathe", "CI_RK3", "CI_RK4", "CI_size")

  # validate user-specified methods
  user_spec <- method[!method %in%  meths]
  if (length(user_spec) > 0){
    for (meth in user_spec){
      # check if it is a function
      if (!is.function(meth)) stop(
        .wr("Invalid competition index function detected:", meth,
            "is not a  function.")
      )
      # check if it hast the correct formal arguments
      if (any(names(formals(meth)) != c("target", "inv"))){
        stop(
          .wr("Invalid competition index function detected:",
              paste0(meth, "()."),
              "Competition index functions take 'target' and 'inv' as",
              "arguments and return a single value: 'function(target, inv)'")
        )
      }
    }
  }
  # if all_methods is in the selection, just return all_methods and
  # user specified methods
  if ("all_methods" %in% method) method <- c("all_methods", user_spec)

  # return vector with methods
  method
}

#' @keywords internal
#' internal function for validating a list of CI methods
.validate_reqs <- function(method, vars){
  # test if explicitly specified methods can be used
  if (!"all_methods" %in% method){
    # define method requirements for built-in indices
    reqs <- c(CI_Hegyi = "dbh", CI_RK1 = "dbh", CI_RK2 = "dbh",
              CI_Braathe = "height", CI_RK3 = "height", CI_RK4  = "height",
              CI_size = "size")

    # get user-specified and built-in methods
    user_spec <- method[!method %in% names(reqs)]
    built_in <- method[method %in% names(reqs)]

    # find methods with met requirements
    met <- reqs[reqs %in% vars]

    # find built-in methods in specifications whose requirements are not met
    unmet <- built_in[!built_in %in% names(met)]

    # stop if there are unmet requirements
    if (length(unmet) > 0){
      stop(
        "The following competition indices require variables ",
        "not in the inventory: \n",
        paste(paste0(unmet, " (requires '", reqs[unmet], "')"), collapse = ", ")
      )
    }
  }else { # define methods to calculate if method == "all_methods"
    # get user-specified methods
    user_spec <- method[method != "all_methods"]
    # list all built-in methods
    names <- list(
      c("CI_Hegyi", "CI_RK1", "CI_RK2"),
      c("CI_Braathe", "CI_RK3", "CI_RK4"),
      c("CI_size")
    )
    # get the methods that can be calculated with the available inventory
    # user specified methods
    method <- c(
      unlist(names[c("dbh", "height", "size") %in% vars]),
      user_spec)
  }
  # return validated methods vector
  return(method)
}


# Define printing method for target_pc objects:
#' @rdname compete_inv
#' @format NULL
#' @usage NULL
#' @export
print.compete_inv <- function(x, digits = 3, topn = 3, ...){
  # get description of target source from lookup table
  target <- as.character(
    c(inventory = "second inventory",
      character = "character vector",
      logical   = "logical vector",
      exclude_edge = "excluding edge",
      buff_edge = "buffer around edge"
    )[ attr(x, "target_type")]
  )
  # print header
  cat("---------------------------------------------------------------------",
      "\n'compete_inv' class inventory with distance-based competition indices",
      "\nCollection of data for", nrow(x),"target and",
      nrow(attr(x, "edge_trees")), "edge trees.",
      "\nSource of target trees:",target, "\t Search radius:",
      attr(x, "radius"),
      "\n---------------------------------------------------------------------\n"
  )
  # print data.table with trees
  .print_as_dt(x, digits = digits, topn = topn, ...)
  # return object invisibly
  invisible(x)
}



