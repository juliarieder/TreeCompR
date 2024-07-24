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
#' @param kmax integer of length 1. maximum number of nearest neighbors to
#'   consider for determining the neighbor trees of each tree. Lower values
#'   speed up computation, which is likely not an issue unlike your inventory
#'   contains tens of thousands of trees, but is very relevant for big datasets.
#'   Defaults to 999, which should be reasonable in almost all forest settings,
#'   but has to be adjusted when there are warnings (especially for large search
#'   radii and/or stands with small trees). If `kmax` is larger than the number
#'   of trees in the plot, the latter is passed on as `k` to [nabor::knn()].
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
#'   `compete_inv()` allows to define target trees in a number of different
#'   ways based on the function [define_target()] that is called internally.
#'
#'   ## Available competition indices
#'   All competition indices are calculated based on the subset of trees that
#'   are within the search radius of the target tree.
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
#'  _Generic size-based Hegyi-type competition index_
#'  * CI_size based on Hegyi (1974), but with a user-specified size-related
#'   variable (\eqn{s_i}: size for neighbor tree \eqn{i}, \eqn{s}: size of the
#'   target tree):
#'   \eqn{CI_{size} = \sum_{i=1}^{n} s_{i} / (s \cdot dist_{i})}
#'
#'  Further indices can be user-specified by developing a corresponding
#'  function (see [competition_indices] for details).
#'
#'  To efficiently deal with large inventory datasets (as can be expected from
#'  ALS sources), the neighbors within the search radius are computed with
#'  [nabor::knn()], which is based on an efficient C++ implementation
#'  of the k-nearest neighbor algorithm from
#'  [libnabo](https://github.com/norlab-ulaval/libnabo). For this, it is
#'  required to specify a maximum number of neighbors to take into account
#'  (`kmax`, which is passed to [nabor::knn()] as `k`) that should be chosen as
#'  low as possible, but high enough to ensure that it is sufficiently larger
#'  than the maximum number of trees expected in the search radius. When
#'  working with small datasets (from classical forest inventories or MLS/TLS
#'  sources), this will likely not matter, but when working with large ALS
#'  datasets this implementation can speed up calculation by several orders of
#'  magnitude.
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
#' # get target trees
#' targets4 <- readr::read_csv("data/inventory4.csv") %>%
#'   dplyr::filter(plot_id == "Plot 1") %>%
#'   read_inv(dbh = diam, verbose = FALSE) %>%
#'   define_target(target_source = "buff_edge", radius = 8)
#'
#' # compute Hegyi index based on existing target_inv object
#' CI1 <- compete_inv(inv_source = targets4, radius = 8,
#'                    method = "CI_Hegyi")
#'
#' # compute Hegyi based on a file source
#' CI2 <- compete_inv(
#'   inv_source = "data/inventory5.csv",
#'   target_source = "buff_edge",
#'   radius = 12,
#'   method = "CI_Hegyi",
#'   x = Koord_x,
#'   y = Koord_y,
#'   id = Baumname,
#'   dbh = Durchmesser,
#'   sep = ";",
#'   dec = ","
#' )
#'
#' # compute all built-in indices that are possible with a dataset
#' (CI3 <- compete_inv(inv_source = targets4,
#'                     radius = 8, method = "all_methods"))
#'
#' # new implementation of Hegyi competition index
#' CI_Hegyi_new <- function(target, neigh)
#'   sum(neigh$dbh / (target$dbh * neigh$dist))
#'
#' # R1 index only for trees taller than the target tree
#' CI_RK1_tall <- function(target, neigh)
#'   CI_RK1(target, neigh[neigh$height > target$height, ])
#'
#' # get output for new indices
#' compete_inv(inv_source = targets4, radius = 8,
#'             method = c("CI_Hegyi", "CI_Hegyi_new", "CI_RK1_tall"))
#'
#' # partial Hegyi index for oak
#' CI_Hegyi_QURO <- function(target, neigh)
#'   CI_Hegyi(target, neigh[neigh$species == "Quercus robur", ])
#' # partial Hegyi index for hornbeam
#' CI_Hegyi_CABE <- function(target, neigh)
#'   CI_Hegyi(target, neigh[neigh$species == "Carpinus betulus", ])
#' # partial Hegyi index for ash
#' CI_Hegyi_FREC <- function(target, neigh)
#'   CI_Hegyi(target, neigh[neigh$species == "Fraxinus excelsior", ])
#'
#' # load dataset with species
#' inv_species <- read_inv("data/inventory6.csv", keep_rest = TRUE)
#' inv_species
#'
#' # compute species-decomposed Hegyi indices
#' comp_species <- compete_inv(
#'   inv_source = inv_species, target_source = "buff_edge", radius = 12,
#'
#' }
compete_inv <- function(inv_source, target_source = "buff_edge", radius,
                        method = "all_methods",
                        x = NULL, y = NULL,
                        dbh = NULL, height = NULL, size = NULL, id = NULL,
                        dbh_unit = c("cm", "m", "mm"),
                        height_unit = c("m", "cm", "mm"),
                        keep_rest = FALSE,
                        verbose = TRUE,
                        tol = 1, kmax = 999,
                        ...) {
  # validate vector of specified CI methods
  method <- .validate_method(method)

  # if target trees are already defined in inv_source, bypass the following
  # steps and directly compute competition indices
  if(!inherits(inv_source, "target_inv")){
    # read and validate forest inventory dataset
    inv <- read_inv(
      inv_source,  x = !!enquo(x), y = !!enquo(y), dbh = !!enquo(dbh),
      height = !!enquo(height), size = !!enquo(size), id = !!enquo(id),
      dbh_unit = dbh_unit, height_unit = height_unit,
      keep_rest = keep_rest, verbose = verbose, ...)

    # check if target_source is a length 1 character
    if(inherits(target_source, "character") && length(target_source) == 1){
      # if it is a valid file path, read and validate target tree dataset
      if (file.exists(target_source)){
        target_source <- read_inv(
          target_source, x = !!enquo(x), y = !!enquo(y), id = !!enquo(id),
          verbose = verbose,  ...)
        # else it is passed to define_target as a character string
      }
    }
    # validate target source in the same way if it is an unformatted table
    if(inherits(target_source, "data.frame") &&
       !inherits(target_source, "forest_inv")){
      target_source <- read_inv(
        target_source,  x = !!enquo(x), y = !!enquo(y), id = !!enquo(id),
        verbose = verbose, ...)
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

  # check if method requirements are met
  method <- .validate_reqs(method, names(inv))

  # get subset of target trees
  targets <- inv[inv$target, -"target"]
  # compute radius
  nn <- nabor::knn(inv[, c("x", "y")], targets[, c("x", "y")],
                   k = min(kmax, nrow(inv)), radius = radius)
  # warn if kmax is too low
  if (kmax < nrow(inv)){
    # only makes sense if kmax is more than the total number of trees
    if (any(nn$nn.idx[,kmax] > 0)){
      warning(
        .wr("Maximum number of target trees reached for",
            sum(nn$nn.idx[,kmax] > 0), "out of", nrow(targets),
            "target trees. Computed Competition indexes are not reliable.",
            "Please increase kmax.")
      )
    }
  }
  # prepare output
  out <- targets
  # create empty vectors for results
  for (meth in method){
    out[[meth]] <- NA_real_
  }
  # loop over all target trees
  for (i in 1:nrow(targets)){
    # get target tree
    target <- targets[i, ]
    # get neighborhood
    ids <- nn$nn.idx[i,-1] # remove first tree: target tree
    neigh   <- inv[ids[ids>0],]
    neigh$dist <-  nn$nn.dists[i, 1:nrow(neigh) + 1]

    # loop over methods to compute indices
    for(meth in method){
      out[[meth]][i] <- do.call(meth, args = list(target, neigh))
    }
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
  class(out) <- c("compete_inv", "forest_inv", "data.table", "data.frame")

  # update column order
  # standard columns
  first <- base::intersect(
    c("id", "x", "y", "target_id", "dbh", "height", "size"), names(out))
  # competition index columns
  CIs <- grep("CI_", names(out), value = TRUE)
  # all other columns
  rest <- base::setdiff(names(out), c(first, CIs))
  # reorder output
  out <- subset(out, select = c(first, CIs, rest))

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
      if (!exists(meth, mode = "function", where = .GlobalEnv)) stop(
        .wr("Invalid competition index function detected:", meth,
            "is not a  function.")
      )
      # check if it has the correct formal arguments
      if (!identical(names(formals(meth)), c("target", "neigh"))){
        stop(
          .wr("Invalid competition index function detected:",
              paste0(meth, "()."),
              "Competition index functions take 'target' and 'neigh' as",
              "arguments and return a single value: 'function(target, neigh)'")
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
print.compete_inv <- function(x, digits = 3, topn = 3, nrows = 8, ...){
  # get description of target source from lookup table
  target <- as.character(
    c(inventory = "tree coordinates",
      character = "tree IDs",
      logical   = "logical selection",
      exclude_edge = "'exclude_edge'",
      buff_edge = "'buff_edge'",
      all_trees = "'all_trees'"
    )[ attr(x, "target_type")]
  )
  if (target == "'buff_edge'")
    target <- paste0(target, " (", attr(x, "spatial_radius"), " m)")

  # prepare header
  header <- paste0(
      "'compete_inv' class distance-based competition index dataset\n",
      "No. of target trees: ", nrow(x),"\t Source inventory size: ",
      nrow(x) + nrow(attr(x, "edge_trees")),
      " trees\nTarget source: ", target,
      "\t Search radius: ", attr(x, "radius"), " m")

  # print data.table with trees
  .print_as_dt(x, digits = digits, topn = topn,
               nrows = nrows, header = header, ...)
  # return object invisibly
  invisible(x)
}


# Define rbind method for forest_inv objects:
#' @rdname compete_inv
#' @format NULL
#' @usage NULL
#' @export
rbind.compete_inv <- function(
    ..., use.names = TRUE, fill = FALSE, idcol = NULL){
  .rbind_with_class(..., use.names = use.names, fill = fill, idcol = idcol)
}


