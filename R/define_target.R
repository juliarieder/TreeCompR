#' Define target trees in forest inventory data
#' @description
#' `define_target()` takes a `forest_inv` dataset and returns an updated forest
#' inventory with highlighted target trees for analysis with `compete_inv`.
#' Target trees can be manually specified by tree ID, imported from a different
#' dataset or identified automatically by excluding edge trees from their
#' coordinates.
#'
#' @param inv object of class `forest_inv` as created with [read_inv()].
#' @param target_source one of the following:
#'   1. a vector of class `"character"` containing the tree IDs identifying the
#'   target trees in the  same format as in the `id` column of `inv`.
#'   2. a vector of class `logical` specifying for each row of `inv` whether
#'   the corresponding tree is a target tree.
#'   3. another object of class `forest_inv` containing the coordinates of the
#'   target trees. In this case, the coordinates are matched against the
#'   coordinates in `inv` and IDs may differ (useful e.g. when target trees
#'   are defined based on GPS coordinates and matched against an airborne laser
#'   scanning dataset).
#'   4. a character vector of length 1 defining the method by which the target
#'   trees should be determined. Allowed are `"buff_edge"` for excluding all
#'   trees that are at least one search radius from the forest edge,
#'   `"exclude_edge"` for only excluding edge trees or `"all_trees"` for
#'   including all trees of the dataset (which is hardly ever a good idea unless
#'   all trees in the entire forest are in the dataset). The standard is
#'   `"buff_edge"`. See below for details.
#' @param radius numeric of length 1, Search radius (in m) around target tree
#'   wherein all neighboring trees are classified as competitors. Only used if
#'   `target_source` is `"buff_edge"`, `"exclude_edge"` or if
#'   `crop_to_target = TRUE`. Defaults to 10.
#' @param tol numeric of length 1. Only used when `target_source` is an
#'   inventory with a second set of coordinates. Tolerance for the match between
#'   tree coordinates in the forest inventory and target datasets if specified
#'    as a second set of coordinates.
#'    If (GPS-based) field measurements of coordinate are used to
#'   identify target trees and the full inventory is from a different data
#'   source (e.g. ALS data), a the tolerance value may have to be adjusted to
#'   identify the trees depending on the GPS accuracy. Values of 0 mean
#'   exact matching. Defaults to 1 (match within a 1 m buffer).
#' @param crop_to_target logical of length 1. Should the inventory be limited
#'   to the extent of the target coordinates? If TRUE, the extent of `inv` will
#'   be cropped to the extent of `forest_inv` \eqn{\pm} `radius + tol` to
#'   reduce computational load in later steps. Defaults to FALSE.
#' @param verbose logical of length 1. Should information about progress be
#'   printed? Defaults to TRUE.
#'
#' @details `define_target()` can be used to select target trees from a
#'   `forest_inv` object either by manually specifying them as a character
#'   vector with tree IDs, as a logical vector that specifies for each tree in
#'   the inventory whether or not it is treated as a target tree, a separate
#'   set of (approximate) coordinates of the target trees that is matched
#'   against the original inventory, or a character string describing how to
#'   choose the target trees based on their spatial arrangement.
#'
#'   When the target is defined by a second set of coordinates, these
#'   coordinates will then be matched against the inventory data. IDs are
#'   ignored in this case and  matching is based only on the closest trees with
#'   in a buffer of `tol` m  (the default is `tol = 1`: matching within 1 m).
#'   All further size-related variables in the second set of coordinates are
#'   ignored as well to make sure that in later steps competition indices will
#'   be computed with data from the same data source.
#'   When different target trees are matched to the same tree in the inventory,
#'   or when two trees in the inventory have the same distance to a target
#'   tree within 5 cm, the function fails with an error. If two inventory trees
#'   are within the specified tolerance and the difference is larger, the
#'   function proceeds with a warning.
#'   The intended use case for determining target trees in this way is to
#'   compute tree competition from ALS data based on GPS coordinates of single
#'   trees in studies that are based on single tree rather than plot-level
#'   data, which creates a need for different data sources to compute
#'   competition.
#'
#'   The methods `target_source = "buff_edge"` and
#'   `target_source = "exclude_edge"` are intended for cases where it is desired
#'   to compute valid competition indices for as many trees as possible while
#'   avoiding edge effects. While it is possible to designate all trees in the
#'   dataset as target tree by setting `target_source = "all_trees"`, this is
#'   not a good idea in the majority of cases: unless your dataset contains all
#'   trees in the forest (possible for ALS-based data, but very unlikely for
#'   data based on classical inventory methods and TLS/MLS), there will be very
#'   extreme edge effects at the outer edge of the extent of the covered trees
#'   resulting in strongly underestimated competition for edge trees.
#'   `target_source = "buff_edge"` excludes all trees who are less than one
#'   search radius (`radius`) away from the plot border (approximated by a
#'   concave hull based on [concaveman::concaveman()] using a length threshold
#'   of 2 times the desired search radius) and hence guarantees to
#'   only include trees that can obtain valid competition index values for that
#'   search radius. `target_source = "exclude_edge"` only removes the edge trees
#'   and hence is less restrictive, but more prone to edge effects.
#'
#'   Do not use `target_source = "all_trees"` unless you know exactly what you
#'   are doing!
#'
#' @return object of class `target_inv` (inherits from `forest_inv`): a
#'  modified data.table with the x and y coordinates of the tree, a unique tree
#'  identifier (`id`), at least one of tree diameter at breast height (`dbh`,
#'  in cm) and tree height (`height`,in m) and a new logical column `target`
#'   specifying whether a tree is defined as a target tree.
#'
#' @seealso [read_inv()] to read forest inventory data,
#'   [compete_inv()] for computing tree competition from inventory data,
#'   [competition_indices] for a list of available indices,
#'   [plot_target()] to plot target tree positions in `target_inv` and
#'   `compete_inv` objects. For visualized examples, see [competition
#'   inventory](https://juliarieder.github.io/TreeCompR/articles/competition-inventory.html#designating-target-trees-with-define_target)
#' @export
#'
#' @examples
#' \dontrun{
#' # read inventory
#' inventory1 <- read_inv(inv_source = "data/inventory1.csv", dbh_unit = "m")
#'
#' # designate target trees based on character vector with tree ids
#' targets1 <- define_target(
#'   inv = inventory1,
#'   target_source = c("FASY-43-24", "FASY-43-27", "FASY-43-30")
#' )
#'
#' # designate target trees based on logical vector
#' targets2 <- define_target(
#'   inv = inventory1,
#'   target_source = grepl("FASY", inventory1$id)
#' )
#'
#' # designate target trees based on GPS coordinates
#' # read target tree positions
#' target_pos <- read_inv("data/target_tree_gps.csv",
#'                        x = gps_x, y = gps_y, verbose = FALSE)
#'
#' # read inventory
#' inventory4 <- readr::read_csv("data/inventory4.csv") %>%
#'   dplyr::filter(plot_id == "Plot 1") %>%
#'   read_inv(dbh = diam, verbose = FALSE)
#'
#' # define target trees
#' targets3 <- define_target(
#'   inv = inventory4,
#'   target_source = target_pos,
#'   tol = 1 # match within 1 m accuracy
#' )
#'
#' # designate target trees with a 10 m buffer to the plot border
#' targets4 <- define_target(
#'   inv = inventory4,
#'   target_source = "buff_edge",
#'   radius = 8)
#'
#' # designate target trees by only excluding the plot border
#' targets5 <- define_target(
#'   inv = inventory4,
#'   target_source = "exclude_edge",
#'   radius = 8
#' )
#'
#' # designate all trees as target trees
#' targets6 <- define_target(
#'   inv = inventory4,
#'   target_source = "all_trees")
#' }
#'
define_target <- function(inv, target_source = "buff_edge", radius = 10,
                          tol = 1, crop_to_target = FALSE, verbose = TRUE) {
  # validate class of inventory
  if(!inherits(inv, "forest_inv")){
    stop(.wr("Please supply forest inventory data in the forest_inv format",
             "as created with read_inv()."))
  }
  # check if forest inventory contains the required variables
  if(ncol(inv) < 4){
    stop(
      .wr("To calculate competition indices, at least one of 'dbh' or 'height'",
          "are required. Please check data structure in 'inv'.")
    )
  }
  # check if radius, buffer threshold and tolerance are valid
  if (!(inherits(radius, "numeric") & length(radius) == 1))
    stop("'radius' should be a numeric of length 1.")
  if (!(inherits(tol, "numeric") & length(tol) == 1))
    stop("'tol' should be a numeric of length 1.")

  # set flag for spatial methods
  spatial <- FALSE

  # handle different cases for target_source
  if (inherits(target_source, "target_inv")){
    # if a target_inv file was supplied as a target_source, just carry over the
    # target trees and send a message
    inv$target <- inv$id %in% target_source[target_source$target,]$id
    message(
      .wr("target_source already is of class target_inv.",
          "Target tree IDs specified in target_source are kept.")
    )
    # keep information about source
    target_type <- "inventory"
  } else {
    if (is.logical(target_source)){
      # handle logical vector
      if (length(target_source) == nrow(inv)) {
        # define target
        inv$target <- target_source
        # keep information about source
        target_type <- "logical"
      } else {
        stop(.wr(
          "If 'target_source' is a logical vector, its length has to match ",
          "the number of rows of 'inv.'")
        )
      }
    } else{
      if (is.character(target_source)){
        # handle characters
        if (length(target_source) == 1){
          if (target_source %in% c("buff_edge", "exclude_edge")) {
            # set flag for spatial methods
            spatial <- TRUE
            # compute target trees from spatial arrangement using internal fun
            inv <- .spatial_comp(inv, type = target_source,
                                 radius = radius)
            # keep information about source
            target_type <- target_source
          } else {
            if (target_source == "all_trees"){
              # define all trees as target trees
              inv$target <- TRUE
              # keep information about source
              target_type <- target_source
            } else {
              # set single target tree
              inv$target <- inv$id %in% target_source
              # keep information about source
              target_type <- "character"
            }
          }
        } else {
          # set multiple target trees
          inv$target <- inv$id %in% target_source
          # warn if trees were not found
          if (any(!target_source %in% inv$id)){
            warning(
              "The following target tree id(s) are not in the inventory:\n",
              paste(target_source[!target_source %in% inv$id], collapse = ", ")
            )
          }
          # keep information about source
          target_type <- "character"
        }
      } else {
        if (inherits(target_source, "forest_inv")){
          # handle second inventory
          # get matching coordinates
          closest <- .closest(
            target = target_source[, c("x", "y")],
            inv = inv[, c("x", "y")],
            tol = tol)
          # check for target trees missing within tolerance
          if (any(is.na(closest))){
            warning(
              "No matching coordinates found for the following ",
              "target tree(s):\n",
              paste(target_source$id[is.na(closest)], collapse = ", "),
              "\n. Revise coordinates and consider increasing 'tol'."
            )
          }
          # get target trees
          inv$target <- inv$id %in% inv$id[stats::na.omit(closest)]
          # carry over ID
          inv$target_id <- NA
          inv$target_id[stats::na.omit(closest)] <-
            target_source$id[!is.na(closest)]
          # keep information about source
          target_type <- "inventory"
        }
      }
    }
  }
  # check if the selection has resulted in any target trees
  if (!any(inv$target)){
    warning("No target trees have been found with the provided specifications.")
  }
  # check if the selection has resulted in any edge trees
  if (!any(!inv$target)){
    warning(
      .wr(
        "Defining all trees as target trees is rarely a good idea.",
        "Unless your forest inventory contains all trees in the",
        "forest, this will lead to strong edge effects. Please make",
        "sure that this is really what you want to do.")
    )
  }
  # remove trees outside range of the target trees if crop_to_target = TRUE
  if (crop_to_target){
    # set flag for spatial methods
    spatial <- TRUE
    # test if there are trees outside the relevant range
    inside <-
      inv$x %inrange% (range(inv[inv$target,"x"]) + c(-1, +1) * (radius + tol)) &
      inv$y %inrange% (range(inv[inv$target,]$y) + c(-1, +1) * (radius + tol))
    # message if dataset was modified
    if (any(!inside) && verbose) message(
      .wr(
        sum(!inside),
        "trees outside the competitive zone around the target",
        " trees were removed.", sum(inside), "trees remain.")
    )
    # filter out trees outside of radius
    inv <- inv[inside, ]
  }
  # update class
  class(inv) <- c("target_inv", class(inv))
  # set attribute for target type
  attr(inv, "target_type") <- target_type
  # if a radius-dependent method was used, add as an attribute
  if (spatial) {
    attr(inv, "spatial_radius") <- radius
  }
  # return inventory
  return(inv)
}


#' Plot selection of target trees
#' @description
#' `plot_target()` can be used with  a `target_inv` dataset or the output
#' of the `compete_inv` function to inspect the spatial positions of the
#' target trees.
#'
#' @param inv object of class `compete_inv` or `target_inv`.
#' @param radius numeric of length 1, Search radius (in m) around target tree
#'   wherein all neighboring trees are classified as competitors. Only needed
#'   for `target_inv` objects in methods that are not radius dependent (i.e.,
#'   in cases where `target_source` defined by a character or logical vector or
#'   in cases where `target_source = "all_trees"`).
#'
#' @details The function creates a plot of the trees in the forest inventory
#'   dataset where the target trees and the surrounding search radii are
#'   highlighted. If they were created with `target_source = "buff_edge"` or
#'   `target_source = "exclude_edge"`, the estimated plot margin (and, in case
#'   of `"buff_edge"`, also the buffer to the margin) are added as a polygon.
#'
#'   This function is meant as a visual inspection tool for checking the
#'   validity of the choice of target trees.
#'
#' @return A plot of the spatial arrangement of target trees.
#'
#' @seealso [read_inv()] to read forest inventory data,
#'   [define_target()] for designating target trees,
#'   [compete_inv()] for computing tree competition from inventory data,
#'   [competition_indices] for a list of available indices. For visualized examples,
#'   see [competition inventory](https://juliarieder.github.io/TreeCompR/articles/competition-inventory.html#designating-target-trees-with-define_target)
#' @export
#'
#' @examples
#' \dontrun{
#' # read inventory dataset
#' inv <- read_inv("data/inventory.csv")
#'
#' # lot neighborhood for 'target_inv' object
#' target <- target_inv(
#'   inv, target_source = c("Tree 1", "Tree 2", "Tree 7"))
#' plot_target(target, radius = 10)
#'
#' # plot neighborhood for 'compete_inv' object
#' comp <- compete_inv(inv, "buff_edge", radius = 10)
#' plot_target(comp)
#' }
#'
plot_target <- function(inv, radius = NULL) {
  # prepare placeholders for center and border trees
  center <- border <- NULL

  # if provided with compete_inv object, get corresponding target inventory
  # and search radius
  if(inherits(inv, "compete_inv")){
    # get radius from attributes
    radius <- attr(inv, "radius")
    # get full positions from attributes (with case handling for
    # target_trees == "all_trees" + other cases without border or center trees)
    if (nrow(attr(inv, "edge_trees")) > 0) {
      center <- sf::st_multipoint(attr(inv, "target_trees"))
    }
    if (nrow(attr(inv, "edge_trees")) > 0) {
      border <- sf::st_multipoint(attr(inv, "edge_trees"))
    }
  } else {
    # if it is a target_inv object, get coordinates by filtering by col target
    if (any(inv$target)){
      center <- sf::st_multipoint(
        as.matrix(inv[inv$target,c("x", "y")]))
    }
    if (any(!inv$target)){
      border <-  sf::st_multipoint(
        as.matrix(inv[!inv$target,c("x", "y")]))
    }
    # check if a spatial radius was defined
    if (is.null(radius)){
      if(attr(inv, "target_type") %in% c("exclude_edge", "buff_edge")){
        radius <- attr(inv, "spatial_radius")
      } else{ # if not, stop with an error
        stop("'radius' has to be defined for plotting.")
      }
    }
  }
  # get full coordinates for both data sources (for plot dimensions)
  coords <- as.matrix(rbind(border, center))
  if (is.null(border)){
    warning("All trees in the dataset are defined as as target trees.")
  }
  if (is.null(center)){
    warning("No target trees have been defined.")
  }

  # get original graphics parameters
  op <- graphics::par(c("mfrow", "mar"))
  # reset original parameters if function breaks
  on.exit(graphics::par(op))
  # set graphical parameters for plot
  graphics::par(mar = c(0,0,0,0))
  graphics::layout(
    mat = matrix(c(1,2,1,3), nrow = 2),
    widths = c(1, 0.7), heights = c(1, 9))
  # plot title
  graphics::plot.new()
  graphics::text(x = 0.5, y = 0.5,"Plot of tree positions",
                 cex = 1.5,font = 2)
  # plot points and buffer
  plot(sf::st_buffer(center, dist = radius),
       col = "grey90", lty = 0,
       xlim = range(coords[,1]) + c(-radius, radius),
       ylim = range(coords[,2]) +  c(-radius, radius)
  )
  plot(center, pch = 16, add = TRUE)
  if(!is.null(border)) plot(border, pch = 1, add = TRUE)
  # get subset of legend
  leg <- data.frame(pch = c(16, 1, 16), lty = NA,
                    col = c(1, 1, "grey90"),
                    cex = c(1, 1, 3),
                    label = c("Target trees",
                              "Border trees",
                              "1 search radius \naround target trees"))
  # if spatial method was used, also plot approximate plot borders
  if(attr(inv, "target_type") %in% c("exclude_edge", "buff_edge")) {
    # get concave hull
    conc <-  sf::st_polygon(
      list(
        concaveman::concaveman(
          coords,
          length_threshold = 2 * radius)
      )
    )
    # plot concave hull
    plot(conc, add = TRUE, border = "grey40")
    # update legend
    leg <- rbind(leg, data.frame(
      pch = NA, lty = 1, col = "grey40", cex = 1,
      label = "Estimated plot border"))

    # if a buffer was used, add buffer
    if (attr(inv, "target_type") == "buff_edge"){
      plot(sf::st_buffer(
        conc, dist = -radius, singleSide = TRUE), add = TRUE,
        lty = 2)
      # update legend
      leg <- rbind(leg, data.frame(
        pch = NA, lty = 2, col = "grey40", cex = 1,
        label = "1 search radius \ndistance from plot border"))
    }
  }
  # plot legend
  graphics::plot.new()
  if (nrow(leg) == 3){ # plot without lty when no lines are involved
    with(leg,
         legend(x = "center",
                pch = pch,  col = col, pt.cex = cex,
                legend = label, bty = "n", y.intersp = 1.5))
  } else{ # plot with lty if lines are involved
    with(leg,
         legend(x = "center",
                pch = pch, lty = lty, col = col, pt.cex = cex,
                legend = label, bty = "n", y.intersp = 1.5))
  }
  # reset graphical parameters
  graphics::par(op)
}


#' @keywords internal
#' internal function for defining the competitors for each target tree based
#' on spatial proximity
.spatial_comp <- function(inv, type, radius){
  # get convex hull of dataset
  conc <- concaveman::concaveman(
    cbind(inv$x, inv$y),
    length_threshold = 2 * radius) # minimum distance up to which the
  # concave hull should try to find shapes that extend inwards more
  # strongly - should not be lower than 2 search radii

  # if only edge trees are considered, identify edge trees
  if (type == "exclude_edge"){
    closest <- inv$id[.closest(conc[-1,], inv[,2:3], 0.1)]
    inv$target <- !(inv$id %in% closest)
  } else {
    if(type == "buff_edge"){
      # convert to sf object
      inv_sf <- sf::st_as_sf(inv, coords = c("x", "y"))
      sf::st_agr(inv_sf) <- "constant"
      # Get polygon for concave hull
      conc_sf <- sf::st_polygon(list(conc))
      # Find trees that are safely within the polygon (center)
      #dist = -radius: buffer direction towards polygon center
      buf_conc <- sf::st_buffer(
        conc_sf, dist = -radius, singleSide = TRUE
      )
      # Check for intersections between points and buffer
      inv$target <- inv$id %in% sf::st_intersection(inv_sf, buf_conc)$id
    } else {
      stop("Unknown method for identifying edge trees.")
    }
  }
  # return inventory
  return(inv)
}


#' @keywords internal
#' internal function for identifying the closest point in two set of coordinates
.closest <- function(target, inv, tol){
  # get nearest neighbors with nabor::knn (k = 2 to speed up,
  # more than one triggers an error)
  nn <- nabor::knn(inv, target,  k = 2, radius = tol)
  # check for duplicates in inventory
  if (any(dups <- nn$nn.idx[,2] > 0)) {
    if (any(
      abs(nn$nn.dists[dups,1] - nn$nn.dists[dups, 2]) < 0.05)){
      stop(
        .wr("More than one set of coordinates in the inventory is",
            "an equally good match (within 5 cm difference) for at least one",
            "target tree. Revise coordinates and consider reducing 'tol'.")
      )
    } else{
      warning(
        .wr("More than one set of coordinates in the inventory is",
            "within the desired tolerance for at least one target tree.",
            "Revise coordinates and consider reducing 'tol'.")
      )
    }
  }
  # prepare output
  out <- nn$nn.idx[,1]
  # check for duplicate targets
  if (any(duplicated(out[!out == 0]))){
    stop(
      .wr("More than one target tree has been matched to",
          "at least one coordinate in the inventory.",
          "Revise coordinates and consider reducing 'tol'.")
    )
  }
  # set target trees without matches to NA and return output
  out[out == 0] <- NA
  return(out)
}


# Define printing method for target_pc objects:
#' @rdname define_target
#' @format NULL
#' @usage NULL
#' @export
print.target_inv <- function(x, digits = 3, topn = 3, nrows = 8, ...){
  # get description of target source from lookup table
  target <- as.character(
    c(inventory = "second inventory",
      character = "character vector",
      logical   = "logical vector",
      exclude_edge = "excluding edge",
      buff_edge = "buffer around edge",
      all_trees = "all trees"
    )[ attr(x, "target_type")]
  )
  # get number of data columns
  if (ncol(x) > 4){
    if (ncol(x) > 5) cols <- paste0(" with ", ncol(x) - 4, " data columns")
    else cols <- " with one data column"
  } else cols <- ""
  # prepare header
  header <- paste0(
    "'target_inv' class inventory dataset with target tree definitions",
    "\nCollection of ", nrow(x)," observation",
    ifelse(nrow(x) > 1, "s", ""), cols,
    "\nNo. of target trees: ", sum(x$target),"   \t Target source: ", target)

  # print data.table with trees
  .print_as_dt(x, digits = digits, topn = topn,
               nrows = nrows, header = header, ...)

  # return object invisibly
  invisible(x)
}


# Define rbind method for forest_inv objects:
#' @rdname define_target
#' @format NULL
#' @usage NULL
#' @export
rbind.target_inv <- function(
    ..., use.names = TRUE, fill = FALSE, idcol = NULL){
  .rbind_with_class(..., use.names = use.names, fill = fill, idcol = idcol)
}

