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
#'   target trees in the  same format as in the `id` column of `inv`,
#'   2. a vector of class `logical` specifying for each row of `inv` whether
#'   the corresponding tree is a target tree,
#'   3. another object of class `forest_inv` containing the coordinates of the
#'   target trees. In this case, the coordinates are matched against the
#'   coordinates in `inv` and IDs may differ (useful e.g. when target trees
#'   are defined based on GPS coordinates and matched against an airborne laser
#'   scanning dataset).
#'   4. a character vector of length 1 defining the method by which the target
#'   trees should be determined. Allowed are `"buff_edge"` for excluding all
#'   trees that are at least one search radius from the forest edge,
#'   `"exclude_edge"` for only excluding edge trees or `"all"` for including
#'   all trees of the dataset (which is hardly ever a good idea unless all
#'   trees in the entire forest are in the dataset). The standard is
#'   `"buff_edge"`. See below for details.
#' @param radius numeric of length 1, Search radius (in m) around target tree
#'   wherein all neighboring trees are classified as competitors. Only used if
#'   `target_source` is `"buff_edge"` or `"exclude_edge"`. Defaults to NULL.
#' @param tol numeric. Tolerance for the match between tree coordinates in the
#'   forest inventory and target datasets if specified as a second set of
#'   coordinates. If a field measurements (e.g. based on GPS) are used to
#'   identify target trees and the full inventory is from a different data
#'   source (e.g. ALS data), a higher tolerance value may be required to
#'   identify the trees depending on the measurement accuracy. Values of 0 mean
#'   exact matching.
#'
#' @details
#' The input data can either be taken directly from field measurements or
#' derived beforehand from LiDAR point clouds.
#'
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
#'
#' @return object of `c("target_inv", "forest_inv", "data.frame")` with x and y
#'  coordinates of the tree, a unique tree identifier (`id`), at least one
#'  of tree diameter at breast height (`dbh`, in cm) and tree height (`height`,
#'  in m) and a new logical column `target` specifying whether a tree is
#'  considered a target tree.
#' @export
#'
#' @examples
#' \dontrun{
#' # red inventory
#' inv    <- read_inv("path/to/invtable.csv")
#' # target trees defined by a buffer around the plot edges
#' target <- define_target(inv, target_source = "buff_edge", radius = 10)
#' }
#'
define_target <- function(inv, target_source, radius = NULL, tol = 1) {
  # validate class of inventory
  if(!inherits(inv, "forest_inv")){
    stop("Please supply forest inventory data in the forest_inv format",
         "as created with read_inv().")
  }
  # check if forest inventory contains the required variables
  if(ncol(inv) < 4){
    stop("To calculate competition indices, at least one of 'dbh' or 'height'",
         "are required. Please check data structure in 'inv'.")
  }
  # check if radius, buffer threshold and tolerance are valid
  if (!is.null(radius)) {
    if (!(inherits(radius, "numeric") & length(radius) == 1))
      stop("'radius' should be a numeric of length 1.")}
  if (!(inherits(tol, "numeric") & length(tol) == 1))
    stop("'tol' should be a numeric of length 1.")

  # set flag for spatial methods
  spatial <- FALSE
  # handle different cases for target_source
  if (is.logical(target_source)){
    # handle logical vector
    if (length(target_source) == nrow(inv)) {
      # define target
      inv$target <- target_source
      # keep information about source
      target_type <- "logical"
    } else {
      stop("If 'target_source' is a logical vector, its length has to match ",
           "the number of rows of 'inv.'")
    }
  } else{
    if (is.character(target_source)){
      # handle characters
      if (length(target_source) == 1){
        if (target_source %in% c("buff_edge", "exclude_edge")) {
          # set flag for spatial methods
          spatial <- TRUE
          # test if radius is specified
          if (is.null(radius)) stop("'radius' is required for ",
                                    "methods 'buff_edge' and 'exclude_edge'.")
          # compute target trees from spatial arrangement using internal fun
          inv <- .spatial_comp(inv, type = target_source,
                               radius = radius)
          # keep information about source
          target_type <- target_source
        } else {
          if (target_source == "all"){
            # define all trees as target trees and send a warning
            inv$target <- TRUE
            # keep information about source
            target_type <- "all trees"
            warning(
              "Defining all trees as target trees is rarely a good idea.",
              " Unless your forest inventory contains all trees in the",
              " forest, this will lead to strong edge effects. Please make",
              " sure that this is really what you want to do.")
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
        # keep information about source
        target_type <- "character"
      }
    } else {
      if (inherits(target_source, "forest_inv")){
        # handle second inventory
        # get matching coordinates
        closest <- .closest(inv[, c("x", "y")],
                            target_source[, c("x", "y")],
                            tol = tol)
        # check for target trees missing within tolerance
        if (any(is.na(closest))){
          warning("No matching coordinates found for the following ",
                  "target tree(s):\n", paste(target_source$id, sep = ", "))      }
        # get target trees
        inv$target <- inv$id %in% inv$id[na.omit(closest)]
        # carry over ID
        inv$target_id <- NA
        inv$target_id[na.omit(closest)] <- target_source$id[!is.na(closest)]
        # keep information about source
        target_type <- "inventory"
      }
    }
  }
  # check if the selection has resulted in any target trees
  if (!any(inv$target)) warning(
    "No target trees have been found with the provided specifications.")
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
#' @param inv object of class `target_inv` (as created with [define_target()]).
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
#' @export
#'
#' @examples
#' \dontrun{
#' x
#' }
#'
plot_target <- function(inv, radius = NULL) {
  # if provided with compete_inv object, get corresponding target inventory
  # and search radius
  if(inherits(inv, "compete_inv")){
    inv    <- inv$inventory
    radius <- inv$radius
  }
  # else try if a spatial radius was defined
  if (is.null(radius)){
    if(attr(inv, "target_type") %in% c("exclude_edge", "buff_edge")) {
      radius <- attr(inv, "spatial_radius")
    } else{ # if not, stop with an error
      stop("'radius' has to be defined for plotting.")
    }
  }
  # get original graphics parameters
  op <- par(c("mfrow", "mar"))
  # reset original parameters if function breaks
  on.exit(par(op))
  # set graphical parameters for plot
  par(mfrow = c(1, 1), mar = c(0,0,2,0))
  # convert inventory to sf objects
  center <- sf::st_multipoint(
    as.matrix(inv[inv$target,c("x", "y")]))
  border <-  sf::st_multipoint(
    as.matrix(inv[!inv$target,c("x", "y")]))

  # plot points and buffer
  plot(sf::st_buffer(center, dist = radius),
       col = "grey90", lty = 0,
       xlim = range(inv$x) + c(-radius, radius + diff(range(inv$x))),
       ylim = range(inv$y) +  c(-radius, radius),
       main = "Plot of tree positions")
  plot(center, pch = 16, add = TRUE)
  plot(border, pch = 1, add = TRUE)

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
          cbind(inv$x, inv$y),
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
  if (nrow(leg) == 3){ # plot without lty when no lines are involved
    with(leg, legend(x = max(inv$x) + diff(range(inv$x))/3,
                     y = max(range(inv$y)),
                     pch = pch,  col = col, pt.cex = cex,
                     legend = label, bty = "n", y.intersp = 1.5))
  } else{ # plot with lty if lines are involved
    with(leg, legend(x = max(inv$x) + diff(range(inv$x))/3,
                     y = max(range(inv$y)),
                     pch = pch, lty = lty, col = col, pt.cex = cex,
                     legend = label, bty = "n", y.intersp = 1.5))
  }
  # reset graphical parameters
  par(op)
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
    closest <- inv$id[.closest(inv[,2:3], conc[-1,], 0.1)]
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
.closest <- function(xy1, xy2, tol){
  # get best matches
  out <- apply(xy2, 1, function(x){
    d <- sqrt((xy1[,1] - x[1])^2 + (xy1[,2] - x[2])^2)
    if (!any(d <= tol)) return(NA) else
      if (sum(d == min(d)) > 1)
        stop("More than one point is any equally good match.")
    else(which.min(d))
  })
  # check if there are duplicates
  if (any(duplicated(out)))
    stop("More than one point is any equally good match.")
  return(out)
}


# Define printing method for target_pc objects:
#' @rdname define_target
#' @format NULL
#' @usage NULL
#' @export
print.target_inv <- function(x, ...){
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
  cat("---------------------------------------------------------------",
      "\n'target_inv' class inventory dataset with defined target trees:",
      "\ncollection of", nrow(x),"observations",
      "\nSource of target trees:",target,
      "\n---------------------------------------------------------------\n"
  )
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


