#' List of distance-based competition indices
#' @description list of distance-based competition index functions for the use
#' with [compete_inv()].
#'
#' @param target single row `forest_inv` class data.table object containing all
#'   the variables needed to calculate the corresponding competition index for
#'   the target tree (i.e. ´x´, ´y´, as well as ´dbh´, ´height´, ´size´ and/or
#'   other size-related attributes).
#' @param inv `forest_inv` class data.table object containing all the variables
#'   needed to calculate the corresponding competition index for all neighbor
#'   trees in the search radius of the target tree (i.e. ´x´, ´y´, as well as
#'   ´dbh´, ´height´, ´size´ and/or other size-related attributes), that
#'   moreover includes their distance to the target tree in m (`dist`).
#'
#' @details `TreeCompR` provides a number of distance-based competition indices
#'   for the use with [compete_inv()].
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
#' ## User-specified competition indices
#'  The creation of new user-specified competition indices is easily possible,
#'   the only requirement is that they take two argument, `target` (a single-row
#'   `forest_inv` class data.table with data for the target tree) and `inv` (a
#'   multi-row `forest_inv` class data.table with data for the neighbor trees),
#'   and return a single numeric value. This function will then be called
#'   internally for each tree and its neighborhood inside `compete_inv()`.
#'   For example, a working implementation of the classical Hegyi index would
#'   look like this:
#' `CI_Hegyi <- function(target, inv) sum(inv$dbh / (target$dbh * inv$dist))`
#'
#'   It is advisable to add checks to ensure the function does the right thing
#'   when it gets passed on empty datasets (e.g. because no trees are in the
#'   neighborhood of a tree) because `sum(NULL)` evaluates to `0` which may not
#'   always be intended.
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
#'
#'
#' @name competition_indices
#' @rdname competition_indices
#'
#' @return A single numeric value (the competition index for the target tree).
#'
#' @seealso
#'   [read_inv()] to read forest inventory data,
#'   [define_target()] for designating target trees,
#'   [compete_inv()] for computing tree competition from inventory data,
#'   [plot_target()] to plot target tree positions in `target_inv` and
#'   `compete_inv` objects.
#'
#' @examples
#' \dontrun{
#'   # read data from all trees in a plot
#'   data <- read_inv("path/to/data.csv")
#'
#'   # compute the Hegyi for the first tree in the dataset, assuming that all
#'   # others were in its search radius
#'   CI_Hegyi(data[1,], data[-1, ])
#' }
NULL


#' @rdname competition_indices
#' @export
CI_Hegyi <- function(target, inv){
  if(!("dbh" %in% names(target)) | !("dbh" %in% names(inv))){
    stop("Hegyi index can only be calculated when 'dbh' is available.")
  }
  sum(inv$dbh / (target$dbh * inv$dist))
}

#' @rdname competition_indices
#' @export
CI_Braathe <- function(target, inv){
  if(!("height" %in% names(target)) | !("height" %in% names(inv))){
    stop("Braathe index can only be calculated when 'height' is available.")
  }
  sum(inv$height / (target$height * inv$dist))
}

#' @rdname competition_indices
#' @export
CI_size <- function(target, inv){
  if(!("size" %in% names(target)) | !("size" %in% names(inv))){
    stop(
      .wr("Generic size-based Hegyi-type competition index can",
          "only be calculated when 'height' is available.")
    )
  }
  sum(inv$size / (target$size * inv$dist))
}


#' @rdname competition_indices
#' @export
CI_RK1 <- function(target, inv){
  if(!("dbh" %in% names(target)) | !("dbh" %in% names(inv))){
    stop("RK1 index can only be calculated when 'dbh' is available.")
  }
  sum(atan(inv$dbh / inv$dist))
}
#' @rdname competition_indices
#' @export
CI_RK2 <- function(target, inv){
  if(!("dbh" %in% names(target)) | !("dbh" %in% names(inv))){
   stop("RK2 index can only be calculated when 'dbh' is available.")
  }
  sum((inv$dbh / target$dbh) * atan(inv$dbh / inv$dist))
}

#' @rdname competition_indices
#' @export
CI_RK3 <- function(target, inv){
  if(!("height" %in% names(target)) | !("height" %in% names(inv))){
    stop("RK3 index can only be calculated when 'height' is available.")
  }
  sum(  atan(inv$height[inv$height > target$height] /
               inv$dist [inv$height > target$height]))
}

#' @rdname competition_indices
#' @export
CI_RK4 <- function(target, inv){
  if(!("height" %in% names(target)) | !("height" %in% names(inv))){
    stop("RK4 index can only be calculated when 'height' is available.")
  }
  sum((inv$height / target$height) * atan(inv$height / inv$dist))
}
