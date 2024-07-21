#' @title Quantify tree competition from point clouds
#' @description Counts the voxels of neighboring trees that intersect a
#'   search cone or search cylinder around the target tree according to Seidel
#'   et al. (2015) and Metz et al. (2013).
#' @param forest_source path to file of neighborhood point cloud or data.frame
#'   or LAS object (see [lidR::LAS-class]) with point cloud data which are
#'   passed on to [read_pc()], or object of class forest_pc as created with
#'   [read_pc()]. The neighborhood can, but does not have to include the target
#'   tree itself, should not be height normalized, and can include ground
#'   points. Coordinates have to be in a Cartesian coordinate system in m.
#'   For paths to source files, the supported formats are are .las, .laz, .ply
#'   as well as all formats accepted by [data.table::fread()] (.csv, .txt, and
#'   others).
#' @param tree_source path to file of the point cloud of the segmented target,
#'   tree or data.frame or LAS object (see [lidR::LAS-class]) with point cloud
#'   data which are passed on to [read_pc()], or object of class forest_pc
#'   as created with [read_pc()].
#'   Coordinates have to be in the same coordinate system as `forest_source`.
#'   For paths to source files, the supported formats are are .las, .laz,
#'   .ply and formats accepted by [data.table::fread()].
#' @param comp_method character string of length 1 with competition method.
#'   Allowed values are "cone" for the cone method, "cylinder" for the cylinder
#'   method or "both" for both methods. Default is the cone method.
#'   See details for computation.
#' @param center_position character string of length 1 with the position used
#'   as the center of the search cone/cylinder. Allowed values are "crown_pos"
#'   for the central point of the crown projected area and "base_pos" for the
#'   stem base position as computed by [tree_pos()]. Default value is "crown_pos".
#' @param tree_name (optional) ID for the tree. If no argument is put, defaults
#'   to the name of the argument provided as `tree_source`.
#' @param cyl_r (optional) only needed when using comp_method "cylinder";
#'   numeric value of cylinder radius in m. Default is 5 m.
#' @param h_cone (optional) only when using comp_method "cone"; numeric value
#'   describing the fraction of the height of the tree where the tip of the
#'   search cone is located. For example, values of 0.5 or 0.6 specify that the
#'   cone opens in 50 or 60 % of target tree's height, respectively. Default is
#'   0.6 as proposed by Seidel et al. (2015).
#' @param z_min integer of length 1 describing the minimum number of points
#'   needed in the lowermost 1 voxel depth Z layer to consider it part of the
#'   target tree. Default is 100. If changing the voxel resolution (`res`) from
#'   the default value of 0.1, different settings may be necessary.  Used to
#'   calculate the stem base position of the target tree. For details see
#'   [tree_pos()].
#' @param acc_digits integer of length 1 defining the number of digits of
#'   accuracy of the point cloud measurements. Data will be rounded internally
#'   to this value to speed up calculations and avoid problems with joining tree
#'   and neighborhood data resulting from numeric accuracy. Defaults to 2
#'   (round to 2 digits after the decimal point).
#' @param h_xy numeric of length 1 describing the height range in m over the
#'   stem base over which the x and y positions are used to calculate the x and
#'   y coordinates of the stem base of the target tree. Default is 0.3 m. Used
#'   to calculate the stem base position of the target tree. For details see
#'   [tree_pos()].
#' @param res numeric of length 1 defining the resolution of a voxel as passed
#'   on to [VoxR::vox()]. Defaults to 0.1 (10 cm voxel size). Only change if
#'   there are good reasons to do so as this is the standard used in Seidel et
#'   al. (2015) and other papers and the results are strongly scale dependent!
#' @param print_progress character of length 1. Allowed values are "full"
#'   (print all progress and full output), "some" (only print main details) and
#'   "none" (do not print any progress). Defaults to "some".
#' @param override_pos_check logical: should the function test if the target
#'   tree is actually situated within the neighborhood? Defaults to FALSE. Only
#'   change if you have very good reasons to do so, e.g., when computing the
#'   competition for a tree situated at the edge of a forest stand.
#' @param ... additional arguments passed on to [data.table::fread()].
#'
#' @details `compete_pc()` computes competition indices based on voxel counts of
#'   neighbor trees that intersect a search cone or search cylinder around
#'   the target tree. In most cases, the function [read_pc()] that is called
#'   internally should be able to automatically identify the columns with the
#'   coordinates of the point clouds when provided with non-standard file types.
#'   If this is not the case, it is possible to provide arguments to pass on to
#'   that function, to load the datasets separately with custom settings with
#'   [read_pc()] or loading them with external functions and pass them to
#'   [compete_pc()] as any kind of object inheriting from `data.frame` (such as
#'   base R data.frames, tibbles, data.tables etc).
#'
#' ## Cone Method
#'   Based on a search cone with an opening angle of 60 degrees,
#'   by default opening from a basal point situated at 60 % of the
#'   height of the target tree. The competition index is defined as the number
#'   of voxels of neighboring trees  (by default, with a 0.1 m res.) situated
#'   within the cone spanned around the target tree (cf. Metz et al 2013;
#'   Seidel et al., 2015).
#'   The standard value of `h_cone = 0.6` can be adjusted, for instance if no
#'   neighbor trees at all intersect the cone of the target tree. However, be
#'   careful with adjusting this parameter, as competition indices computed with
#'   different `h_cone` cannot easily be compared among each other.
#'
#' ## Cylinder Method
#'   Based on a  search cylinder with a pre-defined radius `cyl_r` around the
#'   target tree (5 m by default). The competition index is defined as the
#'   number of the voxels of neighboring trees  (by default, with a 0.1 m res.)
#'   situated within the cylinder around the target tree (cf. Seidel et al.,
#'   2015). The index is sensitive to the choice of the cylinder radius, so be
#'   careful when comparing competition indices computed with different values
#'   of `cyl_r`.
#'
#'   Both indices are highly sensitive to the voxel resolution, and it is not
#'   recommended to change `res` from its default value of 0.1 (i.e., 10 cm
#'   voxel size) unless you have very good reasons to do so.
#'
#'   If calculating competition indices for single trees that each have an
#'   accompanying point cloud of their immediate neighborhood, using the file
#'   paths to these datasets as a `tree_source` / `neighbor_source` will be
#'   computationally efficient. However, when calculating indices for several
#'   trees belonging to the same neighborhood, it may be faster to load the
#'   neighborhood outside [compete_pc()] a single time using [read_pc()] and
#'   then passing it to [compete_pc()] as a `forest_pc` object as this reduces
#'   the computational overhead due to loading the point cloud into the memory.
#'   In such cases, it may also make sense to load and process the forest point
#'   cloud as an LAS object (see [lidR::LAS-class]) and process it outside of
#'   `TreeCompR` before analysis.
#'   If the source files are very large, this may still lead to memory problems
#'   especially on machines with low RAM capacity. In such cases, it may make
#'   more sense to split up the data set into smaller chunks outside R to
#'   reduce the memory load.
#'
#' ## Note: support of .las, .laz and .ply formats
#'   The `lidR` package has to be installed to be able to read in .las/.laz
#'   files, which are internally processed by [lidR::readTLSLAS()].
#'   Analogously, for point clouds in the .ply format, the `Rvcg` package is
#'   required as these are loaded with [Rvcg::vcgPlyRead()].
#'
#' ## Literature
#' * Metz, J., Seidel, D., Schall, P., Scheffer, D., Schulze, E.-D. & Ammer,
#'   C. (2013). Crown modeling by terrestrial laser scanning
#'   as an approach to assess the effect of aboveground intra- and interspecific
#'   competition on tree growth. Forest Ecology and Management,310:275-288.
#'   https://doi.org/10.1016/j.foreco.2013.08.014
#' * Pretzsch, H., Biber, P. & Dursky, J. (2002). The single tree-based stand
#'   simulator SILVA: construction, application and evaluation. For. Ecol.
#'   Manage. 162, 3-21. https://doi.org/10.1016/S0378-1127(02)00047-6
#' * Seidel, D., Hoffmann, N., Ehbrecht, M., Juchheim, J. & Ammer, C. (2015).
#'   How neighborhood affects tree diameter increment - New insights from
#'   terrestrial laser scanning and some methodical considerations. Forest
#'   Ecology and Management, 336: 119-128.
#'   http://dx.doi.org/10.1016/j.foreco.2014.10.020
#'
#' @return object of type `compete_pc`: modified `data.frame` with tree ID,
#'   tree height, the type of center position used for computation as well as
#'   counts of the number of voxels of the neighborhood point cloud that reach
#'   into the cone/cylinder spanned over/around the target tree.
#' @seealso [read_pc()] for details on reading point clouds, [tree_pos()] for
#'   computing tree position from point cloud objects.
#' @export
#'
#' @examples
#' \dontrun{
#' # Due to the large required datasets it is not possible to provide running
#' # examples, but we hope that these example uses are helpful
#'
#' # Quantifying crown competition for a single tree using the cone method
#' CI_cone <- compete_pc("path/to/forest_pc.las", "path/to/tree_pc.las",
#'                           "cone", h_cone = 0.5)
#'
#' # Competition for a single tree using the cylinder method with 4 m radius
#' CI_cyl <- compete_pc("path/to/forest_pc.ply", "path/to/tree_pc.ply",
#' "cylinder", cyl_r = 4)
#'
#' # Quantifying competition for a single tree using both methods
#' CI_cyl <- compete_pc("path/to/forest_pc.txt", "path/to/tree_pc.txt",
#' "cylinder", cyl_r = 4, h_cone = 0.6)
#'
#' # Loading a large neighborhood outside compete_pc() to reuse the data for
#' # several target trees
#'
#' # load neighborhood
#' neigh <- read_pc("path/to/forest_pc.las")
#'
#' # get paths to trees
#' tree_paths <- list.files("folder_with_trees/")
#'
#' # map over paths to get competition indices for all trees
#' library(tidyverse) # for purrr() and bind_rows()
#' CI_data <- map(
#'   tree_paths,
#'   ~compete_pc(
#'       forest_source = neigh,
#'       tree_source = file.path("folder_with_trees", .x),
#'       tree_name = .x,
#'       method = "cone"
#'       )
#'     ) %>%
#'     bind_rows()
#' }
compete_pc <- function(forest_source, tree_source,
                       comp_method = c("cone", "cylinder", "both"),
                       center_position = c("crown_pos", "base_pos"),
                       tree_name = NULL,
                       cyl_r = 5, h_cone = 0.6, z_min = 100, h_xy = 0.3,
                       acc_digits = 2, res = 0.1,
                       print_progress = c("some", "full", "none"),
                       override_pos_check = FALSE,
                       ...){
  # match arguments against the allowed values
  comp_method <- match.arg(comp_method)
  center_position <- match.arg(center_position)
  print_progress <- match.arg(print_progress)

  # avoid errors with undefined global values in CMD check
  x <- y <- z <- ID <- h <- dist <- r_cone <- NULL

  # if no tree_name is specified, get name from function call
  if(is.null(tree_name)){
    # catch name of tree object before evaluation
    tree_name <- deparse(substitute(tree_source))
    # if a path, get file name without extension
    if (inherits(tree_source, "character")){
      tree_name <- tools::file_path_sans_ext(basename(tree_source))
    }
  }
  # print progress
  if (print_progress != "none"){
    cat("----- Processing competition indices for:", tree_name, "-----\n")
  }
  # read data for central tree
  tree <- read_pc(tree_source, verbose = print_progress == "full", ...)
  # get position and height of central tree
  position <- tree_pos(tree, z_min = z_min, h_xy = h_xy, res = res)
  # get basis position of the cone/cylinder
  pos <- position[[center_position]]
  #  extract height of central tree
  h <- position[["height"]]

  # read data for neighborhood
  hood <- read_pc(forest_source, verbose = print_progress == "full", ...)

  # check if the tree is part of this neighborhood
  if (!(pos["x"] %inrange% range(hood$x) && pos["y"] %inrange% range(hood$y))){
    if (!override_pos_check){
      stop(
        .wr(
          "The basis coordinates of the tree are outside the x and y range",
          "of the neighborhood. The tree may not be situated in this plot!",
          "Please check the raw data for mismatch in the coordinates. \n\n",
          "If this is desired because you for instance want to compute the",
          "CI for an edge tree, set override_pos_check = TRUE.")
      )
    }
  }

  # define filters for relevant ranges in the neighborhood
  if (comp_method == "cylinder"){
    xlim <- pos["x"] + c(-1, +1) * (cyl_r + res)
    ylim <- pos["y"] + c(-1, +1) * (cyl_r + res)
    zlim <- NULL
  }
  if (comp_method == "cone"){
    # radius of search cone at highest point in the dataset
    cone_r <- tan(pi / 6) * (max(hood$z) - pos["z"] - h_cone * h)
    xlim <- pos["x"] + c(-1, +1) * (cone_r + res)
    ylim <- pos["y"] + c(-1, +1) * (cone_r + res)
    zlim <- c(h * h_cone + pos["z"] - res, Inf)
  }
  if (comp_method == "both"){
    # radius of search cone at highest point in the dataset
    cone_r <- tan(pi / 6) * (max(hood$z) - pos["z"] - h_cone * h)
    xlim <- pos["x"] + c(-1, +1) * (max(cone_r, cyl_r) + res)
    ylim <- pos["y"] + c(-1, +1) * (max(cone_r, cyl_r) + res)
    zlim <- NULL
  }
  # remove values outside the relevant range (in separate step)
  hood <- .validate_pc(hood, xlim = xlim, ylim = ylim, zlim = zlim)

  # round neighborhood and tree to the specified digits of accuracy
  # (in two steps to perform anti_join with integers: much faster)
  for (i in 1:3){
    hood[[i]] <- as.integer(Rfast::Round(hood[[i]] * 10 ^ acc_digits))
    tree[[i]] <- as.integer(Rfast::Round(tree[[i]] * 10 ^ acc_digits))
  }
  # remove points in the neighborhood that belong to the central tree
  # (data.table version of anti_join)
  neighbor <- hood[!tree, on = c("x", "y", "z")] / (10 ^ acc_digits)

  # prepare data.frame for results
  results <- data.table::data.table(
    target = tree_name,
    height_target = h,
    center_position = ifelse(
      center_position == "base_pos", "base", "crown center"))

  # voxelize neighborhood data
  if (res != 0.1) warning(
    .wr(
      "Creating voxelized dataset with a voxel resolution of res =", res,
      "instead of the standard of res = 0.1.",
      "Non-standard resolutions are not recommended due to the",
      "scale dependence of the indices.")
  )
  voxel <- VoxR::vox(neighbor, res = res)
  # compute competition indices for the cone method
  if (comp_method == "cone" | comp_method == "both") {
    # compute base height of the cone
    cone_h <- h_cone * h
    # remove voxels below critical height
    voxel1 <- voxel[z - pos["z"] >= cone_h, ]
    # compute cone radius (assuming an opening angle of pi / 3)
    voxel1 <- voxel1[, `:=`(
      r_cone = abs(tan(pi / 6) * (z - pos["z"] - cone_h)),
      dist = sqrt((x - pos["x"]) ^ 2 + (y - pos["y"]) ^ 2))]
    # append voxel count inside the cone and cone height to results
    results$CI_cone <- with(voxel1, sum(dist <= r_cone))
    results$h_cone  <- h_cone
  }
  if (comp_method == "cylinder" | comp_method == "both"){
    # compute distance from central position
    voxel2 <- voxel[, `:=`(dist = sqrt((x - pos["x"]) ^ 2 +
                                         (y - pos["y"]) ^ 2))]
    # append voxel count inside the cylinder and cylinder radius to results
    results$CI_cyl <- with(voxel2, sum(dist <= cyl_r))
    results$cyl_r <- cyl_r
  }

  # remove row.names of results for proper printing
  row.names(results) <- NULL
  # set class of results
  class(results) <- c("compete_pc", class(results))

  # print progress
  if (print_progress == "full") print(results)
  if (print_progress == "some"){
    if(comp_method == "cone" | comp_method == "both"){
      cat("Cone-based CI =", results$CI_cone, "     ")
    }
    if(comp_method == "cylinder" | comp_method == "both"){
      cat("Cylinder-based CI =", results$CI_cyl, "     ")
    }
    cat("\n")
  }
  # return results
  return(results)
}

# Define printing method for compete_pc objects:
#' @rdname compete_pc
#' @format NULL
#' @usage NULL
#' @export
print.compete_pc <- function(x, digits = 3, topn = 3, nrows = 8, ...){
  # get tree name or number of observations
  target <- ifelse(nrow(x) == 1,
                   paste0("'", x$target, "'"),
                   paste(nrow(x), " target trees"))
  # prepare header
  header <- paste0(
    "Point cloud based competition indices for ", target)
  # print data.table with points
  .print_as_dt(x, digits = digits, topn = topn,
               header = header, nrows = nrows, ...)
  # return object invisibly
  invisible(x)
}


# Define rbind method for forest_pc objects:
#' @rdname compete_pc
#' @format NULL
#' @usage NULL
#' @export
rbind.compete_pc <- function(
    ..., use.names = TRUE, fill = FALSE, idcol = NULL){
  .rbind_with_class(..., use.names = use.names, fill = fill, idcol = idcol)
}
