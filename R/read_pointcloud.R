#' @title Read trees and neighborhoods from point cloud data.
#'
#' @description Read and validate a point cloud sourced from a file stored on
#'   disk, or from an object that inherits from class data.frame. Supported
#'   file formats are .las, .laz, .ply as well as all formats accepted by
#'   [data.table::fread()] (.csv, .txt, and others).
#' @param pc_source object that inherits from class data.frame, or character
#'   path to point cloud of individual tree or a whole plot either in .las/.laz
#'   or .ply format, or any file format readable with [data.table::fread()].
#'   If provided with a point cloud object in a data.frame, the structure and
#'   column names are validated and homogenized; else, the function tries to
#'   read the point cloud in the specified path.
#' @param verbose logical of length 1. Should information about progress be
#'   printed? Defaults to TRUE.
#' @param xlim (optional) numeric vector of defining the range of x coordinates.
#'  Can be a vector of length 2 with the minimum and maximum x value, or a
#'  vector of arbitrary length (in this case [base::range()] is used to
#'  constrain the x values to its range). This can be useful to for the
#'  memory-efficient handling of very large point cloud objects. Defaults to
#'  NULL (use full range of x coordinates).
#' @param ylim (optional) numeric vector of defining the range of y coordinates.
#'  Can be a vector of length 2 with the minimum and maximum y value, or a
#'  vector of arbitrary length (in this case [base::range()] is used to
#'  constrain the y values to its range). This can be useful to for the
#'  memory-efficient handling of very large point cloud objects. Defaults to
#'  NULL (use full range of y coordinates).
#' @param zlim (optional) numeric vector of defining the range of z coordinates.
#'  Can be a vector of length 2 with the minimum and maximum z value, or a
#'  vector of arbitrary length (in this case [base::range()] is used to
#'  constrain the z values to its range). This can be useful to for the
#'  memory-efficient handling of very large point cloud objects. Defaults to
#'  NULL (use full range of z coordinates).
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Function for reading and validating point cloud data. Currently,
#'   the supported file formats are .las, .laz, .ply, as well as all formats
#'   accepted by [data.table::fread()]. For other formats, please load the
#'   point cloud data separately and enter the coordinates as a data.frame.
#'
#'   If provided with tabular data (either as a data.frame or via a path to an
#'   [data.table::fread()] readable source), the function by default takes the
#'   columns named "X", "Y", and "Z" or "x", "y", and "z" to be the coordinate
#'   vectors.
#'   If no columns with matching names are available, it takes the first three
#'   numeric columns and returns a message. If the dataset does not contain
#'   three numeric columns or one of the columns labeled x, y, and z is not
#'   numeric, the function fails with an error.
#'
#'   The xlim, ylim and zlim arguments are used internally in [compete_pc()] to
#'   filter the neighborhood dataset to a relevant range around the target tree
#'   to speed up calculations.
#'
#' # Note: support of .las, .laz and .ply formats
#'   The the 'lidR' package has to be installed to be able to read in .las/.laz
#'   files, which are internally processed by [lidR::readTLSLAS()].
#'   Analogously, for point clouds in the .ply format, the 'Rvcg' package is
#'   required as these are loaded with [Rvcg::vcgPlyRead()].
#'
#' @return object of class c("forest_pc", "data.table", "data.frame") with x, y
#' and z coordinates of the tree or forest point cloud.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a tree point cloud in txt format
#' tree <- read_pc(pc_source = "path/to/tree_point_cloud.txt")
#' # Read a tree point cloud in las or laz format
#' tree <- read_pc(pc_source = "path/to/tree_point_cloud.las")
#' #if point cloud is already loaded as dataframe tree_df
#' tree <- read_pc(tree_df)
#' }
read_pc <- function(pc_source, verbose = TRUE,
                    xlim = NULL, ylim = NULL, zlim = NULL, ...) {
  . <- NULL
  # check class of source dataset
  if (inherits(pc_source, "data.frame")){
    pc <- .validate_pc(pc_source, verbose = verbose,
                       xlim = xlim, ylim = ylim, zlim = zlim)
  } else if (!(is.character(pc_source) && length(pc_source) == 1)){
    stop("Format of pc_source not recognized.\n",
         " Please provide a data.frame or a path to a source file.\n")
  } else if(!file.exists(pc_source)){
    stop("File", pc_source,
         "does not exist. \nPlease check path to point cloud file.")
  } else {
    # treat pc_source as path to file
    path <- pc_source
    # get file extension
    extension <- tolower( # allow lower and upper case extensions
      utils::tail(
        base::strsplit(path, split = ".", fixed = TRUE)[[1]], 1)
    )
    # load las/laz point cloud
    if (extension %in% c("las", "laz")) {
      # Check if lidR package is installed
      if (requireNamespace("lidR", quietly = TRUE)) {
        # If installed, proceed with the code for *.las files
        las <- lidR::readTLSLAS(path)
        # extract coordinates and validate
        pc <- las@data[,1:3] %>%
          .validate_pc(verbose = verbose,
                       xlim = xlim, ylim = ylim, zlim = zlim)
      } else {
        # If not, return an error message about the missing package
        stop("Please install the 'lidR' package",
             " if you want to use data in .las/.laz format. \n")
      }
      # load las/laz point cloud
    } else if (extension == "ply") {
      # Check if lidR package is installed
      if (requireNamespace("Rvcg", quietly = TRUE)) {
        # If installed, proceed with the code for *.ply files
        ply <- Rvcg::vcgPlyRead(path, updateNormals = TRUE, clean = TRUE)
        # extract coordinates and validate
        pc  <- data.table::data.table(
          x = ply$vb[1,], y = ply$vb[2,], z = ply$vb[3,]) %>%
          .validate_pc(verbose = verbose,
                       xlim = xlim, ylim = ylim, zlim = zlim)
      } else {
        # If not, return an error message about the missing package
        stop("Please install the 'Rvcg' package",
             " if you want to use data in .ply format. \n")
      }
    } else {
      # try loading in point cloud with fread
      pc <- try(
        data.table::fread(file = path, data.table = TRUE, ...)
      )
      if (inherits(pc, "try-error")) {
        # if the file cannot be read, return error message about accepted formats.
        stop("File format cannot be read. ",
             "Please use point cloud in .las/.laz or .ply format,",
             "\n or a format readable by data.table::fread().")
      } else{ # else validate and return
        pc <- .validate_pc(pc, verbose = verbose,
                           xlim = xlim, ylim = ylim, zlim = zlim)
      }
    }
  }
  # return forest_pc object with correct column numbers, names and types
  return(pc)
}

#' @keywords internal
#' internal function for the validation of point cloud data
.validate_pc <- function(pc, res, xlim = NULL, ylim = NULL, zlim = NULL,
                         verbose = FALSE){
  # check if pc is not in forest_pc format, ensure correct formatting
  if (!inherits(pc, "forest_pc")){
    # if dataset is a different format, convert to data.table format
    if (!inherits(pc, "data.table")) pc <- data.table::as.data.table(pc)
    # if coordinates are named, use these
    if (all(c("x", "y", "z") %in% tolower(names(pc)))){
      # convert to lower case
      names(pc) <- tolower(names(pc))
      # use these columns
      pc <- subset(pc, select = c("x", "y", "z"))
      # test if any of the columns has the wrong class
      if (!all(sapply(pc, is.numeric))){
        stop("One or more of the coordinate vectors x, y, z",
             " is not numeric.\nPlease check raw data.")
      }
    } else{ # else, use the first three numeric columns
      # get numeric columns
      nums <- which(sapply(pc, is.numeric))
      # warn if there are not enough numeric columns
      if (length(nums) < 3){
        stop("Point cloud dataset contains less than 3 numeric coordinate ",
             "vectors.\n Please check raw data.")
      } else {
        # get first three numeric columns
        pc <- subset(pc, select = nums[1:3])
        # message about used coordinate vectors
        if(verbose){
          message(
            "No named coordinates. Columns ",
            paste(names(pc), collapse = ", "),
            " (no. ", paste(nums[1:3], collapse = ", "),
            " in raw data)\n   used as x, y, z coordinates, respectively.")
        }
        # adjust names
        names(pc) <- c("x", "y", "z")
      }
    }
  }
  # identify dimensions with range limits
  sub <- !sapply(list(xlim, ylim, zlim), is.null)
  # if there are filters, filter accordingly
  if (any(sub)){
    # create subset for filtering (will also work when ranges are specified
    # as vectors with length > 2 or in incorrect order due to use of range())
    subs <- c("(x %inrange% range(xlim))",
              "(y %inrange% range(ylim))",
              "(z %inrange% range(zlim))")
    filter <- paste(subs[sub], collapse = " & ")
    # call subset on the point cloud with the defined filter
    pc <- do.call(
      "subset",
      args = list(x = pc,
                  subset = parse(text = filter)
      )
    )
  }
  # set class to forest_pc object if not of that class already
  if (!inherits(pc, "forest_pc")) class(pc) <- c("forest_pc", class(pc))
  # return the validated forest object
  return(pc)
}

# Define printing method for forest_pc objects:
#' @rdname read_pc
#' @format NULL
#' @usage NULL
#' @export
print.forest_pc <- function(x, topn = 3, digits = 2, ...){
  cat("---------------------------------------",
      " \n'forest_pc' class point cloud: \ncollection of", nrow(x),"observations",
      "\n---------------------------------------\n"
  )
  # print data.table with points
  print(round(data.table::as.data.table(x), digits = digits), topn = 3, ...)
  # return object invisibly
  invisible(x)
}
