#' @title Read a single tree point cloud
#'
#' @description Read point cloud sourced from a file path or an object that
#'   inherits from class data.frame.
#'
#' @param tree_source object that inherits from class data.frame, or character
#'   path to point cloud of individual tree or a whole plot either in las/laz
#'   format or any file format readable with [data.table::fread()]. If provided
#'   with a point cloud object in a data.frame, the structure and column names
#'   are validated and homogenized; else, the function tries to read the point
#'   cloud in the specified path
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Function for reading and validating point cloud data. Currently, the
#'   supported formats are las/laz and any formats readable with
#'   [data.table::fread()]. For other formats, please load the point cloud data
#'   separately and enter the coordinates as a data.frame.
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
#' @return object of class c("tree_pc", "data.frame") with x, y and z
#'  coordinates of the tree or forest point cloud
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud in txt format
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' # Read the tree point cloud in las or laz format
#' tree <- read_tree(path = "path/to/tree_point_cloud.las")
#' }
read_tree <- function(tree_source, ...) {
  . <- NULL
  # check class of source tree
  if (inherits(tree_source, "data.frame")){
    tree <- .validate_tree(tree_source)
  } else if (!(is.character(tree_source) && length(tree_source) == 1)){
    stop("Format of tree_source not recognized.\n",
         " Please provide a data.frame or a path to a source file.\n")
  } else if(!file.exists(tree_source)){
    stop("File", tree_source,
         "does not exist. \nPlease check path to point cloud file.")
  } else {
    # treat tree_source as path to file
    path <- tree_source
    # get file extension
    extension <- utils::tail(
      base::strsplit(path, split = ".", fixed = TRUE)[[1]], 1)
    # load las/laz point cloud
    if (extension %in% c("las", "laz")) {
      # Check if lidR package is installed
      if (requireNamespace("lidR", quietly = TRUE)) {
        # If installed, proceed with the code for *.las files
        las <- lidR::readTLSLAS(path)
        tree <- data.frame(x = las$X, y = las$Y, z = las$Z) %>%
          .validate_tree()
      } else {
        # If not, return an error message about the missing package
        stop("Please install the 'lidR' package",
             " if you want to use data in las/laz format. \n")
      }
    } else {
      # try loading in point cloud with fread
      tree <- try(
        data.table::fread(file = path, data.table = FALSE, ...)
      )
      if (inherits(tree, "try-error")) {
        # if the file cannot be read, return error message about accepted formats.
        stop("File format cannot be read. ",
             "Please use point cloud with extension las/laz",
             "\n or a format readable by data.table::fread().")
      } else{ # else validate and return
        tree <- .validate_tree(tree)
      }
    }
  }
  # return tree_df object with correct column number, names and types
  return(tree)
}

#' @keywords internal
#' internal function for the validation of point cloud data
.validate_tree <- function(tree){
  # check if tree is already formatted correctly and return if true
  if (inherits(tree, "tree_pc")){
    return(tree)
  } else { #else check for consistency
    # if coordinates are named, use these
    if (all(c("x", "y", "z") %in% tolower(names(tree)))){
      # convert to upper case
      names(tree) <- tolower(names(tree))
      # use these columns
      tree <- tree[, c("x", "y", "z")]
      # test if any of the columns has the wrong class
      if (!all(sapply(tree, is.numeric))){
        stop("One or more of the coordinate vectors x, y, z",
             " is not numeric.\nPlease check raw data.")
      }
    } else{ # else, use the first three numeric columns
      # get numeric columns
      nums <- which(sapply(tree, is.numeric))
      # warn if there are not enough numeric columns
      if (length(nums) < 3){
        stop("Tree contains less than 3 numeric coordinate vectors.",
             "\n Please check raw data.")
      } else {
        # get first three numeric columns
        tree <- tree[,nums[1:3]]
        # message about used coordinate vectors
        message(
          "No named coordinates. Columns ",
          paste(names(tree), collapse = ", "),
          " (no. ", paste(nums[1:3], collapse = ", "),
          " in raw data)\n   used as x, y, z coordinates, respectively.")
        # adjust names
        names(tree) <- c("x", "y", "z")
      }
    }
    # set class to tree_pc object
    class(tree) <- c("tree_pc", class(tree))
    # return the validated tree object
    return(tree)
  }
}
