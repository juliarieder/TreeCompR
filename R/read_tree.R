#' Read a single tree point cloud
#'
#' @param path character path to point cloud of individual tree or a whole plot
#'   in las/laz format or file format readable with [data.table::fread()]
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Internal function for reading point cloud data. Currently, the
#'   supported formats are las/laz and formats readable with
#'   [data.table::fread()]. For other formats, please load the point cloud data
#'   separately and enter the coordinates in downstream functions as a
#'   data.frame.
#'
#'   If provided with tabular data, the function by default takes the columns
#'   named "X", "Y", and "Z" or "x", "y", and "z" to be the coordinate vectors.
#'   If no columns with matching names are available, it takes the first three
#'   numeric columns and returns a message. If the dataset does not contain
#'   three numeric columns or one of the columns labeled X, Y, and Z
#'   is not numeric, the function fails with an error.
#'
#' @return data frame with X,Y and Z coordinates of the tree or forest point
#'    cloud
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud in txt format
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' # Read the tree point cloud in las or laz format
#' tree <- read_tree(path = "path/to/tree_point_cloud.las")
#' }
read_tree <- function(path, ...) {
  . <- NULL

  # get file extension
  extension <- utils::tail(base::strsplit(path, split = ".", fixed = TRUE)[[1]], 1)

  # load las/laz point cloud
  if (extension %in% c("las", "laz")) {
    # Check if lidR package is installed
    if (requireNamespace("lidR", quietly = TRUE)) {
      # If installed, proceed with the code for *.las files
      las <- lidR::readTLSLAS(path)
      tree <- data.frame("X" = las$X, "Y" = las$Y, "Z" = las$Z)
    } else {
      # If not, return an error message about the missing package
      stop("Please install the 'lidR' package if you want to use data in las/laz format. \n")
    }
  } else {
    # try loading in point cloud with fread
    tree <- try(
      data.table::fread(file = path, data.table = FALSE, ...)
    )
    if (inherits(tree, "try-error")) {
      # if the file cannot be read, return error message about accepted formats.
      stop("File format cannot be read. Please use point cloud with extension las/laz\n or a format readable by data.table::fread().")
    }
    # check for consistency
    if (all(c("X", "Y", "Z") %in% toupper(names(tree)))){ # if coordinates are named, use these
      # convert to upper case
      names(tree) <- toupper(names(tree))
      # use these columns
      tree <- tree[, c("X", "Y", "Z")]
      # test if any of the columns has the wrong class
      if (!all(sapply(tree, is.numeric))){
        stop("One or more of the coordinate vectors X, Y, Z is not numeric.\nPlease check raw data.")
      }
    } else{ # else, use the first three numeric columns
      # get numeric columns
      nums <- which(sapply(tree, is.numeric))
      # warn if there are not enough numeric columns
      if (length(nums) < 3){
        stop("Tree contains less than 3 numeric coordinate vectors.\nPlease check raw data.")
      } else {
        # get first three numeric columns
        tree <- tree[,nums[1:3]]
        # message about used coordinate vectors
        message(paste0("No named coordinates. Columns ",
                       paste(names(tree), collapse = ", "),
                       " (no. ", paste(nums[1:3], collapse = ", "),
                       " in raw data)\n   used as X, Y, Z coordinates, respectively."))
        # adjust names
        names(tree) <- c("X", "Y", "Z")
      }
    }
  }

  # return object with correct column number, names and types
  return(tree)
}
