#' Read a single tree point cloud
#'
#' @param path character path to point cloud of individual tree or a whole plot
#'   in txt or las/laz format.
#'
#' @details Internal function for reading point cloud data. Currently, the
#'   supported formats are txt and las/laz. For other formats, please load the
#'   point cloud data separately and enter the coordinates as a data.frame.
#'
#'   If provided with a txt file, the function takes the columns named "X", "Y",
#'   and "Z" or "x", "y", and "z". If no columns with these names are available,
#'   it takes the first three numeric columns and returns a warning. If the
#'   dataset does not contain three numeric columns or one of the columns
#'   labeled X, Y, and Z is not numeric, the function fails with an error.
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
read_tree <- function(path) {
  . <- NULL

  # get file extension
  extension <- utils::tail(base::strsplit(path, split = ".", fixed = TRUE)[[1]], 1)

  if (extension == "txt") {
    # load in tree
    tree <- as.data.frame(
      data.table::fread(path)
      )
    # check for consistency
    if (all(c("X", "Y", "Z") %in% toupper(names(tree)))){ # if coordinates are named, use these
      # convert to upper case
      names(tree) <- toupper(names(tree))
      # use these columns
      tree <- tree[, c("X", "Y", "Z")]
      # test if any of the columns has the wrong class
      if (!all(apply(tree, 2, is.numeric))){
        error("One or more of the coordinate vectors X, Y, Z is not numeric.\nPlease check raw data.")
      }
    } else{ # else, use the first three numeric columns
      # get numeric columns
      tree <- subset(tree, , apply(tree, 2, is.numeric))
      # warn if there are not enough numeric columns
      if (ncol(tree) < 2){
        error("Tree contains less than 3 numeric coordinate vectors.\nPlease check raw data.")
      } else {
        # get first three numeric columns
        tree <- tree[,1:3]
        # message about used coordinate vectors
        message(paste0("No named coordinates. Columns ",
                       paste(names(tree), collapse = ", "),
                       " used as X, Y, Z coordinates, respectively."))
        # adjust names
        names(tree) <- c("X", "Y", "Z")
      }
    }
  } else if (extension %in% c("las", "laz")) {
    # Check if lidR package is installed
    if (requireNamespace("lidR", quietly = TRUE)) {
      # If installed, proceed with the code for *.las files
    las <- lidR::readTLSLAS(path)
    tree <- data.frame("X" = las$X, "Y" = las$Y, "Z" = las$Z)
    } else {
      stop("Please install the 'lidR' package if you want to use data in las/laz format. \n")
    }
  } else {
    stop("Cannot read this extension. Please use point cloud with extension .txt or las/laz")
    tree <- data.frame(X = numeric(0), Y = numeric(0), Z = numeric(0))
  }

  return(tree)
}
