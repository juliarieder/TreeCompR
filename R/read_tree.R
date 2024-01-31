#' Read a single tree point cloud
#'
#' @param path character path to point cloud of individual tree or a whole plot
#'
#' @return data frame with X,Y,Z of the tree or forest point cloud in txt or las/laz format
#'
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
  extension <- utils::tail(base::strsplit(path, split = ".", fixed = TRUE)[[1]], 1)
  if (extension == "txt") {
    tree <- data.table::fread(path) %>%
      data.frame() %>%
      .[, 1:3] %>%
      setNames(c("X", "Y", "Z"))
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
