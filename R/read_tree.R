#' Read a single tree point cloud
#'
#' @param path character path to point cloud of individual tree or a whole plot
#'
#' @return data frame with X,Y,Z of the tree point cloud
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' }
read_tree <- function(path){
data <- data.table::fread(path)
tree <- data.frame(data[, 1:3])
colnames(tree) <- c("X", "Y", "Z")
return(tree)
}
