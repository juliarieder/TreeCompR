#' Position of the Tree from Point Cloud
#'
#' @param tree data frame (X, Y, Z) of the tree point cloud
#'
#' @return X,Y,Z coordinates of the tree's position
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' # Get the position of this tree (X, Y, Z)
#' pos <- position(tree)
#' }
position <- function(tree){
  lowest <- tree[order(tree$Z, decreasing = FALSE), ][1:100, ]
  return(c(stats::median(lowest$X), stats::median(lowest$Y), stats::median(lowest$Z)))
}

#' Height of the Tree Point Cloud
#'
#' @param tree data frame (X, Y, Z) of the tree point cloud
#'
#' @return tree height (numeric)
#' @export
#'
#' @examples
#' \dontrun{
#' # Read the tree point cloud
#' tree <- read_tree(path = "path/to/tree_point_cloud.txt")
#' # Get the position of this tree (X, Y, Z)
#' pos <- position(tree)
#' # Calculate the tree height in m
#' H_tree <- height(tree)
#' }
height <- function(tree){
  h <- max(tree$Z) - min(tree$Z)
  return(h)
}
