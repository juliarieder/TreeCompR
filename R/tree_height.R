#' Position of the Tree from Point Cloud
#'
#' @param tree data frame (X, Y, Z) of the tree point cloud
#'
#' @return X,Y,Z coordinates of the tree's position
#'
#' @export
#'
#' @examples
position <- function(tree){
  lowest <- tree[order, tree$Z, decreasing = FALSE, ][1:50, ]
  return(c(stats::median(lowest$X), stats::median(lowest$Y), stats::median(lowest$Z)))
}

#' Tree Height of the Point Cloud
#'
#' @param tree data frame (X, Y, Z) of the tree point cloud
#'
#' @return tree height (numeric)
#' @export
#'
#' @examples
height <- function(tree){
  h <- max(tree$Z) - min(tree$Z)
  return(h)
}
