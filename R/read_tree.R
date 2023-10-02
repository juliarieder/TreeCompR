#' Read a single tree point cloud
#'
#' @param path character path to point cloud of individual tree or a whole plot
#'
#' @return data frame with X,Y,Z of the tree point cloud
#' @export
#'
#' @examples
read_tree <- function(path){
data <- data.table::fread(path)
tree <- data.frame(data[, 1:3])
colnames(tree) <- c("X", "Y", "Z")
return(tree)
}
