#' Calculating distance-dependant Tree Competition Using Inventory Data (field or from LiDAR data)
#'
#' @param path character string path to .csv file with inventory data with structure (ID, X, Y, DBH, h)
#' @param radius numeric, Search radius around target tree, wherein all neighboring trees are classified as competitors
#' @param dbh_threshold numeric, DBH threshold for classifying the tree as a competitor (default is 0.1 m)
#' @param target_tree numeric (ID) or a vector of coordinates
#' @param type character string assigning the type of input of target_tree "ID" or "coordinates"
#' @param tolerance numeric. tolerance for matching the tree coordinates. If a field measured value is used for target_tree, take a higher tolerance value (default=0.1 m), depending on measurement accuracy
#'
#' @param method character string assigning the method for quantifying competition "Hegyi", "CI12", "CI13"
#'
#' @return numeric. Competition Index value
#' @export
#'
#' @examples
compete_calc <- function(path, radius = 10, dbh_threshold = 0.1, target_tree, type = c("ID", "coordinates"), tolerance = 0.1, method = c("Hegyi", "CI12", "CI13")) {
  #match arguments
  trees <- data.table::fread(path)
  trees <- data.frame(trees[, 1:3])
  colnames(trees) <- c("ID", "X", "Y", "DBH")
  type_ttree <- rlang::arg_match(type)
  if (type_ttree == "ID") {
    #user provides the ID of the target tree
    #Look up the right coordinates
    matching_rows <- data[ID == target_tree]
    if (nrow(matching_rows) == 0){
      stop("This Tree ID is not existing within this plot!")
    } else if(nrow(matching_rows) > 1) {
      stop("Warning: there are more than 1 Trees with this ID")
    } else if(nrow(matching_rows) == 1){
      data <- data %>% dplyr::mutate(status = ifelse(data$ID == target_tree, "target_tree", "competitor"))
    }
  else if(type_ttree == "coordinates"){
#hier funktion schreiben, die die angegebenen Koordinaten (ggf. aus Feld aufgenommen) mit dem Baum in Tabelle matched, der am nächsten liegt; tolerance? bei Felddaten deutlich höher setzen!
    }
  }
  return(data)
  data <- data %>% dplyr::filter(DBH >= dbh_threshold)
  #Hegyi, C11, C12, C13...
}
