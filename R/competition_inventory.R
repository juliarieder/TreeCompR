#' Calculating distance-dependant Tree Competition Using Inventory Data (field or from LiDAR data)
#'
#' @param path character string path to .csv file with inventory data with structure (ID, X, Y, DBH, H)
#' @param radius numeric, Search radius around target tree, wherein all neighboring trees are classified as competitors
#' @param dbh_thr numeric, DBH threshold for classifying the tree as a competitor (default is 0.1 m)
#' @param target_tree numeric (ID) or a vector of coordinates (X, Y)
#' @param type character string assigning the type of input of target_tree "ID" or "coordinates"
#' @param tolerance numeric. tolerance for matching the tree coordinates. If a field measured value is used for target_tree, take a higher tolerance value (default=0.1 m), depending on measurement accuracy
#'
#' @param method character string assigning the method for quantifying competition "Hegyi", "CI12", "CI13"
#'
#' @return numeric. Competition Index value
#' @importFrom utils data
#' @importFrom data.table fread
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate some distance-dependent CIs for one tree inside forest plot
#' # input coordinates target tree
#' ttree <- c(15, 9)
#' CI <- compete_calc("path/to/invtable.csv", dbh_thr = 0.1, ttree, "coordinates", 0.1, method = "all")
#'
#' # Calculate the Hegyi-Index for one tree inside a forest plot, giving the ID of the target tree
#' ID_tree <- 5
#' CI <- compete_calc("path/to/invtable.csv", dbh_thr = 0.1, ID_tree, "ID", 0.1, method = "Hegyi")
#' #}
compete_calc <- function(path, radius = 10, dbh_thr = 0.1, target_tree, type = c("ID", "coordinates"), tolerance = 0.1, method = c("all", "Hegyi", "CI12", "CI13")) {

  #Meldung einbauen, falls invalid extension
  trees <- data.table::fread(path)
  trees <- data.frame(trees[, 1:5])
  target_tree <- as.numeric(target_tree)
  colnames(trees) <- c("ID", "X", "Y", "DBH", "H")

  if (type == "ID") {
    #user provides the ID of the target tree
    #Look up the right coordinates
    #matching_rows <- trees[trees$ID == target_tree]
    matching_rows <- subset(trees, trees$ID == target_tree)
    if (nrow(matching_rows) == 0){
      stop("This Tree ID is not existing within this plot!")
    } else if(nrow(matching_rows) > 1) {
      stop("Warning: there are more than 1 Trees with this ID, please check!")
    } else if(nrow(matching_rows) == 1){
      trees <- trees %>% dplyr::mutate(status = ifelse(.data$ID == target_tree, "target_tree", "competitor"))
    }
    else if(type == "coordinates"){
      #make sure that the coordinates are in the same coordinate system!
      X_pos = target_tree[1]
      Y_pos = target_tree[2]
      trees <- trees %>% dplyr::mutate(euc_dist = sqrt((X_pos - .data$X)^2 + (Y_pos - .data$Y)^2)) %>%
        dplyr::mutate(status = ifelse(.data$euc_dist == min(.data$euc_dist), "target_tree", ifelse(.data$euc_dist > min(.data$euc_dist),"competitor", NA)))
      if (min(.data$euc_dist) < tolerance){
        stop("There was no tree found within the tolerance threshold. Check the coordinates again or if the accuracy of field data was low, set a new tolerance value.")
      } else {

      }}}
  trees <- trees %>% dplyr::mutate(
    H_target = matching_rows$H,
    dbh_target = matching_rows$DBH,
    X_target = matching_rows$X,
    Y_target = matching_rows$Y,
    target_ID = matching_rows$ID)

  trees <- trees %>% dplyr::mutate(euc_dist_comp = sqrt((.data$X_target - .data$X)^2 + (.data$Y_target - .data$Y)^2)) %>% dplyr::filter(.data$euc_dist_comp <= radius) %>%
    dplyr::mutate(CI_h_part = .data$DBH / (.data$dbh_target * .data$euc_dist_comp)) %>%
    dplyr::mutate(CI12_part = atan(.data$H / .data$euc_dist_comp), CI13_part = (.data$H / .data$H_target) * atan(.data$H / .data$euc_dist_comp)) %>% dplyr::mutate(CI11_part = (.data$DBH / .data$dbh_target) * atan(.data$DBH / .data$euc_dist_comp)) %>%
    dplyr::filter(.data$status == "competitor")

  CIs <- trees %>% dplyr::group_by(.data$target_ID) %>% dplyr::summarise(
    CI_Hegyi = sum(.data$CI_h_part),
    CI11 = sum(.data$CI11_part),
    CI12 = sum(.data$CI12_part),
    CI13 = sum(.data$CI13_part)
  )

  if (method == "all") {
    return(CIs)
  } else if (method == "Hegyi") {
    CI_Hegyi <- CIs %>%
      dplyr::select(.data$target_ID, .data$CI_Hegyi)
    return(CI_Hegyi)
  } else if (method == "CI11") {
    CI_11 <- CIs %>%
      dplyr::select(.data$target_ID, .data$CI11)
    return(CI_11)
  } else if (method == "CI12") {
    CI_12 <- CIs %>%
      dplyr::select(.data$target_ID, .data$CI12)
    return(CI_12)
  } else if (method == "CI13") {
    CI_13 <- CIs %>%
      dplyr::select(.data$target_ID, .data$CI13)
    return(CI_13)
  } else {
    stop("This input format is not supported, please enter an existing Tree ID or the coordinates of the target tree.")
  }
  #provide different indices (all, or just some!)
  #Methoden gut beschreiben und Literatur einfügen
  #alle relevanten distance dependent competition indices einbinden
  #Warnmeldung ausgeben, falls target tree nicht mindestens search radius von edge of plot weit weg ist!!
  #fehlt noch ein else() auf Ebene type_ttree??
}

