#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @importFrom concaveman concaveman
#' @importFrom data.table :=
#' @importFrom data.table fread %inrange%
#' @importFrom magrittr %>%
#' @importFrom nabor knn
#' @importFrom Rfast Round Table
#' @importFrom rlang .data
#' @importFrom sf st_as_sf
#' @importFrom sf st_buffer
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersection
#' @importFrom sf st_polygon
#' @importFrom stats setNames
#' @importFrom tools file_path_sans_ext
#' @importFrom utils data
#' @importFrom utils tail
#' @importFrom VoxR vox
## usethis namespace: end
NULL


#' @keywords internal
#' internal function for better wrapping of long messages, warnings and errors
.wr <- function(..., call){
  paste(strwrap(paste(...), prefix = "", initial = ""), collapse = "\n")
}

#' @keywords internal
#' internal function for rapidly output as rounded data tables
.print_as_dt <- function(x, digits, topn, ...){
  # identify and round numeric variables to 'digits'
  numerics <- which(sapply(x, is.double))
  for (i in numerics) x[[i]] <- Rfast::Round(x[[i]], digits)
  # print as data.table with specified settings
  print(data.table::as.data.table(x), topn = topn, trunc.cols = TRUE, ...)
}
