#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @importFrom concaveman concaveman
#' @importFrom data.table :=
#' @importFrom data.table fread %inrange%
#' @importFrom magrittr %>%
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
