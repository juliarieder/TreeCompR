#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom concaveman concaveman
#' @importFrom data.table :=
#' @importFrom data.table fread
#' @importFrom data.table %inrange%
#' @importFrom nabor knn
#' @importFrom Rfast Round
#' @importFrom rlang enquo
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
#' internal function for rapid output as rounded data tables with header
.print_as_dt <- function(
    x, digits = 3, topn = 3, nrows = 8, header = NULL, ...){
  # identify and round numeric variables to 'digits'
  numerics <- which(sapply(x, is.double))
  for (i in numerics) x[[i]] <- Rfast::Round(x[[i]], digits)
  # capture data.table print for width
  print_out <- utils::capture.output(
    print(data.table::as.data.table(x),
          topn = topn, nrows = nrows, trunc.cols = TRUE, ...)
  )
  # process header
  if (!is.null(header)){
    # split into lines
    header <- unlist(strsplit(header, "\n")[[1]])
    # get width of print output
    lwidth <- max(c(sapply(header, nchar), nchar(print_out)))
    # replace \t by filling up with whitespace
    for (i in grep("\t", header)) header[i] <- gsub(
      pattern = "\t",
      replacement = strrep(" ", lwidth - nchar(header[i]) + 1),
      x = header[i])
    # print header
    cat(strrep("-", lwidth),
        header,
        strrep("-", lwidth),
        sep = "\n"
    )
  }
  # print table
  cat(print_out, sep = "\n")
}


#' @keywords internal
#' internal function for data.table rbinds that maintain the class of the input
.rbind_with_class <- function(
    ..., use.names = TRUE, fill = FALSE, idcol = NULL){
  # get list of elements
  l <- list(...)
  # get class of first argument (others are coerced to that type)
  cl <- class(l[[1]])
  # bind rows as in data.table
  l <- lapply(l, function(x) if (is.list(x)) x else
    data.table::as.data.table(x))
  out <- data.table::rbindlist(l, use.names, fill, idcol)
  # re-assign class (rbindlist removes class information)
  class(out) <- cl
  # return output
  out
}
