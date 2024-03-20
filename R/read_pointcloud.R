#' @title Read trees and neighborhoods from point cloud data.
#'
#' @description Read point cloud sourced from a file path or an object that
#'   inherits from class data.frame.
#'
#' @param pc_source object that inherits from class data.frame, or character
#'   path to point cloud of individual tree or a whole plot eitherin las/laz
#'   format or any file format readable with [data.table::fread()]. If provided
#'   with a point cloud object in a data.frame, the structure and column names
#'   are validated and homogenized; else, the function tries to read the point
#'   cloud in the specified path
#' @param verbose logical of length 1. Should information about progress be
#'   printed? Defaults to TRUE.
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Function for reading and validating point cloud data. Currently, the
#'   supported formats are las/laz and any formats readable with
#'   [data.table::fread()]. For other formats, please load the point cloud data
#'   separately and enter the coordinates as a data.frame.
#'
#'   If provided with tabular data (either as a data.frame or via a path to an
#'   [data.table::fread()] readable source), the function by default takes the
#'   columns named "X", "Y", and "Z" or "x", "y", and "z" to be the coordinate
#'   vectors.
#'   If no columns with matching names are available, it takes the first three
#'   numeric columns and returns a message. If the dataset does not contain
#'   three numeric columns or one of the columns labeled x, y, and z is not
#'   numeric, the function fails with an error.
#'
#' @return object of class c("forest_pc", "data.frame") with x, y and z
#'  coordinates of the tree or forest point cloud
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a tree point cloud in txt format
#' tree <- read_pc(pc_source = "path/to/tree_point_cloud.txt")
#' # Read a tree point cloud in las or laz format
#' tree <- read_pc(pc_source = "path/to/tree_point_cloud.las")
#' }
read_pc <- function(pc_source, verbose = TRUE, ...) {
  . <- NULL
  # check class of source dataset
  if (inherits(pc_source, "data.frame")){
    pc <- .validate_pc(pc_source, verbose = verbose)
  } else if (!(is.character(pc_source) && length(pc_source) == 1)){
    stop("Format of pc_source not recognized.\n",
         " Please provide a data.frame or a path to a source file.\n")
  } else if(!file.exists(pc_source)){
    stop("File", pc_source,
         "does not exist. \nPlease check path to point cloud file.")
  } else {
    # treat pc_source as path to file
    path <- pc_source
    # get file extension
    extension <- utils::tail(
      base::strsplit(path, split = ".", fixed = TRUE)[[1]], 1)
    # load las/laz point cloud
    if (extension %in% c("las", "laz")) {
      # Check if lidR package is installed
      if (requireNamespace("lidR", quietly = TRUE)) {
        # If installed, proceed with the code for *.las files
        las <- lidR::readTLSLAS(path)
        pc <- data.frame(x = las$X, y = las$Y, z = las$Z) %>%
          .validate_pc(verbose = verbose)
      } else {
        # If not, return an error message about the missing package
        stop("Please install the 'lidR' package",
             " if you want to use data in las/laz format. \n")
      }
    } else {
      # try loading in point cloud with fread
      pc <- try(
        data.table::fread(file = path, data.table = FALSE, ...)
      )
      if (inherits(pc, "try-error")) {
        # if the file cannot be read, return error message about accepted formats.
        stop("File format cannot be read. ",
             "Please use point cloud with extension las/laz",
             "\n or a format readable by data.table::fread().")
      } else{ # else validate and return
        pc <- .validate_pc(pc, verbose = verbose)
      }
    }
  }
  # return forest_pc object with correct column number, names and types
  return(pc)
}

#' @keywords internal
#' internal function for the validation of point cloud data
.validate_pc <- function(pc, verbose = TRUE){
  # check if pc is already formatted correctly and return if true
  if (inherits(pc, "forest_pc")){
    return(pc)
  } else { #else check for consistency
    # if coordinates are named, use these
    if (all(c("x", "y", "z") %in% tolower(names(pc)))){
      # convert to upper case
      names(pc) <- tolower(names(pc))
      # use these columns
      pc <- pc[, c("x", "y", "z")]
      # test if any of the columns has the wrong class
      if (!all(sapply(pc, is.numeric))){
        stop("One or more of the coordinate vectors x, y, z",
             " is not numeric.\nPlease check raw data.")
      }
    } else{ # else, use the first three numeric columns
      # get numeric columns
      nums <- which(sapply(pc, is.numeric))
      # warn if there are not enough numeric columns
      if (length(nums) < 3){
        stop("Point cloud dataset contains less than 3 numeric coordinate ",
             "vectors.\n Please check raw data.")
      } else {
        # get first three numeric columns
        pc <- pc[,nums[1:3]]
        # message about used coordinate vectors
        if(verbose){
          message(
            "No named coordinates. Columns ",
            paste(names(pc), collapse = ", "),
            " (no. ", paste(nums[1:3], collapse = ", "),
            " in raw data)\n   used as x, y, z coordinates, respectively.")
        }
        # adjust names
        names(pc) <- c("x", "y", "z")
      }
    }
    # set class to forest_pc object
    class(pc) <- c("forest_pc", class(pc))
    # return the validated forest object
    return(pc)
  }
}

# Define printing method for forest_pc objects:
#' @rdname read_pc
#' @format NULL
#' @usage NULL
#' @export
print.forest_pc <- function(x, ...){
  cat("---------------------------------------",
      " \n'forest_pc' class point cloud: \ncollection of", nrow(x),"observations",
      "\n---------------------------------------\n"
  )
  if (nrow(x) < 6) {
    # if there are almost no observations, print the entire dataset
    print(as.data.frame(x), digits = 3)
  } else {
    # else print beginning and end of the data.frame
    temp <- x[1,]
    row.names(temp) <- " "
    for(i in 1:ncol(temp)) temp[, i] <- "..."
    x[, sapply(x, is.numeric)] <- round(x[, sapply(x, is.numeric)], 3)
    print(
      rbind(utils::head(as.data.frame(x), 3),
            temp,
            utils::tail(as.data.frame(x), n = 3)
            )
    )
  }
}
