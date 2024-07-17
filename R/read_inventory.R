#' @title Read forest inventory data
#'
#' @description Read inventory table sourced from a file path or an object that
#'   inherits from class data.frame.
#'
#' @param inv_source object that inherits from class data.frame, or character
#'   path to a file in any file format readable with [data.table::fread()].
#'   If provided with forest inventory data in a data.frame, the structure and
#'   column names are validated and homogenized; else, the function tries to
#'   read the dataset in the specified path.
#' @param x character of length 1 or name of the variable in `inv_source`
#'   containing the x coordinates of the tree in m. If `NULL` (default),
#'   the function tries to identify the x coordinate from the data.
#' @param y character of length 1 or name of the variable in `inv_source`
#'   containing the y coordinates of the tree in meters. If `NULL` (default),
#'   the function tries to identify the y coordinate from the data.
#' @param dbh character of length 1 or name of the variable in `inv_source`
#'   containing the diameter at breast height of the tree (by default in cm, but
#'   can be defined via `dbh_unit`). If `NULL` (default), the function tries to
#'   identify the dbh from the data.
#' @param height character of length 1 or name of the variable in `inv_source`
#'   containing the height of the tree (by default in m, but   can be defined
#'   via `heigh_unit`). If `NULL` (default), the function tries to identify the
#'   height from the data.
#' @param id character of length 1 or name of the variable in `inv_source`
#'   containing a unique tree ID. If `NULL` (default), the function tries to
#'   identify the ID from the data. If this is not possible, the trees are
#'   assigned a unique number. All IDs are coerced to character.
#' @param dbh_unit character of length 1. Unit for the diameter measurements
#'   (one of "cm", "m" or "mm". defaults to "cm").
#' @param height_unit character of length 1. Unit for the diameter measurements
#'   (one of "m", "cm" or "mm". defaults to "m").
#' @param verbose logical of length 1. Should information about progress be
#'   printed? Defaults to TRUE.
#' @param names_as_is logical of length 1. If TRUE, only NULL or characters
#'   are excepted as column names in `x`, `y`, `dbh`, `height` and `id`, and
#'   the column names are not internally substituted. Defaults to FALSE, which
#'   should normally be the right choice for interactive use (TRUE is only
#'   needed for use within functions).
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Function for reading and validating forest inventory data.
#'    Supports any formats readable with [data.table::fread()].
#'
#'   If provided with tabular data without explicitly specified variable names,
#'   the function by default takes the columns named "X" and "Y" (or "x" and
#'   "y") to be the tree coordinates, and looks for columns named "height",
#'   "height_m" or "h" as well as "dbh", "diameter","diam", or "d" (in any
#'   capitalization) as size-related variables. The tree ids are taken from
#'   columns named "id", "tree_id", "treeID" or "tree.id" (in any
#'   capitalization). All special characters besides "." and "_" are stripped
#'   from the column names before matching.
#'
#'   If no columns with coordinates can be identified, the function fails with
#'   an error. If no ID column is available, the function assigns a unique
#'   number to each tree (but note that this will make specifying custom target
#'   trees difficult). It is possible to read in datasets without dbh and
#'   height, but usually only sensible if these are used as accessory datasets
#'   for identifying target trees (e.g. if target trees where identified by
#'   taking their GPS coordinates manually in the field).
#'
#' @return object of class `forest_inv`: a modified data.table with with x and
#' y coordinates of the tree, a unique tree identifier (`id`) and tree diameter
#'  at breast height (`dbh`, in cm) and tree height (`height`, in m) if
#'  available.
#'
#' @seealso [define_target()] for designating target trees,
#'   [compete_inv()] for computing tree competition from inventory data,
#'   [competition_indices] for a list of available indices,
#'   [plot_target()] to plot target tree positions in `target_inv` and
#'   `compete_inv` objects.
#' @export
#'
#' @examples
#' \dontrun{
#' # prepare inventory table for compete_inv()
#' inv <- fread(path = "path/to/table.csv")
#' #specify the units of parameters within your input
#' inv_table <- read_inv(inv, dbh_unit = "cm", height_unit = "m")
#' # Read inventory table directly from directory
#' inv_table <- read_inv(inv_source = "path/to/table.csv", dbh_unit = "cm",
#' height_unit = "m")
#' }
read_inv <- function(inv_source, x = NULL, y = NULL,
                     dbh = NULL, height = NULL, id = NULL,
                     dbh_unit = c("cm", "m", "mm"),
                     height_unit = c("m", "cm", "mm"),
                     verbose = TRUE, names_as_is = FALSE, ...) {

  # match function arguments for units and get multipliers
  dbh_unit    <- match.arg(dbh_unit)
  height_unit <- match.arg(height_unit)

  # parse names if !names_as_is
  if (!names_as_is){
    # catch and validate variable names (treated as character if not NULL,
    # else, NULL is passed on to .validate_inv())
    if (!is.null(substitute(x)))      x      <- as.character(substitute(x))
    if (!is.null(substitute(y)))      y      <- as.character(substitute(y))
    if (!is.null(substitute(dbh)))    dbh    <- as.character(substitute(dbh))
    if (!is.null(substitute(height))) height <- as.character(substitute(height))
    if (!is.null(substitute(id)))     id     <- as.character(substitute(id))
  }

  # check class of source tree
  if (inherits(inv_source, "data.frame")){
    inv <- .validate_inv(inv_source, x = x, y = y,
                         dbh = dbh, height = height, id = id,
                         dbh_unit = dbh_unit, height_unit = height_unit,
                         verbose = verbose)
  } else if (!(is.character(inv_source) && length(inv_source) == 1)){
    stop(.wr(
      "Format of inv_source not recognized.",
      " Please provide a data.frame or a path to a source file.")
    )
  } else if(!file.exists(inv_source)){
    stop(.wr(
      "File", inv_source,
      "does not exist. Please check path to source file.")
    )
  } else {
    # treat tree_source as path to file
    path <- inv_source
    # try loading inventory with fread
    inv <- try(
      data.table::fread(file = path, data.table = TRUE, ...)
    )
    if (inherits(inv, "try-error")) {
      # if the file cannot be read, return error message about accepted formats.
      stop(.wr(
        "File format cannot be read. Please use a format readable",
        "by data.table::fread() or provide the necessary decimal separators,",
        "field separators etc. for reading.")
      )
    } else{ # else validate and return
      inv <- .validate_inv(inv, x = x, y = y,
                           dbh = dbh, height = height, id = id,
                           dbh_unit = dbh_unit, height_unit = height_unit,
                           verbose = verbose)
    }
  }
  # return forst_inv object with correct column number, names and types
  return(inv)
}


#' @keywords internal
#' internal function for the validation of forest inventory data
.validate_inv <- function(inv_source, x = NULL, y = NULL,
                          dbh = NULL, height = NULL, id = NULL,
                          dbh_unit, height_unit,
                          verbose = TRUE){
  # check if inventory data is already formatted correctly and return if true
  if (inherits(inv_source, "forest_inv")){
    return(inv_source)
  } else { #else check for consistency
    # get multipliers for units
    dbh_mult    <- c(cm = 1, mm = 0.1, m = 100)[dbh_unit]
    height_mult <- c(m = 1, cm = 0.01, mm = 0.001)[height_unit]
    # define empty dataset
    inv_out <- data.table::as.data.table(
      matrix(NA, nrow = nrow(inv_source), ncol = 5)
      )
    names(inv_out) <- c("id", "x", "y", "dbh", "height")
    # validate ID
    inv_out$id <- .get_cols(
      data = inv_source,
      which = id,
      names = c("id", "treeid", "tree_id", "tree.id"),
      class_out = "character",
      alternative = 1:nrow(inv_source)
    )
    # test for forbidden IDs
    forbidden <- inv_out$id %in% c("buff_edge", "exclude_edge", "all_trees")
    if (any(forbidden)){
      stop("Found the following tree ids: ",
           paste(inv_out$id[forbidden], collapse = ", "),
           "\n\n",
        .wr("Trees named 'buff_edge', 'exclude_edge' and 'all_trees'",
          "are not allowed as they interfere with define_target().")
      )
    }

    # validate x coordinate
    inv_out$x <- .get_cols(
      data = inv_source,
      which = x,
      names = "x",
      class_in = c("integer", "numeric"),
      class_out = "numeric"
    )
    #validate y coordinate
    inv_out$y <- .get_cols(
      data = inv_source,
      which = y,
      names = "y",
      class_in = c("integer", "numeric"),
      class_out = "numeric"
    )
    # validate dbh
    inv_out$dbh <- .get_cols(
      data = inv_source,
      which = dbh,
      names = c("dbh", "diameter", "diam", "d"),
      class_in = c("integer", "numeric"),
      class_out = "numeric",
      mult = dbh_mult,
      fail_if_missing = FALSE # scrap column if not available
    )
    # validate tree height
    inv_out$height <- .get_cols(
      data = inv_source,
      which = height,
      names = c("height", "h", "height_m"),
      class_in = c("integer", "numeric"),
      class_out = "numeric",
      mult = height_mult,
      alternative = NULL,
      fail_if_missing = FALSE # scrap column if not available
    )
    # message about used coordinate vectors
    if(verbose){
      # get original column names
      orig <- sapply(inv_out, function(x) ifelse(
        is.null(attr(x, "original_column")),
        "automatically generated", attr(x, "original_column")))

      message(
        "The following columns were used to create the inventory dataset:\n",
        paste0(names(inv_out), "\t---\t", orig, "\n"))
    }
  }

  # set class to forest_inv object
  class(inv_out) <- c("forest_inv", class(inv_out))
  # return the validated inventory object
  return(inv_out)
}


#' @keywords internal
#' internal function for extracting columns with matching names
.get_cols <- function(
    data,                  # dataset
    which,                 # column to be picked
    names,                 # allowed variable names
    class_in = NULL,       # vector of allowed classes for validation
    class_out = NULL,      # output class
    mult = NULL,           # multiplier for unit conversion (NULL: no change)
    alternative = NULL,    # alternative value if column not available
    fail_if_missing = TRUE # if column it missing, fail (TRUE) or remove
){                     # column (FALSE)
  # if column name is specified, take this column
  if (!is.null(which)) {
    if (!which %in% names(data)) stop("No variable named ", which,
                                      " in dataset.")
    out <- data[[which]]
  } else { # try to identify correct column
    # get cleaned column names
    clean_names <- tolower(gsub("[^A-Za-z0-9_.]", "", names(data)))
    # find matching columns
    matches <- clean_names %in% names
    if (sum(matches) > 1){
      stop("More than one variable found with a name ",
           "that is a variation of '", paste(names, collapse = ", "), "'" )
    } else {
      if (sum(matches) == 1) {
        which <- names(data)[matches]
        out   <- data[[which]]
      } else {
        if (!any(matches) & !is.null(alternative)){
          # if an alternative value has been specified, return it
          return(alternative)
        } else {
          # if failure is the extended outcome, return error message
          if (fail_if_missing){
            stop(
              "No variable found with a name ",
              "that is a variation of '", paste(names, collapse = ", "), "'" )
          } else { # else remove column by returning NULL
            return(NULL)
          }
        }
      }
    }
  }
  # validate class and break if needed
  if(!is.null(class_in)){
    if (!inherits(out, class_in)) {
      stop(
        "Variable identified as ", names[1]," ('", which, "') ",
        "not of required class (",
        paste(class_out, collapse = " / "),
        ")"
      )
    }
  }
  # perform unit conversion if needed
  if (!is.null(mult)) out <- out * mult
  # convert to output class
  if (!is.null(class_out)) class(out) <- class_out
  # define attributes
  attr(out, "original_column") <- which
  # return output
  return(out)
}


# Define printing method for forest_pc objects:
#' @rdname read_inv
#' @format NULL
#' @usage NULL
#' @export
print.forest_inv <- function(x, digits = 3, topn = 3, ...){
  cat("---------------------------------------",
      " \n'forest_inv' class inventory dataset: \ncollection of",
      nrow(x),"observations",
      "\n---------------------------------------\n"
  )
  # print data.table with trees
  .print_as_dt(x, digits = digits, topn = topn, ...)
  # return object invisibly
  invisible(x)
}

