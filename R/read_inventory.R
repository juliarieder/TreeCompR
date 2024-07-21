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
#' @param size character of length 1 or name of the variable in `inv_source`
#'   containing a generic size-related variable for [CI_size()]. If `NULL`
#'   (default), the variable is not assigned.
#' @param id character of length 1 or name of the variable in `inv_source`
#'   containing a unique tree ID. If `NULL` (default), the function tries to
#'   identify the ID from the data. If this is not possible, the trees are
#'   assigned a unique number. All IDs are coerced to character.
#' @param dbh_unit character of length 1. Unit for the diameter measurements
#'   (one of "cm", "m" or "mm". defaults to "cm").
#' @param height_unit character of length 1. Unit for the diameter measurements
#'   (one of "m", "cm" or "mm". defaults to "m").
#' @param keep_rest logical of length 1. Keep additional variables in the
#'   inventory table besides x, y, dbh, size and id for filtering or further
#'   computations? Defaults to FALSE.
#' @param verbose logical of length 1. Should information about progress be
#'   printed? Defaults to TRUE.
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Function for reading and validating forest inventory data.
#'  Supports any formats readable with [data.table::fread()].
#'
#'  If provided with tabular data without explicitly specified variable names,
#'  the function by default takes the columns named "X" and "Y" (or "x" and
#'  "y") to be the tree coordinates, and looks for columns named "height",
#'  "height_m" or "h" as well as "dbh", "diameter","diam", or "d" (in any
#'  capitalization) as size-related variables. The tree ids are taken from
#'  columns named "id", "tree_id", "treeID" or "tree.id" (in any
#'  capitalization). All special characters besides "." and "_" are stripped
#'  from the column names before matching. For "size", the variable has to be
#'  specified explicitly; if not specified no generic size-related variable
#'  will be selected.
#'  All other columns in the source dataset are appended to the inventory when
#'  `keep_rest = TRUE`, but they are not validated, type-checked or sanitized.
#'
#'  If no columns with coordinates can be identified, the function fails with
#'  an error. If no ID column is available, the function assigns a unique
#'  number to each tree (but note that this will make specifying custom target
#'  trees difficult). It is possible to read in datasets without dbh and
#'  height, but usually only sensible if these are used as accessory datasets
#'  for identifying target trees (e.g. if target trees where identified by
#'  taking their GPS coordinates manually in the field).
#'
#' @return object of class `forest_inv`: a modified data.table with with x and
#'  y coordinates of the tree, a unique tree identifier (`id`) and tree diameter
#'  at breast height (`dbh`, in cm) and tree height (`height`, in m) if
#'  available.
#'
#' @seealso [define_target()] for designating target trees,
#'  [compete_inv()] for computing tree competition from inventory data,
#'  [competition_indices] for a list of available indices,
#'  [plot_target()] to plot target tree positions in `target_inv` and
#'  `compete_inv` objects. Find more examples in our [tutorial](https://juliarieder.github.io/TreeCompR/articles/competition-inventory.html#reading-in-forest-inventory-data-with-read_inv).
#' @export
#'
#' @examples
#' \dontrun{
#' # read inventory with diameter and height units set to m
#' inventory1 <- read_inv(
#'   inv_source = "data/inventory1.csv",
#'   dbh_unit = "m")
#'
#' # read inventory with custom (Spanish) column titles
#' inventory2 <- read_inv(
#'   inv_source = "data/inventory2.csv",
#'   x = utmx,
#'   y = utmy,
#'   dbh = dap_cm,
#'   height = altura,
#'   dbh_unit = "cm",
#'   height_unit = "m")
#'
#'
#' # read inventory with custom decimal and column separators
#' inventory3 <- read_inv(
#'   inv_source = "data/inventory3.csv",
#'   dec = ",",
#'   sep = ";",
#'   verbose = FALSE)
#'
#' # read dataset outside read_inv and filter to a single plot
#' dat <- readr::read_csv("data/inventory4.csv") %>%
#'   dplyr::filter(plot_id == "Plot 1")
#'
#' # use read_inv to convert to a forest_inv object that works with
#' TreeCompR functions
#' inventory4 <- read_inv(
#'   inv_source = dat,
#'   dbh = diam,
#'   dbh_unit = "cm",
#'   height_unit = "m",
#'   verbose = FALSE)
#'
#' # read same inventory without dropping columns
#' inventory4a <- read_inv(
#'   inv_source = dat,
#'   dbh = diam,
#'   dbh_unit = "cm",
#'   height_unit = "m",
#'   keep_rest = TRUE,
#'   verbose = FALSE)
#' }
read_inv <- function(inv_source, x = NULL, y = NULL,
                     dbh = NULL, height = NULL, size = NULL, id = NULL,
                     dbh_unit = c("cm", "m", "mm"),
                     height_unit = c("m", "cm", "mm"),
                     keep_rest = FALSE,
                     verbose = TRUE, ...) {

  # match function arguments for units and get multipliers
  dbh_unit    <- match.arg(dbh_unit)
  height_unit <- match.arg(height_unit)

  # check class of source tree
  if (inherits(inv_source, "data.frame")){
    inv <- .validate_inv(
      inv_source, x = !!enquo(x), y = !!enquo(y), dbh = !!enquo(dbh),
      height = !!enquo(height), size = !!enquo(size), id = !!enquo(id),
      dbh_unit = dbh_unit, height_unit = height_unit,
      keep_rest = keep_rest,
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
      inv <- .validate_inv(
        inv, x = !!enquo(x), y = !!enquo(y), dbh = !!enquo(dbh),
        height = !!enquo(height), size = !!enquo(size), id = !!enquo(id),
        dbh_unit = dbh_unit, height_unit = height_unit,
        keep_rest = keep_rest,
        verbose = verbose)
    }
  }
  # return forst_inv object with correct column number, names and types
  return(inv)
}


#' @keywords internal
#' internal function for the validation of forest inventory data
.validate_inv <- function(inv_source, x = NULL, y = NULL,
                          dbh = NULL, height = NULL, size = NULL, id = NULL,
                          dbh_unit, height_unit,
                          keep_rest = FALSE,
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
      matrix(NA, nrow = nrow(inv_source), ncol = 6)
    )
    names(inv_out) <- c("id", "x", "y", "dbh", "height", "size")
    # validate ID
    inv_out$id <- .get_cols(
      data = inv_source,
      which = !!enquo(id),
      names = c("id", "treeid", "tree_id", "tree.id"),
      class_out = "character",
      alternative = as.character(1:nrow(inv_source))
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
      which = !!enquo(x),
      names = "x",
      class_in = c("integer", "numeric"),
      class_out = "numeric"
    )
    #validate y coordinate
    inv_out$y <- .get_cols(
      data = inv_source,
      which =  !!enquo(y),
      names = "y",
      class_in = c("integer", "numeric"),
      class_out = "numeric"
    )
    # validate dbh
    inv_out$dbh <- .get_cols(
      data = inv_source,
      which =  !!enquo(dbh),
      names = c("dbh", "diameter", "diam", "d"),
      class_in = c("integer", "numeric"),
      class_out = "numeric",
      mult = dbh_mult,
      fail_if_missing = FALSE # scrap column if not available
    )
    # validate tree height
    inv_out$height <- .get_cols(
      data = inv_source,
      which = !!enquo(height),
      names = c("height", "h", "height_m"),
      class_in = c("integer", "numeric"),
      class_out = "numeric",
      mult = height_mult,
      fail_if_missing = FALSE # scrap column if not available
    )
    # validate tree height
    inv_out$size <- .get_cols(
      data = inv_source,
      which = !!enquo(size),
      names = NULL,
      class_in = c("integer", "numeric"),
      class_out = "numeric",
      mult = height_mult,
      fail_if_missing = FALSE # scrap column if not available
    )
    # get original column names
    orig <- sapply(inv_out, function(x) ifelse(
      is.null(attr(x, "original_column")),
      "automatically generated", attr(x, "original_column")))
    # message about used coordinate vectors
    if(verbose){
      message(
        "The following columns were used to create the inventory dataset:\n",
        paste0(names(inv_out), "\t---\t", orig, "\n"))
    }
  }
  # get additional columns
  if (keep_rest){
    # get names of variables to keep
    rest <- setdiff(names(inv_source), orig[orig != "automatically generated"])
    # update output dataset
    inv_out <- cbind(
      inv_out, subset(data.table::as.data.table(inv_source), select = rest)
    )
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
  # quote column specification
  which <- enquo(which)
  # if column name is specified, take this column
  if (!rlang::quo_is_null(which)) {
    #conver to character
    which <- rlang::as_name(which)
    # check if exists
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


# Define printing method for forest_inv objects:
#' @rdname read_inv
#' @format NULL
#' @usage NULL
#' @export
print.forest_inv <- function(x, digits = 3, topn = 3, nrows = 8, ...){
  # get number of data columns
  if (ncol(x) > 3){
    if (ncol(x) > 4) cols <- paste0(" with ", ncol(x) - 3, " data columns")
    else cols <- " with one data column"
  } else cols <- ""
  # prepare header
  header <- paste0(
      "'forest_inv' class inventory dataset \nCollection of ",
      nrow(x)," observation", ifelse(nrow(x) > 1, "s", ""), cols
  )
  # print data.table with trees
  .print_as_dt(x, digits = digits, topn = topn,
               nrows = nrows, header = header, ...)
  # return object invisibly
  invisible(x)
}


# Define rbind method for forest_inv objects:
#' @rdname read_inv
#' @format NULL
#' @usage NULL
#' @export
rbind.forest_inv <- function(
    ..., use.names = TRUE, fill = FALSE, idcol = NULL){
  .rbind_with_class(..., use.names = use.names, fill = fill, idcol = idcol)
}
