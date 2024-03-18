#' @title Read forest inventory data
#'
#' @description Read point cloud sourced from a file path or an object that
#'   inherits from class data.frame.
#'
#' @param inv_source object that inherits from class data.frame, or character
#'   path to a file in any file format readable with [data.table::fread()].
#'   If provided with forest inventory data in a data.frame, the structure and
#'   column names are validated and homogenized; else, the function tries to
#'   read the dataset in the specified path.
#' @param x character of length 1 or name of the variable in `inv_source`
#'   containing the x coordinates of the tree in meters . If `NULL (default),
#'   the function tries to identify the x coordinate from the data.
#' @param y character of length 1 or name of the variable in `inv_source`
#'   containing the y coordinates of the tree in meters. If `NULL (default), the
#'   function tries to identify the y coordinate from the data.
#' @param dbh character of length 1 or name of the variable in `inv_source`
#'   containing the diameter at breast height of the tree (by default in cm, but
#'   can be defined via `dbh_unit`). If `NULL` (default), the function tries to
#'   identify the dbh from the data.
#' @param height character of length 1 or name of the variable in `inv_source`
#'   containing the height of the tree (by default in m, but   can be defined
#'   via `h_unit`). If `NULL` (default), the function tries to identify the dbh
#'   from the data.
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
#' @param ... additional arguments passed on to [data.table::fread()]
#'
#' @details Function for reading and validating forest inventory data.
#'    Supports any formats readable with [data.table::fread()].
#'
#'   If provided with tabular data without explicitly specified variable names,
#'   the function by default takes the columns named "X" and "Y" (or "x" and
#'   "y") to be the tree coordinates, and looks for columns named "height",
#'   "height_m" or "h" as well as "dbh", "diameter","diam", or "d" (in any
#'   capitalization) as size-related variables. The coordinates are taken from
#'   columns named "id", "tree_id", "treeID" or "tree.id" (in any
#'   capitalization).
#'
#'   If no columns with coordinates can be identified, the function fails with
#'   an error. If no ID column is available, the function assigns a unique
#'   number to each tree (but note that this will make specifying custom target
#'   trees difficult). It is possible to read in datasets without dbh and
#'   height, but usually only sensible if these are used as accessory datasets
#'   for identifying target trees (e.g. if target trees where identified by
#'   taking their GPS coordinates manually in the field).
#'
#' @return object of class `c("forest_inv", "data.frame")` with x and y
#'  coordinates of the tree, a unique tree identifier (`id`) and tree diameter
#'  at breast height (`dbh`, in cm) and tree height (`height`, in m) if
#'  available.
#' @export
#'
#' @examples
#' \dontrun{
#' # prepare inventory table for compete_inv()
#' inv <- fread(path = "path/to/table.csv")
#' inv_table <- read_inv(inv, dbh_unit = "cm", height_unit = "m")
#' # Read inventory table from directory
#' inv_table <- read_inv(inv_source = "path/to/table.csv", dbh_unit = "cm",
#' height_unit = "m")
#' }
read_inv <- function(inv_source, x = NULL, y = NULL,
                     dbh = NULL, height = NULL, id = NULL,
                     dbh_unit = c("cm", "m", "mm"),
                     height_unit = c("m", "cm", "mm"),
                     verbose = TRUE, ...) {

  # match function arguments for units and get multipliers
  dbh_unit    <- match.arg(dbh_unit)
  height_unit <- match.arg(height_unit)

  # catch and validate variable names (treated as character if not NULL,
  # else, NULL is passed on to .validate_inv())
  if (!is.null(substitute(x)))      x      <- as.character(substitute(x))
  if (!is.null(substitute(y)))      y      <- as.character(substitute(y))
  if (!is.null(substitute(dbh)))    dbh    <- as.character(substitute(dbh))
  if (!is.null(substitute(height))) height <- as.character(substitute(height))
  if (!is.null(substitute(id)))     id     <- as.character(substitute(id))

  # check class of source tree
  if (inherits(inv_source, "data.frame")){
    inv <- .validate_inv(inv_source, x = x, y = y,
                         dbh = dbh, height = height, id = id,
                         dbh_unit = dbh_unit, height_unit = height_unit,
                         verbose = verbose)
  } else if (!(is.character(inv_source) && length(inv_source) == 1)){
    stop("Format of inv_source not recognized.\n",
         " Please provide a data.frame or a path to a source file.\n")
  } else if(!file.exists(inv_source)){
    stop("File", inv_source,
         "does not exist. \nPlease check path to point cloud file.")
  } else {
    # treat tree_source as path to file
    path <- inv_source
    # try loading in point cloud with fread
    inv <- try(
      data.table::fread(file = path, data.table = FALSE, ...)
    )
    if (inherits(inv, "try-error")) {
      # if the file cannot be read, return error message about accepted formats.
      stop("File format cannot be read. Please use a format readable ",
           "by data.table::fread() or provide the necessary decimal separators,",
           " field separators etc. for reading.")
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
#' internal function for the validation of point cloud data
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
    inv_out <- as.data.frame(matrix(NA, nrow = nrow(inv_source), ncol = 5))
    names(inv_out) <- c("id", "x", "y", "dbh", "height")
    # validate ID
    inv_out$id <- as.character(
      .get_cols(
        data = inv_source,
        which = id,
        names = c("id", "treeid", "tree_id", "tree.id"),
        alternative = 1:nrow(inv_source)
      )
    )
    # validate x coordinate
    inv_out$x <- .get_cols(
      data = inv_source,
      which = x,
      names = "x",
      class = c("integer", "numeric")
    )
    #validate y coordinate
    inv_out$y <- .get_cols(
      data = inv_source,
      which = y,
      names = "y",
      class = c("integer", "numeric")
    )
    # validate dbh
    inv_out$dbh <- .get_cols(
      data = inv_source,
      which = dbh,
      names = c("dbh", "diameter", "diam", "d"),
      class = c("integer", "numeric"),
      mult = dbh_mult,
      fail_if_missing = FALSE # scrap column if not available
    )
    # validate tree height
    inv_out$height <- .get_cols(
      data = inv_source,
      which = height,
      names = c("height", "h", "height_m"),
      class = c("integer", "numeric"),
      mult = height_mult,
      alternative = NULL,
      fail_if_missing = FALSE # scrap column if not available
    )
    # message about used coordinate vectors
    if(verbose){
      message(
        "The following columns were used to create the inventory dataset:\n",
        # this is absolutely hacky and ugly and should be replaced by something
        # more sensible ASAP. I just wanted a working version before I go to bed.
        paste(
          sapply(1:ncol(inv_out), function(i)
            paste0(names(inv_out)[i], "\t---\t",
                   names(inv_source)[
                     apply(
                       inv_source, 2, function(x)
                         is.logical(
                           all.equal(as.vector(x), inv_out[,i])
                         )
                     )
                   ]
            )
          ), collapse = "\n"
        )
      )
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
    class = NULL,          # class vector for validation
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
    matches <- tolower(names(data)) %in% names
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
  if(!is.null(class)){
    if (!inherits(out, class)) {
      stop(
        "Variable identified as ", names[1]," ('", which, "') ",
        "not of required class (",
        paste(class, collapse = " / "),
        ")"
      )
    }
  }
  # perform unit conversion if needed
  if (!is.null(mult)) out <- out * mult
  # return output
  return(out)
}



# Define printing method for forest_pc objects:
#' @rdname read_inv
#' @format NULL
#' @usage NULL
#' @export
print.forest_inv <- function(x, ...){
  cat("---------------------------------------",
      " \n'forest_inv' class inventory dataset: \ncollection of", nrow(x),"observations",
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
      rbind(head(as.data.frame(x), 3),
            temp,
            tail(as.data.frame(x), n = 3)
      )
    )
  }
}

