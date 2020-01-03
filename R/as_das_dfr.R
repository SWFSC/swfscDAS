#' Coerce object to a das_dfr object
#'
#' Check if an object is of class \code{\link{das_dfr}}, or coerce it if possible.
#'
#' @param x A object to be coerced to class \code{das_dfr}
#'
#' @details Currently only data frames can be coerced to an object of class \code{\link{das_dfr}}.
#'   If the \code{x} does not have column names and classes as specified in \code{\link{das_dfr}},
#'   then the function returns an error message detailing the first column that does not
#'   meet the \code{\link{das_dfr}} requirements.
#'
#' @return An object of class `das_dfr`
#'
#' @seealso \code{\link{das_dfr-class}}
#'
#' @export
as_das_dfr <- function(x) UseMethod("as_das_dfr")

#' @name as_das_dfr
#' @export
as_das_dfr.das_dfr <- function(x) x

#' @name as_das_dfr
#' @export
as_das_dfr.data.frame <- function(x) {
  exp.class <- list(
    Event = "character",
    EffortDot = "logical",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    Data8 = "character",
    Data9 = "character",
    EventNum = "integer",
    file_das = "character",
    line_num = "integer"
  )

  x.class <- lapply(x, class)
  if (!identical(exp.class, x.class)) {
    for (i in seq_along(x)) {
      if (!identical(exp.class[i], x.class[i])) {
        stop("The provided object (x) cannot be coerced to an object of class das_dfr ",
             "because it does not contain the correct columns. ",
             "Specifically, column ", i, " must be named '", names(exp.class)[i], "' ",
             "and be of class '", exp.class[[i]], "'\n",
             "Was x created using das_read()? See TODO for more details.")
      }
    }
  }

  class(x) <- c("das_dfr", setdiff(class(x), "das_dfr"))

  x
}
