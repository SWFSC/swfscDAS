#' Convert object to a das_df object
#'
#' Convert object to a das_df object
#'
#' @param x object to be converted to a `das_df` object
#'
#' @details todo
#'
#' @return an object of class `das_df`
#'
#' @export
as_das_df <- function(x) UseMethod("as_das_df")

#' @name as_das_df
#' @export
as_das_df.das_df <- function(x) x

#' @name as_das_df
#' @export
as_das_df.data.frame <- function(x) {
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Cruise = "numeric",
    Mode = "character",
    EffType = "character",
    Course = "numeric",
    Bft = "numeric",
    SwellHght = "numeric",
    RainFog = "numeric",
    HorizSun = "numeric",
    VertSun = "numeric",
    Glare = "logical",
    Vis = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    Data8 = "character",
    Data9 = "character",
    EventNum = "numeric",
    file_das = "character",
    line_num = "integer"
  )

  x.class <- lapply(x, class)
  if (!identical(exp.class, x.class)) {
    for (i in seq_along(x)) {
      if (!identical(exp.class[i], x.class[i])) {
        stop("The provided object (x) cannot be coerced to an object of class das_df ",
             "because it does not contain the correct columns. ",
             "Specifically, column ", i, " must be named '", names(exp.class)[i], "' ",
             "and be of class '", exp.class[[i]], "'\n",
             "Was x created using das_process()? See TODO for more details.")
      }
    }
  }

  class(x) <- c("das_df", setdiff(class(x), "das_df"))

  x
}
