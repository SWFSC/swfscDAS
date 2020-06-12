#' DAS strata - points
#'
#' Determine the strata for swfscDAS outputs
#'
#' @param x a list or a data frame (including an object of class \code{das_df}).
#'   If \code{x} is a list, then it must be the output of
#'   \code{\link{das_effort}} or \code{\link{das_effort_sight}}.
#'   If \code{x} is a data frame, the user must also specify
#'   the columns of \code{x} with the relevant coordinates
#'   using \code{x.lon} and \code{x.lat}.
#' @param ... ignored
#' @param y list of file path(s) of the CSV(s) that contain points defining the strata.
#'   The list may be named; see 'Value' section for how these names are used
#' @param x.lon character; name of the longitude column of \code{x}.
#'   Ignored if \code{x} is a list; default is "Lon"
#' @param x.lat character; name of the latitude column of \code{x}.
#'   Ignored if \code{x} is a list; default is "Lat"
#'
#' @details Assigns DAS event points or segment midpoints to strata polygon.
#'   If \code{x} is a list, then 1) it must be the output of
#'   \code{link{das_effort}} or \code{link{das_effort_sight}} and
#'   2) the segment midpoints (column names mlon and mlat, respectively)
#'   are the points checked if they are in the strata.
#'   If \code{x} is a data frame, then the user must provide the columns
#'   that specify the point coordinates to check.
#'
#'   \code{x} should not be an object of class \code{das_dfr},
#'   or an object of class \code{das_df} created with \code{add.dtll.sight = FALSE},
#'   because the ? and numeric event codes will have NA latitude and longitude values.
#'
#' @return Columns are added to \code{x} indicating if each point was
#'   in (\code{TRUE}) or out (\code{FALSE}) of the corresponding strata polygon.
#'   The names of these columns are the names of \code{y};
#'   the element(s) of \code{y} will have the name InPoly#,
#'   where '#' is the index of that strata polygon in \code{y}.
#'
#'   If \code{x} is a list (i.e. the output of one of the effort functions),
#'   then the strata columns are added to both the segdata and sightinfo data frames.
#'   However, note that the columns added to the sightinfo data frame still indicate
#'   whether or not the corresponding segment midpoint was in the strata,
#'   not the sighting point itself.
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#' y.eff <- das_effort(y.proc, method = "section", num.cores = 1)
#'
#' strata.file <- system.file("das_sample_strata.csv", package = "swfscDAS")
#' das_within_strata(y.eff, list(InPoly = strata.file), x.lon = "Lon", x.lat = "Lat")
#'
#' das_within_strata(y.proc, list(strata.file))
#'
#' \dontrun{
#' # Visualize effort midpoints and strata polygon
#' require(sf)
#' y.eff.strata <- das_within_strata(y.eff, list(InPoly = strata.file))
#' segdata <- st_as_sf(y.eff.strata$segdata, coords = c("mlon", "mlat"), crs = 4326)
#'
#' # Make strata polygon
#' strata.df <- read.csv(strata.file)
#' strata.sfc <- st_sfc(
#'   st_polygon(list(matrix(c(strata.df$Lon, strata.df$Lat), ncol = 2))),
#'   crs = 4326
#' )
#'
#' plot(segdata["InPoly"], axes = TRUE, reset = FALSE,
#'      xlim = c(-137, -142.5), ylim = c(42, 47))
#' plot(strata.sfc, add = TRUE)
#' }
#'
#' @export
das_within_strata <- function(x, ...) UseMethod("das_within_strata")

#' @name das_within_strata
#' @export
das_within_strata.list <- function(x, y, ...) {
  if (!identical(names(x), c("segdata", "sightinfo", "randpicks")))
    stop("If x is a list, then it must be the output of das_effort or ",
         "das_effort_sight. ",
         "Thus, it must contain exactly three named elements: ",
         "segdata, sightinfo, and randpicks (in order)")

  x1 <- das_within_strata(x$segdata, y, "mlon", "mlat")
  names.new <- base::setdiff(names(x1), names(x$segdata))
  x1.tojoin <- x1 %>% select(.data$segnum, !!names.new)

  x2 <- left_join(x$sightinfo, x1.tojoin, by = "segnum")
  # x2 <- das_within_strata(x$sightinfo, y, "mlat", "mlon")

  list(segdata = x1, sightinfo = x2, randpicks = x$randpicks)
}

#' @name das_within_strata
#' @export
das_within_strata.data.frame <- function(x, y, x.lon = "Lon", x.lat = "Lat", ...) {
  # Make x into an sfc object
  if (!all(c(x.lat, x.lon) %in% names(x)))
    stop("x.lon and x.lat must be the names of numeric columns in x")

  x.coords <- data.frame(lon = x[[x.lon]], lat = x[[x.lat]])
  if (!(inherits(x.coords$lon, "numeric") & inherits(x.coords$lat, "numeric")))
    stop("x.lon and x.lat must be the names of numeric columns in x")

  if (any(is.na(x.coords$lon)) | any(is.na(x.coords$lat)))
    stop("The longitude and latitude columns of x must not have NA values")

  x.sfc <- st_wrap_dateline(
    st_geometry(st_as_sf(x.coords, coords = c("lon", "lat"), crs = 4326))
  )

  # Read and coerce y into an sfc object (polygon)
  if (!inherits(y, "list")) stop("y must be a list of (named) file paths")
  y.list <- lapply(y, function(i) {
    tryCatch({
      # This is mostly copied from eSDM::pts2poly_vertices
      y.df <- read.csv(i)
      stopifnot(
        inherits(y.df, "data.frame"),
        ncol(y.df) >= 2,
        is.numeric(y.df[[1]]) & is.numeric(y.df[[2]]),
        identical(is.na(y.df[[1]]), is.na(y.df[[2]]))
      )

      y.df <- y.df %>% select(c(1, 2))
      names(y.df) <- c("lon", "lat")

      if (anyNA(y.df$lon)) {
        obj.list <- y.df %>%
          mutate(na_sum = cumsum(is.na(.data$lon) & is.na(.data$lat))) %>%
          filter(!is.na(.data$lon) & !is.na(.data$lat)) %>%
          group_by(.data$na_sum) %>%
          summarise(temp = list(
            st_polygon(list(matrix(c(.data$lon, .data$lat), ncol = 2)))
          ))

        tmp.out <- st_sfc(obj.list$temp, crs = 4326)

      } else {
        tmp.out <- st_sfc(st_polygon(list(matrix(c(y.df$lon, y.df$lat), ncol = 2))), crs = 4326)
      }

      suppressWarnings(st_wrap_dateline(tmp.out))
    }, error = paste("Unable to read and convert elements of y to sf objects.",
                     "All elements of y must be paths to csv files with at least",
                     "two columns: longitude and latitude, respectively"))
  })

  if (is.null(names(y.list))) names(y.list) <- paste0("InPoly", seq_along(y.list))
  which.blank <- which(names(y.list) == "")
  if (length(which.blank) > 0) names(y.list)[which.blank] <- paste0("InPoly", which.blank)
  rm(which.blank)

  # x in y
  x.in.df <- data.frame(lapply(y.list, function(j, x.sfc) {
    tryCatch({
      stopifnot(inherits(x.sfc, "sfc"), inherits(j, "sfc"))
      vapply(st_intersects(x.sfc, j), length, 1)
    }, error = "Error intersecting x with the elemnts of y")
  }, x.sfc = x.sfc))

  cbind(x, x.in.df)
}
