#' Summarize DAS effort
#'
#' Chop DAS data into effort segments
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}},
#'  or a data frame that can be coerced to a \code{das_df} object
#' @param method character; method to use to chop DAS data into effort segments
#'   Can be \code{"condition"} or \code{"equallength"} (case-sensitive)
#'   to usex \code{\link{das_chop_condition}} or \code{\link{das_chop_condition}},
#'   respectively
#' @param sp.codes character; species code(s) to include in segdata output
#' @param conditions character vector of names of conditions to include in segdata output.
#'   These values must be column names from the output of \code{\link{das_process}},
#'   e.g. 'Bft', 'SwellHght', etc.
#'   If \code{method == "condition"}, then these also are the conditions which
#'   trigger segment chopping when they change.
#' @param dist.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be \code{"greatcircle"} to use the great circle distance method (TODO - add ref),
#'   or one of \code{"lawofcosines"}, \code{"haversine"},
#'   or \code{"vincenty"} to use
#'   \code{\link[swfscMisc]{distance}}. Default is \code{"greatcircle"}
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL}, which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}.
#'   Using 1 core likely will be faster for smaller datasets
#' @param ... arguments passed to the chopping function specified using \code{method},
#'   such as \code{seg.km} or \code{seg.min.km}
#'
#' @details This is the top-level function for chopping processed DAS data
#'   into modeling segments (henceforth 'segments'), and assigning sightings
#'   and related information (e.g., weather conditions) to each segment.
#'   This function returns data frames with all relevant information for the
#'   effort segments and associated sightings ('segdata' and 'siteinfo', respectively).
#'   Before chopping, the DAS data is filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column "E".
#'   In other words, the data is filtered for continuous effort sections (henceforth 'effort sections'),
#'   where effort sections run from "B"/"R" to "E" events (inclusive),
#'   and then passed to the chopping function specified using \code{method}.
#'   All on effort events that are not one of ?, 1, 2, 3, 4, 5, 6, 7, or 8
#'   must have non-\code{NA} Lat and Lon values.
#'
#'   The following chopping methods are currently available:
#'   \code{"condition"} and \code{"equallength"}.
#'   When using the \code{"condition"} method, effort sections are chopped
#'   into segments every time a condition changes,
#'   thereby ensuring that the conditions are consistent across the entire segment.
#'   See \code{\link{das_chop_condition}} for more details about this method,
#'   including arguments that must be passed to it via \code{...}.
#'
#'   The \code{"equallength"} method consists of
#'   chopping effort sections into equal-length segments of length \code{seg.km},
#'   and doing a weighted average of the conditions for the length of that segment.
#'   See \code{\link{das_chop_equal}} for more details about this method,
#'   including arguments that must be passed to it via \code{...}.
#'
#'   The sightings included in the segdata counts sightings that were made when
#'   on effort and in a Beaufort sea state less than or equal to five.
#'   Included sightings are those with a \code{TRUE} value in the 'included'
#'   column in siteinfo (described below).
#'   TODO: Allow user to specify this.
#'
#'   The distance between the lat/lon points of subsequent events
#'   is calculated using the method specified in \code{dist.method}.
#'   See \code{\link{das_sight}} for how the sightings are processed.
#'
#' @return List of three data frames:
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number, start/end/midpoint coordinates, conditions (e.g. Beaufort),
#'       and number of sightings and number of animals on that segment for every species
#'       indicated in \code{sp.codes}.
#'     \item siteinfo: details for all sightings in \code{x}, including:
#'       the unique segment number it is associated with, segment mid points (lat/lon),
#'       whether the sighting was included in the segdata counts (column name 'included'),
#'       and the output information described in \code{\link{das_sight}}.
#'     \item randpicks: see \code{\link{das_chop_equal}}.
#'       \code{NULL} if using "condition" method.
#'   }
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' # Using "condition" method
#' das_effort(
#'   y.proc, method = "condition", sp.codes = c("016", "018"),
#'   seg.min.km = 0.05, num.cores = 1
#' )
#'
#' # Using "equallength" method
#' y.rand <- system.file("das_sample_randpicks.csv", package = "swfscDAS")
#' das_effort(
#'   y.proc, method = "equallength", sp.codes = c("016", "018"),
#'   seg.km = 10, randpicks.load = y.rand, num.cores = 1
#' )
#'
#' @export
das_effort <- function(x, ...) UseMethod("das_effort")


#' @name das_effort
#' @export
das_effort.data.frame <- function(x, ...) {
  das_effort(as_das_df(x), ...)
}


#' @name das_effort
#' @export
das_effort.das_df <- function(x, method, sp.codes, conditions = NULL,
                              dist.method = "greatcircle", num.cores = NULL,
                              ...) {
  #----------------------------------------------------------------------------
  # Input checks
  methods.acc <- c("equallength", "condition")
  if (!(length(method) == 1 & (method %in% methods.acc)))
    stop("method must be a string of length one, and must be one of: ",
         paste0("\"", paste(methods.acc, collapse = "\", \""), "\""))

  #Check for dist.method happens in .dist_from_prev()

  # Conditions
  conditions.acc <- c(
    "Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Glare", "Vis"
  )

  if (is.null(conditions)) {
    conditions <- if (method == "condition") {
      c("Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Glare", "Vis")
    } else {
      c("Bft", "SwellHght", "HorizSun", "VertSun", "Glare", "Vis")
      #TODO: RainFog?
    }

  } else {
    if (!all(conditions %in% conditions.acc))
      stop("Please ensure all components of the conditions argument are ",
           "one of the following accepted values:\n",
           paste(conditions.acc, collapse  = ", "))
  }


  #----------------------------------------------------------------------------
  # Prep
  # Add index column for adding back in ? and 1:8 events, and extract those events
  x$idx_eff <- seq_len(nrow(x))
  event.tmp <- c("?", 1:8)

  # Filter for continuous effort sections; extract ? and 1:8 events
  #   'on effort + 1' is to capture O/E event.
  x.oneff.which <- sort(unique(c(which(x$OnEffort), which(x$OnEffort) + 1)))
  stopifnot(all(between(x.oneff.which, 1, nrow(x))))

  x.oneff.all <- x[x.oneff.which, ]

  x.oneff <- x.oneff.all %>% filter(!(.data$Event %in% event.tmp))
  x.oneff.tmp <- x.oneff.all %>%
    filter(.data$Event %in% event.tmp) %>%
    mutate(dist_from_prev = NA, cont_eff_section = NA,
           effort_seg = NA, seg_idx = NA, segnum = NA)

  rownames(x.oneff) <- rownames(x.oneff.tmp) <- NULL
  stopifnot(
    all(x.oneff[!x.oneff$OnEffort, "Event"] == "E"),
    sum(c(nrow(x.oneff), nrow(x.oneff.tmp))) == nrow(x.oneff.all)
  )

  # For each event, calculate distance to previous event
  x.oneff$dist_from_prev <- .dist_from_prev(x.oneff, dist.method)

  #----------------------------------------------------------------------------
  # Chop and summarize effort using specified method
  eff.list <- if (method == "equallength") {
    das_chop_equal(as_das_df(x.oneff), conditions = conditions,
                   num.cores = num.cores, ...)
  } else if (method == "condition") {
    das_chop_condition(as_das_df(x.oneff), conditions = conditions,
                       num.cores = num.cores, ...)
  } else {
    stop("method is not an accepted value")
  }

  x.eff <- eff.list[[1]]
  segdata <- eff.list[[2]]
  randpicks <- eff.list[[3]]

  # Check that things are as expected
  x.eff.names <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "ESWsides", "Course", "Bft", "SwellHght",
    "RainFog", "HorizSun", "VertSun", "Glare", "Vis", "Data1", "Data2",
    "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9",
    "EffortDot", "EventNum", "file_das", "line_num", "idx_eff", "dist_from_prev",
    "cont_eff_section", "effort_seg", "seg_idx", "segnum"
  )
  if (!identical(names(x.eff), x.eff.names))
    stop("Error in das_effort: names of x.eff. ",
         "Please report this as an issue")

  if (!all(x.eff$segnum %in% segdata$segnum))
    stop("Error in das_effort(): Error creating and processing ",
         "segement numbers. Please report this as an issue")

  # Add back in ? and 1:8 (events.tmp) events
  # Only for siteinfo groupsizes, and thus no segdata info doesn't matter
  x.eff.all <- rbind(x.eff, x.oneff.tmp) %>%
    arrange(.data$idx_eff) %>%
    select(-.data$idx_eff)


  #----------------------------------------------------------------------------
  # Summarize sightings (based on siteinfo) and add applicable data to segdata
  siteinfo <- x.eff.all %>%
    left_join(select(segdata, .data$segnum, .data$mlat, .data$mlon),
              by = "segnum") %>%
    das_sight(mixed.multi = TRUE) %>%
    filter(.data$Event == "S") %>%
    mutate(included = (.data$Bft <= 5 & .data$OnEffort),
           included = ifelse(is.na(.data$included), FALSE, .data$included)) %>%
    select(-.data$dist_from_prev, -.data$cont_eff_section, -.data$effort_seg)


  # Make data frame with nSI and ANI columns, and join it with segdata
  # TODO: Throw warning if element(s) of sp.codes are not in data?
  sp.codes <- sort(sp.codes)

  segdata.col1 <- select(segdata, .data$seg_idx)
  siteinfo.forsegdata.list <- lapply(sp.codes, function(i, siteinfo, d1) {
    d0 <- siteinfo %>%
      filter(.data$included, .data$Sp == i) %>%
      group_by(.data$seg_idx) %>%
      summarise(nSI = length(.data$Sp),
                ANI = sum(.data$GsSp))

    names(d0) <- c("seg_idx", paste0(i, "_", names(d0)[-1]))

    z <- full_join(d1, d0, by = "seg_idx") %>% select(-.data$seg_idx)
    z[is.na(z)] <- 0

    z
  }, siteinfo = siteinfo, d1 = segdata.col1)

  siteinfo.forsegdata.df <- segdata.col1 %>%
    bind_cols(siteinfo.forsegdata.list)

  # Format outputs as desired
  segdata <- segdata %>%
    left_join(siteinfo.forsegdata.df, by = "seg_idx")

  siteinfo <- siteinfo %>%
    select(-.data$seg_idx) %>%
    select(.data$segnum, .data$mlat, .data$mlon, everything())


  #----------------------------------------------------------------------------
  # Return list
  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
}




#' @name swfscAirDAS-internals
#' @param z ignore
#' @param z.dist.method ignore
#' @export
.dist_from_prev <- function(z, z.dist.method) {
  ### Inputs
  # z: data frame of class das_df
  # z.dist.method: dist.method from das_effort()

  ### Output: numeric of distance (km) to previous event; first element is NA

  # Input check
  dist.methods.acc <- c("greatcircle", "lawofcosines", "haversine", "vincenty")
  if (!(length(z.dist.method) == 1 & (z.dist.method %in% dist.methods.acc)))
    stop("dist.method must be a string of length one, and must be one of: ",
         paste0("\"", paste(dist.methods.acc, collapse = "\", \""), "\""))

  # Check for NA Lat/Lon
  z.llna <- which(is.na(z$Lat) | is.na(z$Lon))
  if (length(z.llna) > 0)
    stop("Error in das_effort: Some unexpected events ",
         "have NA values in the Lat and/or Lon columns, ",
         "and thus the distance between each point cannot be determined. ",
         "Please remove or fix these events before running this function. ",
         "These events are in the following lines of the original file:\n",
         paste(z$line_num[z.llna], collapse = ", "))

  # Calcualte distances
  if (identical(z.dist.method, "greatcircle")) {
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      .fn.grcirclkm(y1, x1, y2, x2)
    },
    y1 = head(z$Lat, -1), x1 = head(z$Lon, -1), y2 = z$Lat[-1], x2 = z$Lon[-1],
    SIMPLIFY = TRUE)

  } else if (z.dist.method %in% c("lawofcosines", "haversine", "vincenty")) {
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      distance(y1, x1, y2, x2, units = "km", method = z.dist.method)
    },
    y1 = head(z$Lat, -1), x1 = head(z$Lon, -1), y2 = z$Lat[-1], x2 = z$Lon[-1],
    SIMPLIFY = TRUE)

  } else {
    stop("Error in distance calcualtion - ",
         "please pass an accepted argument to dist.method")
  }

  # Return distances, with inital NA since this are distances from previous point
  c(NA, dist.from.prev)
}



#' @name swfscAirDAS-internals
#' @param lat1 ignore
#' @param lon1 ignore
#' @param lat2 ignore
#' @param lon2 ignore
#' @export
.fn.grcirclkm <- function(lat1, lon1, lat2, lon2) {
  # FUNCTION to calculate the great circle distance (in km) between two lat/lons
  # From EAB and KAF

  R <- pi/180      #angle in radians = angle in degrees * R
  D <- 180/pi      #angle in degrees = angle in radains * D
  dist <- 0

  NAcheck <- sum(is.na(c(lat1, lon1, lat2, lon2)))
  if (NAcheck == 0) {             #only continue if no NA positions
    if ((lat1 != lat2) | (lon1 != lon2))  {
      dlat1 <- lat1 * R              # convert to radian values:
      dlng1 <- lon1 * R
      dlat2 <- lat2 * R
      dlng2 <- lon2 * R
      las <- sin(dlat1) * sin(dlat2);   # compute distance
      lac <- cos(dlat1) * cos(dlat2) * cos(dlng1 - dlng2)
      laf <- las + lac
      if (laf < -1) {
        laf <- -1
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)))
      } else if (laf < 1) {
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)));
      } else {
        stop('laf value out of bounds')
      }
      dist <- (dacos * D * 60) * 1.852           #calculate distance in km
    }
  }

  dist
}
