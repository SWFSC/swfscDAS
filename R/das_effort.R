#' Summarize DAS effort
#'
#' Chop DAS data into effort segments
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}},
#'  or a data frame that can be coerced to a \code{das_df} object
#' @param method character; method to use to chop DAS data into effort segments
#'   Can be "condition", "equallength", or any partial match thereof (case sensitive)
#' @param conditions character vector of names of conditions to include in segdata output.
#'   These values must be column names from the output of \code{\link{das_process}},
#'   e.g. 'Bft', 'SwellHght', etc.
#'   If \code{method == "condition"}, then these also are the conditions which
#'   trigger segment chopping when they change.
#' @param distance.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be "greatcircle", "lawofcosines", "haversine", "vincenty",
#'   or any partial match thereof (case sensitive).
#'   Default is "greatcircle"
#' @param seg0.drop logical; flag indicating whether or not to drop segments
#'   of length 0 that contain no sighting (S, K, M, G, t) events.
#'   Default is \code{FALSE}
#' @param comment.drop logical; flag indicating if comments ("C" events)
#'   should be ignored (i.e. position information should not be used)
#'   when segment chopping. Default is \code{FALSE}
#' @param event.touse character vector of events to use to determine
#'   segment lengths; overrides \code{comment.drop}.
#'   If \code{NULL} (the default), then all on effort events are used.
#'   If used, this argument must include R, E, S, and A events
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL}, which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}.
#'   Using 1 core likely will be faster for smaller datasets
#' @param ... arguments passed to the specified chopping function,
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
#'   where effort sections run from "R" to "E" events (inclusive),
#'   and then passed to the chopping function specified using \code{method}.
#'   Note that while B events immediately preceding an R are on effort,
#'   they are ignored during effort chopping.
#'   In addition, all on effort events (other than ? and numeric events)
#'   with \code{NA} DateTime, Lat, or Lon values are verbosely removed.
#'
#'   The following chopping methods are currently available:
#'   "condition" and "equallength".
#'   When using the \code{"condition"} method, effort sections are chopped
#'   into segments every time a condition changes,
#'   thereby ensuring that the conditions are consistent across the entire segment.
#'   See \code{\link{das_chop_condition}} for more details about this method,
#'   including arguments that must be passed to it via the argument \code{...}
#'
#'   The "equallength" method consists of
#'   chopping effort sections into equal-length segments of length \code{seg.km},
#'   and doing a weighted average of the conditions for the length of that segment.
#'   See \code{\link{das_chop_equal}} for more details about this method,
#'   including arguments that must be passed to it via the argument \code{...}
#'
#'   The distance between the lat/lon points of subsequent events
#'   is calculated using the method specified in \code{distance.method}.
#'   If "greatcircle", \code{\link{distance_greatcircle}} is used,
#'   while \code{\link[swfscMisc]{distance}} is used otherwise.
#'   See \code{\link{das_sight}} for how the sightings are processed.
#'
#'   The siteinfo data frame includes the column 'included',
#'   which is used in \code{\link{das_effort_sight}} when summarizing
#'   the number of sightings and animals for selected species.
#'   \code{\link{das_effort_sight}} is a separate function to allow users to
#'   personalize the included values as desired for their analysis.
#'   By default, i.e. in the output of this function, 'included' is \code{TRUE} if:
#'   the sighting was made when on effort,
#'   by a standard observer (see \code{\link{das_sight}}),
#'   and in a Beaufort sea state less than or equal to five.
#'
#' @return List of three data frames:
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number, start/end/midpoint coordinates, and conditions (e.g. Beaufort)
#'     \item siteinfo: details for all sightings in \code{x}, including:
#'       the unique segment number it is associated with, segment mid points (lat/lon),
#'       the 'included' column described in the 'Details' section,
#'       and the output information described in \code{\link{das_sight}}
#'     \item randpicks: see \code{\link{das_chop_equal}};
#'       \code{NULL} if using "condition" method
#'   }
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' # Using "condition" method
#' das_effort(
#'   y.proc, method = "condition", conditions = c("Bft", "SwellHght", "Vis"),
#'   seg.min.km = 0.05, num.cores = 1
#' )
#'
#' # Using "equallength" method
#' y.rand <- system.file("das_sample_randpicks.csv", package = "swfscDAS")
#' das_effort(
#'   y.proc, method = "equallength", seg.km = 10, randpicks.load = y.rand,
#'   num.cores = 1
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
das_effort.das_df <- function(x, method = c("condition", "equallength"),
                              conditions = NULL,
                              distance.method = c("greatcircle", "lawofcosines", "haversine", "vincenty"),
                              seg0.drop = FALSE, comment.drop = FALSE, event.touse = NULL,
                              num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!(inherits(seg0.drop, "logical") & inherits(comment.drop, "logical")))
    stop("seg0.drop and comment.drop must both be logicals (either TRUE or FALSE)")

  method <- match.arg(method)
  distance.method <- match.arg(distance.method)

  conditions <- .das_conditions_check(conditions, method)

  if (!is.null(event.touse)) {
    if (comment.drop)
      warning("comment.drop is ignored because event.touse is not NULL")

    if (!all(c("R", "E", "S", "A") %in% event.touse))
      stop("event.use must include at least the following events: ",
           paste(c("R", "E", "S", "A"), collapse = ", "))
  }


  #----------------------------------------------------------------------------
  # Prep for chop functions

  # Remove comments if specified
  if (is.null(event.touse) & comment.drop) x <- x %>% filter(.data$Event != "C")

  # Add index column for adding back in ? and 1:8 events, and extract those events
  x$idx_eff <- seq_len(nrow(x))
  event.tmp <- c("?", 1:8)

  # Filter for continuous effort sections; extract ? and 1:8 events
  #   'on effort + 1' is to capture O/E event.
  x.B.preEff <- which(x$Event == "B")
  x.B.preEff <- x.B.preEff[(x.B.preEff %in% c(1, which(!x$OnEffort) + 1))]

  # Don't use Event == "E" in case there are rogue E events
  x.oneff.which <- sort(unique(c(which(x$OnEffort), which(x$OnEffort) + 1)))
  x.oneff.which <- x.oneff.which[!(x.oneff.which %in% x.B.preEff)]
  stopifnot(all(between(x.oneff.which, 1, nrow(x))))
  rm(x.B.preEff)

  x.oneff.all <- x[x.oneff.which, ]

  x.oneff <- x.oneff.all %>% filter(!(.data$Event %in% event.tmp) )
  x.oneff.tmp <- x.oneff.all %>%
    filter(.data$Event %in% event.tmp) %>%
    mutate(cont_eff_section = NA, dist_from_prev = NA, seg_idx = NA, segnum = NA)

  rownames(x.oneff) <- rownames(x.oneff.tmp) <- NULL
  stopifnot(
    all(x.oneff[!x.oneff$OnEffort, "Event"] == "E"),
    sum(c(nrow(x.oneff), nrow(x.oneff.tmp))) == nrow(x.oneff.all)
  )

  # Verbosely remove remaining data without Lat/Lon/DateTime info
  if (any(is.na(x.oneff$Lat) | is.na(x.oneff$Lon) | is.na(x.oneff$DateTime))) {
    x.nacheck <- x.oneff %>%
      mutate(ll_dt_na = is.na(.data$Lat) | is.na(.data$Lon) | is.na(.data$DateTime),
             sight_na = .data$ll_dt_na & (.data$Event %in% c("S", "K", "M", "G", "t", "A")))

    # Check that no sightings have NA lat/lon/dt info
    if (any(x.nacheck$sight_na))
      stop("One or more sightings at the following line number(s) ",
           "have NA Lat/Lon/DateTime values; ",
           "please fix or remove before processing:\n",
           paste(x.oneff$line_num[x.nacheck$sight_na], collapse = ", "))

    # Remove events with NA lat/lon/dt info
    x.oneff <- x.oneff %>%
      filter(!is.na(.data$Lat) & !is.na(.data$Lon) & !is.na(.data$DateTime))
    message(paste0("There were ", sum(x.nacheck$ll_dt_na), " on effort ",
                   ifelse(comment.drop, "(non-C) ", ""), "events ",
                   "with NA Lat/Lon/DateTime values that will ignored ",
                   "during segment chopping"))

    rm(x.nacheck)
  }

  # For each event, calculate distance to previous event
  if (!is.null(event.touse))
    x.oneff <- x.oneff %>% filter(.data$Event %in% event.touse)

  x.oneff$dist_from_prev <- .dist_from_prev(x.oneff, distance.method)

  # Determine continuous effort sections
  x.oneff$cont_eff_section <- cumsum(x.oneff$Event %in% "R")

  # If specified, verbosely remove cont eff sections with length 0 and no sighting events
  if (seg0.drop) {
    x.ces.summ <- x.oneff %>%
      group_by(.data$cont_eff_section) %>%
      summarise(dist_sum = sum(.data$dist_from_prev[-1]),
                has_sight = any(c("S", "K", "M", "G", "t") %in% .data$Event),
                line_min = min(.data$line_num))
    ces.keep <- filter(x.ces.summ, .data$has_sight | .data$dist_sum > 0)[["cont_eff_section"]]

    x.oneff <- x.oneff %>% filter(.data$cont_eff_section %in% ces.keep)

    x.oneff$cont_eff_section <- cumsum(x.oneff$Event %in% "R")

    message(paste("There were", nrow(x.ces.summ) - length(ces.keep),
                  "continuous effort sections removed because they have a",
                  "length of 0 and contain no sighting events"))
    rm(x.ces.summ, ces.keep)
  }

  if (length(unique(x.oneff$cont_eff_section)) != sum(x.oneff$Event == "R") |
      max(x.oneff$cont_eff_section) != sum(x.oneff$Event == "R"))
    stop("Error in processing continuous effort sections - ",
         "please report this as an issue")


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
    names(x), "dist_from_prev", "cont_eff_section", "seg_idx", "segnum"
  )
  if (!identical(names(x.eff), x.eff.names))
    stop("Error in das_effort: names of x.eff. Please report this as an issue")

  if (!all(x.eff$segnum %in% segdata$segnum))
    stop("Error in das_effort(): creating and processing segement numbers. ",
         "Please report this as an issue")

  # Add back in ? and 1:8 (events.tmp) events
  # Only for siteinfo groupsizes, and thus no segdata info doesn't matter
  x.eff.all <- rbind(x.eff, x.oneff.tmp) %>%
    arrange(.data$idx_eff) %>%
    select(-.data$idx_eff)


  #----------------------------------------------------------------------------
  # Summarize sightings
  siteinfo <- x.eff.all %>%
    left_join(select(segdata, .data$segnum, .data$mlat, .data$mlon),
              by = "segnum") %>%
    das_sight(returnformat = "default") %>%
    mutate(included = (.data$Bft <= 5 & .data$OnEffort & .data$Obs_std),
           included = ifelse(is.na(.data$included), FALSE, .data$included)) %>%
    select(-.data$dist_from_prev, -.data$cont_eff_section)

  # Clean and return
  segdata <- segdata %>% select(-.data$seg_idx)

  siteinfo <- siteinfo %>%
    mutate(year = year(.data$DateTime)) %>%
    select(-.data$seg_idx) %>%
    select(.data$segnum, .data$mlat, .data$mlon, .data$Event,
           .data$DateTime, .data$year, everything())

  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
}
