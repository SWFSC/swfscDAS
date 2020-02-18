#' Summarize DAS effort
#'
#' Chop DAS data into effort segments
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}},
#'  or a data frame that can be coerced to a \code{das_df} object
#' @param method character; method to use to chop DAS data into effort segments
#'   Can be "equallength" or "condition" (case-sensitive)
#' @param sp.codes character; species code(s) to include in segdata
#' @param ... arguments passed to the chopping function specified using \code{method}
#'
#' @importFrom dplyr %>% arrange between bind_cols filter full_join group_by left_join mutate slice summarise
#' @importFrom swfscMisc distance
#' @importFrom utils head
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
#'
#'   TODO
#'   included: On effort and Beaufort less than or equal to 5
#'
#' @return List of three data frames:
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number, start/end/midpoint coordinates, conditions (e.g. Beaufort),
#'       and number of sightings and number of animals on that segment for every species
#'       indicated in \code{sp.codes}.
#'     \item siteinfo: details for all sightings in \code{x}, including:
#'       the unique segment number it is associated with, segment mid points (lat/lon),
#'       and whether the sighting was included in the segdata counts (column \code{included}),
#'       in addition to the other output information described in \code{\link{das_sight}}.
#'     \item randpicks: see \code{\link{das_chop_equal}}
#'   }
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#'
#' y.proc <- das_process(y)
#' das_effort(
#'   y.proc, method = "equallength", sp.codes = c("016", "018"),
#'   seg.km = 10
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
das_effort.das_df <- function(x, method, sp.codes, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  methods.acc <- c("equallength", "condition")
  if (!(length(method) == 1 & (method %in% methods.acc)))
    stop("method must be a string, and must be one of: ",
         paste0("\"", paste(methods.acc, collapse = "\", \""), "\""))


  #----------------------------------------------------------------------------
  # Prep
  # Add index column for adding back in ? and 1:8 events, and extract those evtns
  x$idx <- seq_len(nrow(x))
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
  if (any(is.na(x.oneff$Lat)) | any(is.na(x.oneff$Lon))) {
    stop("Error in das_effort: Some unexpected events ",
         "(i.e. not one of ?, 1, 2, 3, 4, 5, 6, 7, 8) ",
         "have NA values in the Lat and/or Lon columns, ",
         "and thus this data cannot currently be processed")
  }
  dist.from.prev <- mapply(function(x1, y1, x2, y2) {
    distance(y1, x1, y2, x2, units = "km", method = "vincenty")
  },
  x1 = head(x.oneff$Lon, -1), y1 = head(x.oneff$Lat, -1),
  x2 = x.oneff$Lon[-1], y2 = x.oneff$Lat[-1],
  SIMPLIFY = TRUE)

  x.oneff$dist_from_prev <- c(NA, dist.from.prev)


  #----------------------------------------------------------------------------
  # Chop and summarize effort using specified method
  if (method == "equallength") {
    eff.list <- das_chop_equal(as_das_df(x.oneff), ...)
    x.eff <- eff.list[[1]]
    segdata <- eff.list[[2]]
    randpicks <- eff.list[[3]]

  } else if (method == "condition") {
    stop("The condition method is not ready yet")
    eff.list <- das_chop_condition(as_das_df(x.oneff), ...)
    x.eff <- eff.list[[1]]
    segdata <- eff.list[[2]]
    randpicks <- NULL
  }

  # Check that things are as expected
  x.eff.names <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "Course", "Bft", "SwellHght", "RainFog",
    "HorizSun", "VertSun", "Glare", "Vis", "Data1", "Data2",
    "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9",
    "EffortDot", "EventNum", "file_das", "line_num", "idx", "dist_from_prev",
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
    arrange(.data$idx)


  #----------------------------------------------------------------------------
  # Summarize sightings (based on siteinfo) and add applicable data to segdata
  siteinfo <- x.eff.all %>%
    left_join(select(segdata, .data$segnum, .data$mlat, .data$mlon),
              by = "segnum") %>%
    das_sight(mixed.multi = TRUE) %>%
    mutate(included = (.data$Bft <= 5 & .data$OnEffort),
           included = ifelse(is.na(.data$included), FALSE, .data$included)) %>%
    select(-.data$dist_from_prev, -.data$cont_eff_section, -.data$effort_seg)


  # Make data frame with nSI and ANI columns, and join it with segdata
  # TODO: Throw warning if element(s) of sp.codes are not in data?
  sp.codes <- sort(sp.codes)

  segdata.col1 <- select(segdata, .data$seg_idx)
  siteinfo.forsegdata.list <- lapply(sp.codes, function(i, siteinfo, d1) {
    d0 <- siteinfo %>%
      filter(.data$included, .data$Species == i) %>%
      group_by(.data$seg_idx) %>%
      summarise(nSI = length(.data$Species),
                ANI = sum(.data$GsSpecies))

    names(d0) <- c("seg_idx", paste0(i, "_", names(d0)[-1]))

    z <- full_join(d1, d0, by = "seg_idx") %>% select(-.data$seg_idx)
    z[is.na(z)] <- 0

    z
  }, siteinfo = siteinfo, d1 = segdata.col1)

  siteinfo.forsegdata.df <- segdata.col1 %>%
    bind_cols(siteinfo.forsegdata.list)

  segdata <- segdata %>%
    left_join(siteinfo.forsegdata.df, by = "seg_idx")


  #----------------------------------------------------------------------------
  # Return list
  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
}
