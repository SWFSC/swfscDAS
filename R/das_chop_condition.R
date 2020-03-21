#' Chop DAS data - condition
#'
#' Chop DAS data into a new effort segment every time a condition changes
#'
#' @param x \code{das_df} object,
#'   or a data frame that can be coerced to a \code{das_df} object.
#'   This data must be filtered for 'OnEffort' events;
#'   see the Details section below
#' @param ... ignored
#' @param dist.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be "greatcircle" to use the great circle distance method,
#'   or one of "lawofcosines", "haversine", or "vincenty" to use
#'   \code{\link[swfscMisc]{distance}}.
#'   Default is \code{NULL} since these distances should have already been
#'   calculated in \code{\link{das_effort}}
#' @param seg.km.min numeric; minimum allowable segment length (in kilometers).
#'   Default is 0.1. See the Details section below for more information
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL} which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}
#'
#' @details This function is intended to only be called by \code{\link{das_effort}}
#'   when the "condition" method is specified.
#'   Thus, \code{x} must be filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E";
#'   see \code{\link{das_effort}} for more details.
#'   This function chops each continuous effort section (henceforth 'effort sections')
#'   in \code{x} into modeling segments (henceforth 'segments') by
#'   creating a new segment every time a condition changes.
#'   Each effort section runs from a B/R event to its corresponding E event.
#'   After chopping, \code{\link{das_segdata_avg}} is called to get relevant
#'   segdata information for each segment.
#'
#'   Changes in the following conditions trigger a new segment:
#'   Beaufort, swell height, rain/fog/haze code, horizontal sun, vertical sun,
#'   and visibility (no glare because glare is dependent on sun positions).
#'   The main exception is when multiple condition changes happen at
#'   the same location, such as a 'BRPVNW' series of events.
#'   When this happens, no segments of length zero are created;
#'   rather, a single segment is created that includes all of the condition changes
#'   (i.e. all of the events in the event series) that happened during
#'   the series of events (i.e. at the same location).
#'
#'   In addition, (almost) all segments whose length is less than \code{seg.km.min}
#'   are combined with the segment immediately following them to ensure that the length
#'   of (almost) all segments is at least \code{seg.km.min}.
#'   This allows users to account for situations where multiple conditions,
#'   such as Beaufort and the visibility, change in rapid succession, say 0.05 km apart.
#'   When segments are combined, a warning is thrown and the conditions are averaged together
#'   across the now-larger segment (question - is this right? See below).
#'   The only exception to this rule is if the short segment ends in an "E" event,
#'   meaning it is the last segment of the effort section.
#'   Since in this case there is no 'next' segment, this segment is left as-is.
#'
#'   Note that the above rule for 'combining' condition changes that have the same location
#'   into a single segment (such as a 'BRPVNW' series of events)
#'   is followed even if \code{seg.km.min = 0}.
#'
#'   If the column \code{dist_from_prev} does not exist, the distance between
#'   subsequent events is calculated as described in \code{\link{das_effort}}
#'
#'   TODO: Make das_segdata_max function so that conditions from tiny segments aren't averaged in
#'
#' @return List of two data frames:
#' \itemize{
#'   \item \code{x}, with columns added for the corresponding unique segment code and number
#'   \item segdata: data frame with one row for each segment, and columns with
#'     relevant data (see \code{\link{das_effort}} for specifics)
#' }
#'
#' @keywords internal
#'
#' @export
das_chop_condition <- function(x, ...) UseMethod("das_chop_condition")


#' @name das_chop_condition
#' @export
das_chop_condition.data.frame <- function(x, ...) {
  das_chop_condition(as_das_df(x), ...)
}


#' @name das_chop_condition
#' @export
das_chop_condition.das_df <- function(x, seg.km.min = 0.1, dist.method = NULL,
                                      num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!all(x$OnEffort | x$Event == "E"))
    stop("x must be filtered for on effort events; see `?das_chop_condition")

  if (!inherits(seg.km.min, c("integer", "numeric")))
    stop("When using the \"condition\" method, seg.km.min must be a numeric. ",
         "See `?das_chop_condition` for more details")

  if (!.greater_equal(seg.km.min, 0))
    stop("seg.km.min must be greater than or equal to 0; ",
         "see `?das_chop_condition")

  #Check for dist.method happens in .dist_from_prev()


  #----------------------------------------------------------------------------
  # Calculate distance between points if necessary
  if (!("dist_from_prev" %in% names(x))) {
    if (is.null(dist.method))
      stop("If the distance between consectutive points (events) ",
           "has not already been calculated, ",
           "then you must provide a valid argument for dist.method")

    x$dist_from_prev <- .dist_from_prev(x, dist.method)
  }

  # Get distance to next point
  x$dist_to_next <- c(x$dist_from_prev[-1], NA)

  #----------------------------------------------------------------------------
  # ID continuous effort sections, then for each modeling segment:
  #   1) chop by condition change
  #   2) aggregate 0-length segments (e.g. tvpaw),
  #   3) aggregate small segments as specified by user
  # x$cont_eff_section <- cumsum(x$Event %in% c("T", "R"))
  x$cont_eff_section <- cumsum(x$Event %in% "R")
  event.B <- x$Event == "B"

  if (all(x$Event[which(event.B) + 1] == "R")) {
    x$cont_eff_section[event.B] <- x$cont_eff_section[event.B] + 1
  } else {
    warning("das_chop_effort event B inconsistency. ",
            "Please report this as an issue",
            immediate. = TRUE)
  }
  eff.uniq <- unique(x$cont_eff_section)
  stopifnot(length(eff.uniq) == sum(x$Event == "R"))

  cond.names <- c(
    "Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Vis"
  )

  # Prep for parallel
  call.x <- x
  call.cond.names <- cond.names
  call.seg.km.min <- seg.km.min
  call.func1 <- das_segdata_avg
  call.func2 <- as_das_df

  # Setup number of cores
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  if(is.na(num.cores)) num.cores <- 1
  num.cores <- max(1, num.cores)
  num.cores <- min(parallel::detectCores() - 1, num.cores)


  # Use parallel to lapply through - modeled after rfPermute
  cl <- swfscMisc::setupClusters(num.cores)
  eff.list <- tryCatch({
    if(is.null(cl)) { # Don't parallelize if num.cores == 1
      lapply(
        eff.uniq, .chop_condition_eff, call.x = call.x,
        call.cond.names = call.cond.names, call.seg.km.min = call.seg.km.min,
        call.func1 = call.func1, call.func2 = call.func2
      )

    } else { # Run lapply using parLapplyLB
      parallel::clusterExport(
        cl = cl,
        varlist = c("call.x", "call.cond.names", "call.seg.km.min",
                    "call.func1", "call.func2"),
        envir = environment()
      )
      parallel::parLapplyLB(
        cl, eff.uniq, .chop_condition_eff, call.x = call.x,
        call.cond.names = call.cond.names, call.seg.km.min = call.seg.km.min,
        call.func1 = call.func1, call.func2 = call.func2
      )
    }
  }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)


  #----------------------------------------------------------------------------
  # Extract information from eff.list, and return

  ### Segdata
  segdata <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[["das.df.segdata"]])),
    stringsAsFactors = FALSE
  ) %>%
    mutate(segnum = seq_along(.data$seg_idx),
           dist = round(.data$dist, 4)) %>%
    select(.data$segnum, .data$seg_idx, everything())

  ### Segment lengths
  x.len <- lapply(eff.list, function(i) i[["seg.lengths"]])

  ### Each DAS data point, along with segnum
  x.eff <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[["das.df"]])),
    stringsAsFactors = FALSE
  ) %>%
    left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx") %>%
    select(-.data$dist_to_next)

  ### Message about segments that were combined
  ###   Must be outside b/c no messages come out of parallel
  segs.message <- na.omit(vapply(eff.list, function(i) i[["segs.combine"]], 1))
  if (length(segs.message) > 0)
    message("Since seg.km.min > 0, ",
            "segments with different conditions were combined ",
            "in the following continuous effort section(s): ",
            paste(segs.message, collapse = ", "))


  #----------------------------------------------------------------------------
  # Return; NULL is for randpicks
  list(as_das_df(x.eff), segdata, NULL)
}



#' @name swfscAirDAS-funcs
#' @param i ignore
#' @param call.x ignore
#' @param call.cond.names ignore
#' @param call.seg.km.min ignore
#' @param call.func1 ignore
#' @param call.func2 ignore
#' @export
.chop_condition_eff <- function(i, call.x, call.cond.names, call.seg.km.min,
                                call.func1, call.func2) {
  ### Inputs
  # i: Index of current continuous effort section
  # call.x: das data frame
  # call.cond.names: Names of condition columns to use to chop;
  #   i.e., if there's a change in one of these columns, create new segment
  # call.seg.km.min: seg.km.min argument from das_chop_condition()
  # call.func1: _segdata_ function - needs to be passed in since
  #   this function is used by swfscAirDAS as well
  # call.func2: as_..._df function

  ### Output
  # List with, for this continuous effort section:
  #   1) DAS data frame, 2) segment lengths, and 3) segdata

  #------------------------------------------------------
  # Prep
  das.df <- filter(call.x, .data$cont_eff_section == i)

  # Ignore distance from last effort
  das.df$dist_from_prev[1] <- 0
  # Ignore distance past this continuous effort section
  das.df$dist_to_next[nrow(das.df)] <- 0


  #------------------------------------------------------
  ### Determine indices of condition changes, and combine as needed
  cond.list <- lapply(call.cond.names, function(j) {
    which(c(NA, head(das.df[[j]], -1) != das.df[[j]][-1]))
  })
  cond.idx.pre <- sort(unique(c(1, unlist(cond.list))))

  effort.seg.pre <- rep(FALSE, nrow(das.df))
  effort.seg.pre[cond.idx.pre] <- TRUE

  das.df$effort_seg_pre <- cumsum(effort.seg.pre)
  das.df$idx <- seq_len(nrow(das.df))

  # Get distances of current effort sections
  d.pre <- das.df %>%
    group_by(.data$effort_seg_pre) %>%
    summarise(idx_start = min(.data$idx),
              idx_end = max(.data$idx),
              dist_length = sum(.data$dist_to_next))

  # == 0 check is here in case seg.km.min is 0
  seg.len0 <- d.pre$idx_end[.equal(d.pre$dist_length, 0)] + 1
  seg.len1 <- d.pre$idx_end[.less(d.pre$dist_length, call.seg.km.min)] + 1

  seg.diff <- setdiff(seg.len1, seg.len0)
  segs.combine <- if (length(seg.diff) > 0 & all(seg.diff <= nrow(das.df))) {
    i
  } else {
    NA
  }

  idx.torm <- sort(unique(c(seg.len0, seg.len1)))

  # Remove segment breaks that create too-small segments
  #   Ignores idx.torm values > nrow(das.df)
  cond.idx <- cond.idx.pre[!(cond.idx.pre %in% idx.torm)]
  effort.seg <- rep(FALSE, nrow(das.df))
  effort.seg[cond.idx] <- TRUE

  das.df <- das.df %>%
    select(-.data$effort_seg_pre, -.data$idx) %>%
    mutate(effort_seg = cumsum(effort.seg),
           seg_idx = paste(i, .data$effort_seg, sep = "_"))


  #------------------------------------------------------
  ### Calculate lengths of effort segments
  d <- das.df %>%
    group_by(.data$effort_seg) %>%
    summarise(sum_dist = sum(.data$dist_to_next))

  seg.lengths <- d$sum_dist

  #------------------------------------------------------
  ### Get segdata and return
  # TODO: develop non-avg function
  # das.df.segdata <- das_segdata_avg(as_das_df(das.df), seg.lengths, i)
  das.df.segdata <- call.func1(call.func2(das.df), seg.lengths, i)

  list(
    das.df = das.df, seg.lengths = seg.lengths,
    das.df.segdata = das.df.segdata,
    segs.combine = segs.combine
  )
}
