#' Chop DAS data - equal length
#'
#' Chop DAS data into equal-length effort segments, averaging conditions by segment
#'
#' @param x \code{das_df} object,
#'   or a data frame that can be coerced to a \code{das_df} object.
#'   This data must be filtered for 'OnEffort' events;
#'   see the Details section below
#' @param seg.km numeric; target segment length in kilometers
#' @param randpicks.load character or \code{NULL}; if character,
#'   filename of past randpicks output to load and use
#'   (passed to \code{file} argument of \code{\link[utils:read.table]{read.csv}}).
#'   \code{NULL} if new randpicks values should be generated
#' @param ... ignored
#'
#' @importFrom dplyr %>% .data between filter left_join mutate select
#' @importFrom stats runif
#' @importFrom swfscMisc distance
#' @importFrom utils head read.csv
#'
#' @details This function is intended to only be called by \code{\link{das_effort}}
#'   when the "equallength" method is specified.
#'   Thus, \code{x} must be filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E";
#'   see \code{\link{das_effort}} for more details.
#'   This function chops each continuous effort section (henceforth 'effort sections')
#'   in \code{x} into modeling segments (henceforth 'segments') of equal length.
#'   Each effort section runs from a "B"/"R" event to its corresponding "E" event.
#'   After chopping, \code{\link{das_segdata_avg}} is called to get relevant
#'   segdata information for each segment.
#'
#'   When chopping the effort sections in segments of length \code{seg.km},
#'   there are several possible scenarios:
#'   \itemize{
#'     \item The extra length remaining after chopping is greater than or equal to
#'       half of the target segment length (i.e. \code{>= 0.5*seg.km}):
#'       the extra length is assigned to a random portion of the effort section as its own segment
#'       (\href{https://github.com/smwoodman/swfscDAS/blob/master/inst/DAS_chop_equal_figures.pdf}{see Fig. 1a})
#'     \item The extra length remaining after chopping is less than half of the
#'       target segment length (i.e. \code{< 0.5*seg.km}):
#'       the extra length is added to one of the (randomly selected) equal-length segments
#'       (\href{https://github.com/smwoodman/swfscDAS/blob/master/inst/DAS_chop_equal_figures.pdf}{see Fig. 1b})
#'     \item The length of the effort section is less than or equal to
#'       the target segment length: the entire segment becomes a segment
#'       (\href{https://github.com/smwoodman/swfscDAS/blob/master/inst/DAS_chop_equal_figures.pdf}{see Fig. 1c})
#'     \item The length of the effort section is zero: a segment of length zero.
#'       If there are more than two events (the "B"/R" and "E" events),
#'       the function throws a warning
#'   }
#'
#'   Therefore, the length of each segment is constrained to be between
#'   one half and one and one half of \code{seg.km} (i.e. \code{0.5*seg.km <=}
#'   segment length \code{>=1.5*seg.km}),
#'   and the central tendency is approximately equal to the target segment length.
#'   The only exception is when a continuous effort section is less than
#'   one half of the target segment length (i.e. \code{< 0.5*seg.km};
#'   \href{https://github.com/smwoodman/swfscDAS/blob/master/inst/DAS_chop_equal_figures.pdf}{see Fig. 1c}).
#'
#'   Note the PDF with Figs. 1a - 1c is included in the package, and can be found at:
#'   \code{system.file("DAS_chop_equal_figures.pdf", package = "swfscDAS")}
#'
#'   'Randpicks' is a record of the random assignments that were made when
#'   chopping the effort sections into segments, and can be saved to allow
#'   users to recreate the same random allocation of extra km when chopping.
#'   The randpicks returned by this function is a data frame with two columns:
#'   the number of the effort section and the randpick value.
#'   Users should save the randpicks output to a CSV file,
#'   which then can be specified using the \code{randpicks.load} argument
#'   to recreate the same effort segments from \code{x}
#'   (i.e., using the same DAS data) in the future.
#'   Note that when saving with \code{\link[utils:read.table]{write.csv}}, users must
#'   specify \code{row.names = FALSE} so that the CSV file only has two columns.
#'   For an example randpicks file, see
#'   \code{system.file("das_sample_randpicks.csv", package = "swfscDAS")}
#'
#'   If the column \code{dist_from_prev} does not exist
#'   (it should be calculated and added to \code{x} in \code{\link{das_effort}}),
#'   then the distance between the lat/lon points of subsequent events
#'   is calculated using \code{\link[swfscMisc]{distance}}, \code{method = "vincenty"}.
#'
#' @return List of three data frames:
#' \itemize{
#'   \item \code{x}, with columns added for the corresponding unique segment code and number
#'   \item segdata: data frame with one row for each segment, and columns with
#'     relevant data (see \code{\link{das_effort}} for specifics)
#'   \item randpicks: data frame with record of length allocations
#'     (see Details section above)
#' }
#'
#' @keywords internal
#'
#' @export
das_chop_equal <- function(x, ...) UseMethod("das_chop_equal")


#' @name das_chop_equal
#' @export
das_chop_equal.data.frame <- function(x, ...) {
  das_chop_equal(as_das_df(x), ...)
}


#' @name das_chop_equal
#' @export
das_chop_equal.das_df <- function(x, seg.km, randpicks.load = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (missing(seg.km))
    stop("You must specify a 'seg.km' argument when using the \"equallength\" ",
         "method. See `?das_chop_equal` for more details")

  if (!all(x$OnEffort | x$Event %in% "E"))
    stop("x must be filtered for on effort events; see `?das_shop_equal")


  #----------------------------------------------------------------------------
  # Calculate distance between points if necessary
  if (!("dist_from_prev" %in% names(x))) {
    if (any(is.na(x$Lat)) | any(is.na(x$Lon))) {
      stop("Error in das_chop_equal: Some unexpected events ",
           "(i.e. not one of ?, 1, 2, 3, 4, 5, 6, 7, 8) ",
           "have NA values in the Lat and/or Lon columns, ",
           "and thus the distance between each point cannot be determined")
    }
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      distance(y1, x1, y2, x2, units = "km", method = "vincenty")
    },
    x1 = head(x$Lon, -1), y1 = head(x$Lat, -1),
    x2 = x$Lon[-1], y2 = x$Lat[-1],
    SIMPLIFY = TRUE)

    x$dist_from_prev <- c(NA, dist.from.prev)
  }


  #----------------------------------------------------------------------------
  # Load randpicks if applicable
  if (is.null(randpicks.load)) {
    r.pos <- NULL
    message("No argument was passed via randpicks.load, and thus new ",
            "randpicks values will be generated")

  } else {
    randpicks.df <- read.csv(randpicks.load)

    if (all(c("effort_section", "randpicks") %in% names(randpicks.df))) {
      r.eff.sect <- randpicks.df$effort_section
      r.pos <- randpicks.df$randpicks

    } else {
      warning("For the provided randpicks CSV file, it is assumed that ",
              "the first column is the continuous effort section numbers, ",
              "and the second column is the randpick values for that ",
              "continuous effort section",
              immediate. = TRUE)
      r.eff.sect <- randpicks.df[[1]]
      r.pos <- randpicks.df[[2]]
    }
  }


  #----------------------------------------------------------------------------
  # ID continuous effort sections, and if appl check against randpicks
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
  if (exists("r.eff.sect")) {
    if (length(eff.uniq) != length(r.eff.sect))
      stop("The provided DAS data (x) does not have the same number of ",
           "continuous effort sections as the provided randpicks file has rows. ",
           "Did you load the correct randpicks file, and does it have ",
           "proper column names? See `?das_chop_equal` for more details")
  }


  #----------------------------------------------------------------------------
  # For each continuous effort section, get segment lengths and segdata
  eff.list <- lapply(eff.uniq, function(i, x, seg.km, r.pos) {
    #------------------------------------------------------
    ### Get lengths of effort segments
    # Prep
    das.df <- filter(x, .data$cont_eff_section == i)
    pos <- r.pos[i]

    das.df$dist_from_prev[1] <- 0 #Ignore distance from last effort

    seg.dist <- sum(das.df$dist_from_prev)
    seg.dist.mod <- seg.dist %% seg.km

    # Determine segment lengths
    if (.equal(seg.dist, 0)) {
      # If current segment length is 0 and there are other events, throw warning
      if (nrow(das.df) > 2)
        warning("The length of continuous effort section ", i, " was zero, ",
                "and there were events between start and end points",
                immediate. = TRUE)

      # EAB makes a 0.1km segment if it includes a sighting - ?
      seg.lengths <- 0
      pos <- NA

    } else {
      if (.less_equal(seg.dist, seg.km)) {
        # If current segment length is less than target length,
        #   only make one segment
        n.subseg <- 1
        if (is.null(pos)) pos <- NA
        seg.lengths <- seg.dist

      } else if (.greater_equal(seg.dist.mod, (seg.km / 2))) {
        # If current segment length is greater than the target length and
        #   remainder is greater than or equal to half of the target length,
        #   the remainder is its own (randomly placed) segment
        n.subseg <- ceiling(seg.dist/seg.km)
        if (is.null(pos)) pos <- ceiling(runif(1, 0, 1) * n.subseg)
        if (is.na(pos) | !between(pos, 1, n.subseg))
          stop("Randpicks value is not in proper range")
        seg.lengths <- rep(seg.km, n.subseg)
        seg.lengths[pos] <- seg.dist.mod

      } else if (.less(seg.dist.mod, (seg.km / 2))) {
        # If current segment length is greater than the target length and
        #   remainder is less than half of the target length,
        #   the remainder added to a random segment
        n.subseg <- floor(seg.dist/seg.km)
        if (is.null(pos)) pos <- ceiling(runif(1, 0, 1) * n.subseg)
        if (is.na(pos) | !between(pos, 1, n.subseg))
          stop("Randpicks value is not in proper range")
        seg.lengths <- rep(seg.km, n.subseg)
        seg.lengths[pos] <- seg.km + seg.dist.mod

      } else {
        stop("Error in das_chop_equal() - unrecognized effort situation. ",
             "Please report this as an issue")
      }
    }


    #------------------------------------------------------
    ### Assign each event to a segment
    subseg.cumsum <- cumsum(seg.lengths)
    das.cumsum <- cumsum(das.df$dist_from_prev)

    das.df$effort_seg <- findInterval(
      round(das.cumsum, 4), round(c(-1, subseg.cumsum), 4),
      left.open = TRUE, rightmost.closed = TRUE
    )
    das.df$seg_idx <- paste0(i, "_", das.df$effort_seg)


    #------------------------------------------------------
    ### Get segdata, and return
    das.df.segdata <- das_segdata_avg(as_das_df(das.df), seg.lengths, i)

    list(das.df = das.df, seg.lengths = seg.lengths, pos = pos,
         das.df.segdata = das.df.segdata)
  }, x = x, seg.km = seg.km, r.pos = r.pos)


  #----------------------------------------------------------------------------
  # Extract information from eff.list, and return

  ### Randpicks; including writing to csv if specified
  randpicks <- data.frame(
    effort_section = eff.uniq,
    randpicks = vapply(eff.list, function(j) j[["pos"]], 1)
  )

  ### Segdata
  segdata <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[["das.df.segdata"]])),
    stringsAsFactors = FALSE
  ) %>%
    mutate(segnum = seq_along(.data$seg_idx),
           dist = round(.data$dist, 4)) %>%
    select(.data$segnum, .data$seg_idx, everything())

  ### Each das data point, along with segnum
  x.eff <- data.frame(
    do.call(rbind, lapply(eff.list, function(i) i[["das.df"]])),
    stringsAsFactors = FALSE
  ) %>%
    left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx")


  #----------------------------------------------------------------------------
  # Return
  list(as_das_df(x.eff), segdata, randpicks)
}
