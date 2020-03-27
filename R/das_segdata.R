#' Summarize DAS data for a continuous effort section
#'
#' Summarize DAS effort data by effort segment, while averaging conditions
#'
#' @param x \code{das_df} object,
#'   or a data frame that can be coerced to a \code{das_df} object.
#'   Must contain a single continuous effort section of DAS data;
#'   see the Details section below
#' @param conditions see \code{\link{das_effort}}, or
#'   see Details section for more information
#' @param segdata.method character; either \code{"avg"} or \code{"maxdist"}.
#'   \code{"avg"} means the condition values will be
#'   calculated as a weighted average by distance, while
#'   \code{"maxdist"} means the condition values will be those recorded
#'   for the longest distance during that segment
#' @param seg.lengths numeric; length of the modeling segments
#'   into which \code{x} will be chopped
#' @param section.id numeric; the ID of \code{x} (the current continuous effort section)
#' @param ... ignored
#'
#' @details This function was designed to be called by \code{\link{das_chop_equal}};
#'   users should avoid calling it themselves.
#'   It loops through the events in \code{x}, chopping \code{x} into modeling segments
#'   while calculating and storing relevant information for each segment.
#'   Because \code{x} is a continuous effort section, \code{x} must begin with
#'   a "B" or "R" event and end with the corresponding "E" event.
#'
#'   For each segment, this function reports the segment number,
#'   segment ID, cruise number, the start/end/midpoints (lat/lon), segment length,
#'   year, month, day, time, mode, effort type,
#'   effective strip width sides (number of sides searched), and average conditions.
#'   The segment ID is designated as \code{eff_id} _ index of the modeling segment.
#'   Thus, if \code{section.id} is \code{1}, then the segment ID for
#'   the second segment from \code{x} is \code{"1_2"}.
#'
#'   The average condition values are calculated as a weighted average by distance,
#'   and reported for the conditions specified by the \code{conditions} argument.
#'   For logical columns (e.g. Glare), the reported value is the percentage
#'   (in decimals) of the segment in which that condition was \code{TRUE}.
#'
#'   Cruise number, mode, effort type, sides searched, and file name are
#'   also included in the segdata output.
#'   These values should all be consistent across the entire effort section,
#'   and thus across all segments in \code{x};
#'   a warning is printed if there are any inconsistencies
#'
#'   \code{\link[swfscMisc]{bearing}} and \code{\link[swfscMisc]{destination}}
#'   are used to calculate the segment start, mid, and end points.
#'   TODO: option to pass distance method to swfscMisc functions?
#'   How does EAB/Eiren do these calculations?
#'
#' @return Data frame with the segdata information described above
#'   and in \code{\link{das_effort}}
#'
#' @keywords internal
#'
#' @export
das_segdata <- function(x, ...) UseMethod("das_segdata")


#' @name das_segdata
#' @export
das_segdata.data.frame <- function(x, ...) {
  das_segdata(as_das_df(x), ...)
}


#' @name das_segdata
#' @export
das_segdata.das_df <- function(x, conditions, segdata.method,
                               seg.lengths, section.id, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  conditions.acc <- c(
    "Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Glare", "Vis"
  )
  if (!all(conditions %in% conditions.acc))
    stop("Was this function called by a _chop_ function? ",
         "Please ensure all components of the conditions argument are ",
         "one of the following accepted values:\n",
         paste(conditions.acc, collapse  = ", "))


  segdata.method.acc <- c("avg", "maxdist")
  if (!(segdata.method %in% segdata.method.acc))
    stop("Was this function called by a _chop_ function? ",
         "segdata.method must be one of the following:\n",
         paste(segdata.method.acc, collapse = ", "))

  if (!("dist_from_prev" %in% names(x)))
    stop("x must contain a 'dist_from_prev' column; ",
         "was this function called by a _chop_ function?")

  stopifnot(
    inherits(seg.lengths, c("numeric", "integer")),
    inherits(section.id, c("numeric", "integer"))
  )

  if (!.equal(sum(seg.lengths), sum(x$dist_from_prev)))
    stop("The sum of the seg.lengths values does not equal the sum of the ",
         "x$dist_from_prev' values; ",
         "was this function called by a _chop_ function?")

  rm(conditions.acc, segdata.method.acc)


  #----------------------------------------------------------------------------
  # Prep stuff
  das.df <- x

  ### Prep - get the info that is consistent for the entire effort length
  # ymd determined below to be safe
  df.out1.cols <- c("file_das", "Cruise", "Mode", "EffType", "ESWsides")

  # <=1 accounts for when all
  df.out1.check <- vapply(df.out1.cols, function(i) {
    length(unique(na.omit(das.df[[i]]))) <= 1
  }, as.logical(1))
  if (!all(df.out1.check))
    warning("Is there an error in the data? ",
            "Not all of the following data were consistent across ",
            "continuous effort section ", section.id, ":\n",
            paste(df.out1.cols, collapse  = ", "))

  df.out1 <- das.df %>%
    select(!!df.out1.cols) %>%
    select(file = .data$file_das, everything()) %>%
    slice(n()) #use n() instead of 1 b/c some vars may be NA in first line


  #----------------------------------------------------------------------------
  segdata.all <- .segdata_proc(
    das.df = das.df, conditions = conditions, segdata.method = segdata.method,
    seg.lengths = seg.lengths, section.id = section.id, df.out1 = df.out1
  )

  #----------------------------------------------------------------------------
  segdata.all %>%
    select(.data$seg_idx, .data$Cruise, .data$file, .data$stlin, .data$endlin,
           .data$lat1, .data$lon1, .data$lat2, .data$lon2,
           .data$mlat, .data$mlon, .data$dist,
           .data$mDateTime, .data$year, .data$month, .data$day, .data$mtime,
           everything())
}





#' @name swfscAirDAS-internals
#' @param das.df ignore
#' @param conditions ignore
#' @param segdata.method ignore
#' @param seg.lengths ignore
#' @param section.id ignore
#' @param df.out1 ignore
#' @export
.segdata_proc <- function(das.df, conditions, segdata.method,
                          seg.lengths, section.id, df.out1) {
  #----------------------------------------------------------------------------
  ### Prep
  # Conditions list
  conditions.list.init <- lapply(seq_along(conditions), function(i) {
    data.frame(val = NA, dist = 0, stringsAsFactors = FALSE)
  })
  names(conditions.list.init) <- conditions


  # Calculate objects for for loop
  n.subseg <- length(seg.lengths)
  subseg.cumsum <- cumsum(seg.lengths)
  subseg.mid.cumsum <- (c(0, head(subseg.cumsum, -1)) + subseg.cumsum) / 2

  if (!("dist_from_prev_cumsum" %in% names(das.df)))
    das.df$dist_from_prev_cumsum <- cumsum(das.df$dist_from_prev)



  # 'Initialize' necessary objects
  subseg.curr <- 1
  stlin.curr <- das.df$line_num[1]
  startpt.curr <- c(das.df$Lat[1], das.df$Lon[2])
  midpt.curr <- NULL
  segdata.all <- NULL

  conditions.list <- conditions.list.init

  if (!(nrow(das.df) >= 2))
    stop("x must have at least 2 rows; please report this as an issue")


  #----------------------------------------------------------------------------
  ### Step through each point in effort length,
  #     calculating segment midpoints, endpoints, and avg conditions as you go
  for (j in 2:nrow(das.df)) {
    # t1 and t2: Is point j past the segment midpt or endpt, respectively,
    #   i.e. do we need to calculate the midpt or endpt?
    dist.pt.curr <- das.df$dist_from_prev_cumsum[j]
    t1 <- .greater_equal(dist.pt.curr, subseg.mid.cumsum[subseg.curr])
    t2 <- .greater_equal(dist.pt.curr, subseg.cumsum[subseg.curr])

    if (!t2) {
      # If we didn't cross a segment endpoint, get
      #   1) the percentage of the segment between j-1 and j, and
      #   2) the condition and sight info
      seg.perc <- das.df$dist_from_prev[j] / seg.lengths[subseg.curr]
      conditions.list <- .segdata_aggr(conditions.list, das.df, j-1, seg.perc)
      rm(seg.perc)
    }

    # While the current subsegment midpoint or endpoint
    #   comes before the next event (which is indexed by j)
    while((t1 & is.null(midpt.curr)) | t2) {
      ### Make objects for values used multiple times (pt2)
      # Needs to be here for when there are multiple trips through while loop
      dist.subseg.curr <- subseg.cumsum[subseg.curr]
      dist.subseg.prev <- subseg.cumsum[subseg.curr - 1]
      dist.pt.curr     <- das.df$dist_from_prev_cumsum[j]
      dist.pt.prev     <- das.df$dist_from_prev_cumsum[j-1]

      ### Get data
      # Calculate midpoint (if not already done for this segment)
      if (t1 & is.null(midpt.curr)) {
        midpt.curr <- destination(
          das.df$Lat[j-1], das.df$Lon[j-1],
          bearing(das.df$Lat[j-1], das.df$Lon[j-1], das.df$Lat[j], das.df$Lon[j])[1],
          units = "km",
          distance = subseg.mid.cumsum[subseg.curr] - dist.pt.prev
        )
      }

      # Calculate endpoint
      if (t2) {
        ### Destination calculated from das.df[j-1, ], so d calc is ok
        ###   (destination calculates the endpoint, not the seg length)
        d <- dist.subseg.curr - dist.pt.prev
        endpt.curr <- destination(
          das.df$Lat[j-1], das.df$Lon[j-1],
          bearing(das.df$Lat[j-1], das.df$Lon[j-1], das.df$Lat[j], das.df$Lon[j])[1],
          units = "km", type = "vincenty", distance = d
        )

        ### Conditions and sightings
        #     d.tmp handles multiple segments between pts, aka when current
        #     segment start point is closer than [j-1]
        d.tmp <- max(dist.pt.prev, dist.subseg.prev)
        d.rat <- (dist.subseg.curr - d.tmp) / seg.lengths[subseg.curr]
        # if (is.nan(d.rat)) d.rat <- NA
        conditions.list <- .segdata_aggr(conditions.list, das.df, j-1, d.rat)
        rm(d, d.tmp, d.rat)

        ## If next point is at the same location, don't end the segment yet
        if (j < nrow(das.df)) {
          if (das.df$dist_from_prev[j+1] == 0) {
            # tmp1 is dist from current point to end of last segment
            tmp1a <- ifelse(subseg.curr > 1, subseg.cumsum[subseg.curr-1], 0)
            tmp1 <- dist.pt.curr - tmp1a
            tmp2 <- seg.lengths[subseg.curr]

            if (.less_equal(tmp1, tmp2)) {break}
            rm(tmp1a, tmp1, tmp2)
          }
        }

        ### Store data from this segment
        # Get condition information
        if (segdata.method == "avg") {
          conditions.list.df <- data.frame(
            lapply(names(conditions.list), function(k, k.list) {
              if (inherits(k.list[[k]]$val, "character")) {
                paste(unique(na.omit(k.list[[k]]$val)), collapse = ";")

              } else {
                # This must be a numeric because .segdata_aggr()
                #   throws an error if not
                tmp <- k.list[[k]] %>%
                  filter(!is.na(.data$val)) %>%
                  mutate(val_frac = .data$val * .data$dist)

                if (nrow(tmp) == 0) NA else round(sum(tmp$val_frac) / sum(tmp$dist), 2)
              }
            }, k.list = conditions.list),
            stringsAsFactors = FALSE
          )

        } else if (segdata.method == "maxdist") {
          conditions.list.df <- data.frame(
            lapply(names(conditions.list), function(k, k.list) {
              tmp <- k.list[[k]] %>%
                filter(!is.na(.data$val)) %>%
                group_by(.data$val) %>%
                summarise(dist_sum = sum(.data$dist)) %>%
                arrange(desc(.data$dist_sum), .data$val)

              if (nrow(tmp) == 0) NA else tmp$val[1]
            }, k.list = conditions.list),
            stringsAsFactors = FALSE
          )

        } else {
          stop("Unrecognized segdata.method; please report this as an issue")
        }

        names(conditions.list.df) <- paste0(
          # switch(segdata.method, avg = "ave", maxdist = "maxdist"),
          segdata.method, names(conditions.list)
        )

        # Get start line
        j.stlin.curr <- which(das.df$line_num == stlin.curr)

        # Add segdata to .all data frame
        segdata <- data.frame(
          seg_idx = paste0(section.id, "_", subseg.curr),
          stlin = stlin.curr, endlin = das.df$line_num[j],
          lat1 = startpt.curr[1], lon1 = startpt.curr[2],
          lat2 = endpt.curr[1], lon2 = endpt.curr[2],
          mlat = midpt.curr[1], mlon = midpt.curr[2],
          mDateTime = mean(c(das.df$DateTime[j.stlin.curr], das.df$DateTime[j])),
          dist = seg.lengths[subseg.curr],
          stringsAsFactors = FALSE
        ) %>%
          mutate(mtime = strftime(.data$mDateTime, format = "%H:%M:%S",
                                  tz = tz(.data$mDateTime)),
                 year = year(.data$mDateTime), month = month(.data$mDateTime),
                 day = day(.data$mDateTime)) %>%
          bind_cols(df.out1, conditions.list.df)

        segdata.all <- rbind(segdata.all, segdata)
        rm(conditions.list.df, j.stlin.curr, segdata) #obs.vals


        ### Prep for next segment
        if (j == nrow(das.df) & subseg.curr == n.subseg) {
          # If at the end of das.df and all segs have been processed, break
          break

        } else {
          # Else, prep for next segment:
          # Increment
          subseg.curr <- subseg.curr + 1

          # Reset/set points as appropriate
          startpt.curr <- endpt.curr
          midpt.curr <- NULL
          endpt.curr <- NULL
          stlin.curr <- das.df$line_num[j]

          t1 <- .greater_equal(dist.pt.curr, subseg.mid.cumsum[subseg.curr])
          t2 <- .greater_equal(dist.pt.curr, subseg.cumsum[subseg.curr])

          # If pt j is before the next seg endpoint, get data from endpt to j
          #   Else, this info is calculated in t2 section above
          tmp1 <- das.df$dist_from_prev_cumsum[j] - subseg.cumsum[subseg.curr - 1]
          tmp2 <- seg.lengths[subseg.curr]
          conditions.list <- conditions.list.init

          if (.less(tmp1, tmp2)) {
            conditions.list <- .segdata_aggr(
              conditions.list.init, das.df, j-1, tmp1 / tmp2
            )
          }
          rm(tmp1, tmp2)
        }
      }
    }
  }

  # Return something
  segdata.all
}




#' @name swfscAirDAS-internals
#' @param data.list ignore
#' @param curr.df ignore
#' @param idx ignore
#' @param dist.perc ignore
#' @export
.segdata_aggr <- function(data.list, curr.df, idx, dist.perc) {
  stopifnot(
    all(names(data.list) %in% names(curr.df)),
    idx <= nrow(curr.df)
  )

  if (is.na(dist.perc)) {
    warning("dist.perc is NA, ignoring")
    data.list    #lapply(data.list, function(i) NA)

  } else if (dist.perc == 0) {
    data.list

  } else {
    tmp <- lapply(names(data.list), function(k) {
      rbind(data.list[[k]], c(curr.df[[k]][idx], dist.perc))
    })
    names(tmp) <- names(data.list)
    tmp
  }
}
