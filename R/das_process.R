#' Process DAS data
#'
#' Process DAS data (the output of \code{\link{das_read}}),
#'   including extracting state and condition information for each DAS event
#'
#' @param x either a \code{das_dfr} object (the output of \code{\link{das_read}}),
#'   or a character (filepath) which is first passed to \code{\link{das_read}}
#' @param ... ignored
#' @export
das_process <- function(x, ...) UseMethod("das_process")


#' @name das_process
#' @export
das_process.character <- function(x, ...) {
  das_process(das_read(x), ...)
}


#' @name das_process
#' @export
das_process.data.frame <- function(x, ...) {
  das_process(as_das_dfr(x), ...)
}


#' @name das_process
#' @export
das_process.tbl_df <- function(x, ...) {
  das_process(as_das_dfr(x), ...)
}


#' @name das_process
#'
#' @param days.gap numeric of length 1; time gap (in days) used to identify
#'   a new cruise in concatenated DAS files, and thus also when
#'   state/condition information (weather, Bft, Mode, etc) is reset.
#'   Default is 10 days
#' @param reset.event logical; indicates if state/condition information
#'   (weather, Bft, Mode, etc) should be reset to \code{NA} if there is an
#'   applicable event with an \code{NA} for that state/condition
#' @param reset.day logical; indicates if state/condition information
#'   (weather, Bft, Mode, etc) should be reset to \code{NA} at the beginning of each day.
#'   This argument should only be set to \code{FALSE}
#'   for comparison with older methods, such as Report
#'
#' @importFrom dplyr %>% select
#' @importFrom lubridate day
#' @importFrom rlang !!
#' @importFrom utils head
#'
#' @details If \code{x} is a character, it is assumed to be a filepath and first passed to \code{\link{das_read}}.
#'   This output is then passed to \code{das_process}.
#'
#'   DAS data is event-based, meaning most events indicate when a state or weather condition changes.
#'   For instance, a 'V' event indicates when the Beaufort sea state changes, and
#'   the Beaufort is the same for subsequent events until the next 'V' event.
#'   For each state/condition: a new column is created,
#'   the state/condition information is extracted from relevant events,
#'   and extracted information is propagated to appropriate subsequent rows (events).
#'   Thus, each row in the output data frame contains all
#'   pertinent state/condition information for that row.
#'
#'   The following assumptions/decisions are made during processing:
#'   \itemize{
#'     \item Event codes are expected to be one of the following:
#'       #, *, ?, 1, 2, 3, 4, 5, 6, 7, 8, A, B, C, E, F, k, K, N, P, Q, R, s, S, t, V, W
#'     \item All '#' events (deleted events) are removed
#'     \item An event is considered 'on effort' if it is 1) an R event,
#'       2) a B event immediately preceding an R event, or 3) between corresponding R and E events.
#'       The 'EffortDot' column is ignored
#'     \item All state/condition information is reset at the beginning of each cruise.
#'       New cruises are identifed using \code{days.gap}.
#'     \item 'Mode' is capitalized, and 'Mode' values of \code{NA} are assigned a value of "C"
#'     \item 'EffType' is capitalized, and values of \code{NA} are assigned a value of "S"
#'     \item 'Glare': \code{TRUE} if 'HzSun' is 11, 12 or 1 and 'VtSun' is 2 or 3,
#'       or if 'HzSun' is 12 and 'VtSun' is 1;
#'       \code{NA} if 'HzSun' or 'VtSun' is \code{NA};
#'       otherwise \code{FALSE}
#'     \item Missing values are \code{NA} rather than \code{-1}
#'   }
#'
#'   This function was inspired by \code{\link[swfscMisc]{das.read}}
#'
#' @return A \code{das_df} object, which is also a data frame.
#'   It consists of the input data frame, i.e. the output of \code{\link{das_read}},
#'   with the following columns added:
#'   \tabular{ll}{
#'     \emph{State/condition}        \tab \emph{Column name}\cr
#'     On/off effort                 \tab OnEffort\cr
#'     Cruise number                 \tab Cruise\cr
#'     Effort mode                   \tab Mode\cr
#'     Effort type                   \tab EffType\cr
#'     Course (ship direction)       \tab Course\cr
#'     Beaufort sea state            \tab Bft\cr
#'     Swell height (ft)             \tab SwellHght\cr
#'     Rain/fog/haze code            \tab RainFog\cr
#'     Horizontal sun (clock system) \tab HorizSun\cr
#'     Vertical sun (clock system)   \tab VertSun\cr
#'     Glare                         \tab Glare\cr
#'     Visibility (nm)               \tab Vis\cr
#'   }
#'
#' @seealso For more details about WinCruz, see
#'   \url{https://swfsc.noaa.gov/uploadedFiles/Divisions/PRD/WinCruz.pdf}
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' das_process(y)
#'
#' y.read <- das_read(y)
#' das_process(y.read)
#'
#' @export
das_process.das_dfr <- function(x, days.gap = 10, reset.event = TRUE,
                                reset.day = TRUE, ...)
{
  #----------------------------------------------------------------------------
  ### Input checks
  das.df <- x

  # TODO: verbosely ignore arguments passed via ... ?

  # Input classes
  stopifnot(
    inherits(x, "data.frame"),
    inherits(days.gap, c("integer", "numeric")),
    length(days.gap) == 1,
    days.gap > 0,
    inherits(reset.day, "logical")
  )

  # das.df has expected columns
  # TODO: fix
  das.df.names <- c(
    'Event', 'EffortDot', 'DateTime',
    'Lat', 'Lon', 'Data1', 'Data2', 'Data3', 'Data4', 'Data5', 'Data6',
    'Data7', 'Data8', 'Data9', "file_das", "event_num", 'line_num'
  )
  if (!identical(names(das.df), das.df.names)) {
    warning("x is expected to have the following column names:\n",
            paste(das.df.names, collapse = ", "))
  }


  #----------------------------------------------------------------------------
  # Prep

  ### Remove '#' events
  das.df <- das.df[das.df$Event != "#", ]

  ### Determine effort using B/R and E events
  nDAS <- nrow(das.df)

  ndx.B <- which(das.df$Event == "B")
  ndx.R <- which(das.df$Event == "R")
  ndx.E <- which(das.df$Event == "E")

  if (length(ndx.E) != length(ndx.R)) {
    stop("Error: There are not an equal number of 'R' and 'E' events",
         "in the provided DAS file")
  }
  if (!all(ndx.E - ndx.R > 0) | !all(head(ndx.E, -1) < ndx.R[-1])) {
    stop("Error: Not all 'R' events are followed by 'E' events")
  }

  OnEffort.idx <- unlist(mapply(function(i, j) {
    i:(j-1)
  }, ndx.R, ndx.E, SIMPLIFY = FALSE))
  ndx.B.preR <- ndx.B[(ndx.B + 1) %in% ndx.R]
  OnEffort <- seq_len(nDAS) %in% c(OnEffort.idx, ndx.B.preR)


  #----------------------------------------------------------------------------
  # Determine 'reset' rows, i.e. rows that mark a new cruise in concatenated
  #   DAS file or a new day for conditions
  dt.na <- is.na(das.df$DateTime)

  ### Determine indices where time-date change is by more than 'day.gap' days
  ###   Used to recognize data from new cruise in concatenated DAS files
  time_diff <- rep(NA, nDAS)
  time_diff[!dt.na] <- c(NA, abs(diff(das.df$DateTime[!dt.na]))) / (60*60*24)

  idx.new.cruise <- c(1, which(time_diff > days.gap))

  ### Determine row numbers of new days in das.df;
  ###   these will include idxs of new cruises. Used when reset.day is TRUE
  idx.nona.new <- which(diff(day(das.df$DateTime)[!dt.na]) != 0) + 1
  idx.new.day <- c(1, seq_len(nDAS)[!dt.na][idx.nona.new])

  if (!all(idx.new.cruise %in% idx.new.day)) {
    warning("Warning: not all new cruises row indices were new day indices",
            immediate. = TRUE)
  }


  #----------------------------------------------------------------------------
  # Add columns for helpful info that is pertinent for a series of records,
  #   such as Beaufort but not sigting cue

  #--------------------------------------------------------
  ### Create vectors with data where values change/are reset
  event.B <- das.df$Event == "B"
  event.N <- das.df$Event == "N"
  event.R <- das.df$Event == "R"
  event.V <- das.df$Event == "V"
  event.W <- das.df$Event == "W"

  init.val <- as.numeric(rep(NA, nDAS))
  event.na <- ifelse(reset.event, -9999, NA)

  Cruise    <- .das_process_num(init.val, das.df, "Data1", event.B, event.na)
  Mode      <- .das_process_chr(init.val, das.df, "Data2", event.B, event.na)
  Course    <- .das_process_num(init.val, das.df, "Data1", event.N, event.na)
  EffType   <- .das_process_chr(init.val, das.df, "Data1", event.R, event.na)
  Bft       <- .das_process_num(init.val, das.df, "Data1", event.V, event.na)
  SwellHght <- .das_process_num(init.val, das.df, "Data2", event.V, event.na)
  RainFog   <- .das_process_num(init.val, das.df, "Data1", event.W, event.na)
  HorizSun  <- .das_process_num(init.val, das.df, "Data2", event.W, event.na)
  VertSun   <- .das_process_num(init.val, das.df, "Data3", event.W, event.na)
  Vis       <- .das_process_num(init.val, das.df, "Data5", event.W, event.na)

  # Additional processing done after for loop


  #--------------------------------------------------------
  ### Loop through data for 'carry-over info' that applies to subsequent events
  # idx.new.cruise always includes 1, so don't need to pre-set Last.. objects
  for (i in 1:nDAS) {
    # Reset cruise info when starting data for a new cruise
    if (i %in% idx.new.cruise) {
      LastBft <- LastCourse <- LastEMode <- LastEType <-
        LastHS <- LastVS <- LastOP <- LastRF <- LastSwH <- LastVis <-
        LastCruise <- NA
    }

    # Reset applicable info (aka all but 'LastCruise') when starting a new day
    if ((i %in% idx.new.day) & reset.day) {
      LastBft <- LastCourse <- LastEMode <- LastEType <-
        LastHS <- LastVS <- LastOP <- LastRF <- LastSwH <- LastVis <- NA
    }

    # Set/pass along 'carry-over info'
    if (is.na(Cruise[i]))    Cruise[i] <- LastCruise else LastCruise <- Cruise[i] #Cruise
    if (is.na(Mode[i]))      Mode[i] <- LastEMode    else LastEMode <- Mode[i]    #Mode
    if (is.na(Course[i]))    Course[i] <- LastCourse else LastCourse <- Course[i] #Course
    if (is.na(EffType[i]))   EffType[i] <- LastEType else LastEType <- EffType[i] #Effort type
    if (is.na(Bft[i]))       Bft[i] <- LastBft       else LastBft <- Bft[i]       #Beaufort
    if (is.na(SwellHght[i])) SwellHght[i] <- LastSwH else LastSwH <- SwellHght[i] #Swell height
    if (is.na(RainFog[i]))   RainFog[i] <- LastRF    else LastRF <- RainFog[i]    #Rain or fog
    if (is.na(HorizSun[i]))  HorizSun[i] <- LastHS   else LastHS <- HorizSun[i]   #Horizontal sun
    if (is.na(VertSun[i]))   VertSun[i] <- LastVS    else LastVS <- VertSun[i]    #Vertical sun
    if (is.na(Vis[i]))       Vis[i] <- LastVis       else LastVis <- Vis[i]       #Visibility
  }


  #--------------------------------------------------------
  ### Post-processing
  tmp <- list(
    Cruise = Cruise, Mode = Mode, Course = Course, EffType = EffType,
    Bft = Bft, SwellHght = SwellHght, RainFog = RainFog,
    HorizSun = HorizSun, VertSun = VertSun, Vis = Vis
  )

  # Replace event.reset values with NAs
  if (reset.event) {
    tmp <- lapply(tmp, function(j) {
      j[j == -9999] <- NA
      j
    })
  }

  # Post-for loop variable processing
  tmp$Glare <- ifelse(
    is.na(tmp$HorizSun) | is.na(tmp$VertSun), NA,
    (tmp$HorizSun %in% c(11, 12, 1) & tmp$VertSun %in% c(2, 3)) |
      (tmp$HorizSun %in% 12 & tmp$VertSun %in% 1)
  ) #Per JVR notes, NA Glare should be FALSE to match Abund

  tmp$EffType <- as.character(tmp$EffType)
  tmp$Mode <- as.character(toupper(tmp$Mode))
  # tmp$RainFog <- as.logical(ifelse(is.na(tmp$RainFog), NA, tmp$RainFog %in% c(2:4)))


  #----------------------------------------------------------------------------
  # A couple of warning checks
  event.acc <- c("*", "?", 1:8, "A", "B", "C", "E", "F", "k", "K", "N",
                 "P", "Q", "R", "s", "S", "t", "V", "W")
  if (!all(das.df$Event %in% event.acc))
    warning("The following lines in the DAS file (from line_num in output ",
            "DAS data frame) contain unexpected event codes:\n",
            paste(das.df$line_num[!(das.df$Event %in% event.acc)], collapse = ", "),
            "\nExpected event codes (case sensitive): ",
            paste(event.acc, collapse = ", "))


  #----------------------------------------------------------------------------
  ### Create and order data frame to return
  cols.tokeep <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "Course", "Bft", "SwellHght",
    "RainFog", "HorizSun", "VertSun", "Glare", "Vis",
    paste0("Data", 1:9), "file_das", "event_num", "line_num"
  )

  as_das_df(
    select(data.frame(das.df, tmp, OnEffort, stringsAsFactors = FALSE),
           !!cols.tokeep)
  )
}
