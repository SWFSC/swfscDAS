#' Process DAS data
#'
#' Process DAS data (the output of \code{\link{das_read}}),
#'   including extracting state and condition information for each DAS event
#'
#' @param x either a \code{das_dfr} object (the output of \code{\link{das_read}}),
#'   or a character (filepath) which is first passed to \code{\link{das_read}}
#' @param ... ignored
#' @param days.gap numeric of length 1; default is \code{10}.
#'   Time gap (in days) used to identify a new cruise in concatenated DAS files,
#'   and thus also when state/condition information
#'   (cruise number, weather, Bft, Mode, etc) is reset
#' @param reset.event logical; default is \code{TRUE}.
#'   Indicates if state/condition information (weather, Bft, Mode, etc) should be reset to \code{NA}
#'   if there is an applicable event with an \code{NA} for that state/condition
#' @param reset.effort logical; default is \code{TRUE}.
#'   Indicates if state/condition information should be reset to \code{NA}
#'   when beginning a new continuous effort section. See Details section
#' @param reset.day logical; default is \code{TRUE}.
#'   Indicates if state/condition information should be reset to \code{NA}
#'   at the beginning of each day. This argument should only
#'   be set to \code{FALSE} for comparison with older methods, such as REPORT
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
#'       #, *, ?, 1, 2, 3, 4, 5, 6, 7, 8, A, B, C, E, F, k, K, M, N, P, Q, R, s, S, t, V, W, G, g, p, X, Y, Z.
#'       The codes G, g (subgroup of a current sighting, and resight of subgroup, resepctively),
#'       p (pinniped sighting), X (to identify an 'object' on the WinCruz map, typically the small RHIB boat),
#'       and Y/Z (biopsy-related position) were added for the sake of the 2014 and 2018 cruise data
#'     \item All '#' events (deleted events) are removed
#'     \item An event is considered 'on effort' if it is 1) an R event,
#'       2) a B event immediately preceding an R event, or 3) between corresponding R and E events
#'       (not including the E event). The 'EffortDot' column is ignored here.
#'     \item All state/condition information is reset at the beginning of each cruise.
#'       New cruises are identifed using \code{days.gap}.
#'     \item All state/condition information relating to B, R, P, V, N, and W events
#'       are reset every time there is a BR event sequence, because
#'       a BR event sequence is (nearly) always a BRPVNW event sequence.
#'       An event sequence means that all of the events have the same Lat/Lon/DateTime info,
#'       and thus previous values for conditions set during the event sequence should not
#'       carry over to any part of the event sequence.
#'     \item 'Mode' is capitalized, and 'Mode' values of \code{NA} are assigned a value of "C"
#'     \item 'EffType' is capitalized, and values of \code{NA} are assigned a value of "S"
#'     \item 'Glare': \code{TRUE} if 'HorizSun' is 11, 12 or 1 and 'VertSun' is 2 or 3,
#'       or if 'HorizSun' is 12 and 'VertSun' is 1;
#'       \code{NA} if 'HorizSun' or 'VertSun' is \code{NA};
#'       otherwise \code{FALSE}
#'     \item Missing values are \code{NA} rather than \code{-1}
#'   }
#'
#'   In WinCruz, a BR or R event series (to indicate starting/resuming effort)
#'   are supposed to be immediately followed by a PVNW event series.
#'   The \code{reset.effort} argument causes the conditions set in the RPVNW event series
#'   (effort mode, Beaufort, visibility, etc.) to be reset to \code{NA} at each BR or R event series
#'
#'   This function was inspired by \code{\link[swfscMisc]{das.read}}
#'
#' @return A \code{das_df} object, which is also a data frame.
#'   It consists of the input data frame, i.e. the output of \code{\link{das_read}},
#'   with the following columns added:
#'   \tabular{lll}{
#'     \emph{State/condition}        \tab \emph{Column name} \tab \emph{Data source}\cr
#'     On/off effort                 \tab OnEffort  \tab B/R and E events\cr
#'     Cruise number                 \tab Cruise    \tab Event: B; Column: Data1\cr
#'     Effort mode                   \tab Mode      \tab Event: B; Column: Data2\cr
#'     Effort type                   \tab EffType   \tab Event: R; Column: Data1\cr
#'     Course (ship direction)       \tab Course    \tab Event: N; Column: Data1\cr
#'     Beaufort sea state            \tab Bft       \tab Event: V; Column: Data1\cr
#'     Swell height (ft)             \tab SwellHght \tab Event: V; Column: Data2\cr
#'     Rain/fog/haze code            \tab RainFog   \tab Event: W; Column: Data1\cr
#'     Horizontal sun (clock system) \tab HorizSun  \tab Event: W; Column: Data2\cr
#'     Vertical sun (clock system)   \tab VertSun   \tab Event: W; Column: Data3\cr
#'     Glare                         \tab Glare     \tab HorizSun and VertSun\cr
#'     Visibility (nm)               \tab Vis       \tab Event: W; Column: Data5\cr
#'   }
#'
#'   Warnings are printed with row numbers of unexpected event codes,
#'   as well as if there is are potential issues with the number and/or order
#'   of R and E events
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
#' @export
das_process.das_dfr <- function(x, days.gap = 10, reset.event = TRUE,
                                reset.effort = TRUE, reset.day = TRUE, ...)
{
  #----------------------------------------------------------------------------
  ### Input checks
  stopifnot(
    inherits(days.gap, c("integer", "numeric")),
    length(days.gap) == 1,
    days.gap > 0,
    inherits(reset.day, "logical")
  )


  #----------------------------------------------------------------------------
  # Prep

  ### Remove '#' events
  das.df <- x[x$Event != "#", ]
  rownames(das.df) <- NULL # for debugging purposes

  ### Determine effort using B/R and E events
  nDAS <- nrow(das.df)

  idx.B <- which(das.df$Event == "B")
  idx.R <- which(das.df$Event == "R")
  idx.E <- which(das.df$Event == "E")

  if (length(idx.E) != length(idx.R)) {
    warning("There are not an equal number of 'R' and 'E' events in the ",
            "provided DAS file; should this be fixed before processing?")
  } else if (!all(idx.E - idx.R > 0) | !all(head(idx.E, -1) < idx.R[-1])) {
    warning("Error: Not all 'R' events are followed by 'E' events before ",
            "another R event; should this be fixed before processing?")
  }


  #----------------------------------------------------------------------------
  # Determine 'reset' rows, i.e. rows that mark a new cruise in concatenated
  #   DAS file, or a new day for conditions
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

  if (!all(idx.new.cruise %in% idx.new.day))
    warning("Warning: not all new cruises row indices were new day indices - ",
            "is the data formatted correctly?",
            immediate. = TRUE)


  #----------------------------------------------------------------------------
  # Add columns for helpful info that is pertinent for a series of records,
  #   such as Beaufort but not sigting cue

  #--------------------------------------------------------
  ### Create vectors with data where values change/are reset
  event.B <- das.df$Event == "B"
  event.N <- das.df$Event == "N"
  event.R <- das.df$Event == "R"
  event.E <- das.df$Event == "E"
  event.V <- das.df$Event == "V"
  event.W <- das.df$Event == "W"

  event.B.preR <- (das.df$Event == "B") & (c(das.df$Event[-1], NA) == "R")

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

  Eff <- as.logical(init.val)
  Eff[sort(unique(c(idx.new.cruise, idx.new.day)))] <- FALSE
  Eff[event.B.preR | event.R] <- TRUE
  Eff[event.E] <- FALSE

  # Additional processing done after for loop

  # Determine reset rows for effort reset
  idx.eff <- sort(unique(c(which(event.B.preR), idx.R)))


  #--------------------------------------------------------
  ### Loop through data for 'carry-over info' that applies to subsequent events
  # idx.new.cruise always includes 1, so don't need to pre-set Last.. objects
  for (i in 1:nDAS) {
    # Reset all info when starting data for a new cruise
    if (i %in% idx.new.cruise) {
      LastEff <- LastEMode <- LastEType <- LastBft <- LastSwH <-
        LastCourse <- LastRF <- LastHS <- LastVS <- LastVis <-
        LastCruise <- NA
    }

    # Reset applicable info (aka all but 'LastCruise') when starting a new day
    if ((i %in% idx.new.day) & reset.day) {
      LastEff <- LastEMode <- LastEType <- LastBft <- LastSwH <-
        LastCourse <- LastRF <- LastHS <- LastVS <- LastVis <- NA
    }

    # Reset applicable info (all RPVNW-related) when starting BR/R event sequence
    if ((i %in% idx.eff) & reset.effort) {
      LastEType <- LastBft <- LastSwH <-
        LastCourse <- LastRF <- LastHS <- LastVS <- LastVis <- NA
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
    if (is.na(Eff[i]))       Eff[i] <- LastEff       else LastEff <- Eff[i]       #Effort
  }


  #--------------------------------------------------------
  ### Post-processing
  tmp <- list(
    Cruise = Cruise, Mode = Mode, Course = Course, EffType = EffType,
    Bft = Bft, SwellHght = SwellHght, RainFog = RainFog,
    HorizSun = HorizSun, VertSun = VertSun, Vis = Vis, OnEffort = Eff
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
  event.acc <- c("*", "?", 1:8, "A", "B", "C", "E", "F", "k", "K", "M", "N",
                 "P", "Q", "R", "s", "S", "t", "V", "W",
                 "G", "g", "p", "X", "Y", "Z")
  if (!all(das.df$Event %in% event.acc))
    warning(paste0("Expected event codes (case sensitive): ",
                   paste(event.acc, collapse = ", "), "\n"),
            "The following rows in the output  (note these ",
            "are NOT necessarily the line numbers of the original file) ",
            "contain unexpected event codes:\n",
            paste(which(!(das.df$Event %in% event.acc)), collapse = ", "))


  #----------------------------------------------------------------------------
  ### Create and order data frame to return
  cols.tokeep <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "Course", "Bft", "SwellHght",
    "RainFog", "HorizSun", "VertSun", "Glare", "Vis",
    paste0("Data", 1:9), "EffortDot", "EventNum", "file_das", "line_num"
  )

  as_das_df(
    select(data.frame(das.df, tmp, stringsAsFactors = FALSE), !!cols.tokeep)
  )
}
