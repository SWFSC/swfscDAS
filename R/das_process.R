#' Process DAS data
#'
#' Process output of \link{das_read}
#'
#' @param x Either a data frame (the output of \link{das_read}) or
#'   a character which is then passed to \link{das_read}
#' @param ... Ignore
#' @export
das_process <- function(x, ...) UseMethod("das_process")


#' @name das_process
#' @export
das_process.character <- function(x, ...) {
  das_process(das_read(x), ...)
}


#' @name das_process
#'
#' @param days.gap numeric of length 1; time gap (in days) used to identify
#'   a new cruise in concatenated DAS files, and thus also when propogated
#'   info (weather/env, observers, etc) is reset.
#'   The default is 10 days
#' @param reset.event logical; indicates if condition/effort info
#'   (Bft, Mode, etc) should be reset to NA if there is an
#'   applicable event with an NA for that value
#' @param reset.day logical; indicates if condition/effort info
#'   (e.g. Bft, Mode, etc) should be reset to NA at the beginning of each day.
#'   This argument should only be used (i.e. be \code{FALSE}
#'   for comparison with older methods, such as Report
#' @param cruise.name character of length 1; name of cruise, e.g. 'MOPS'
#' @param cruise.num character or numeric of length 1; cruise number, e.g. 989.
#'   If \code{NA}, the cruise number will be determined using the data for
#'   as many rows as possible
#' @param ship.name character of length 1; abbr name of ship, e.g. 'DSJ'
#'
#' @importFrom dplyr select
#' @importFrom rlang !!
#' @importFrom utils head
#'
#' @details
#'   das_process() is an S3 that either calls das_read() on a character vector,
#'   or das_read() data frame as input and outputs a data frame with additional columns, e.g. for Mode or Bft
#'   TODO: describe columns created?
#'   TODO: check for events..?
#'   # TODO 1? If/how to incorporate these assumptions
#'   Effort assumptions; Specified by Jeff Moore June 2018 for CruzPlot for sake of early cruises
#'     \code{Mode[is.na(Mode)] <- "C"}
#'     \code{EffType[is.na(EffType)] <- "S"}
#'
#'  TODO 2? Add warning() checks for unexpected values, e.g. for EffType?
#'    any(!(EffType %in% c("S", "N", "F", NA)))
#'    Ties into broader question of how much data checking should this do?
#'
#'   Assumptions made during processing
#'   \itemize{
#'     \item Determines effort using both '.' (OnEffort) and R and E events (OnEffort_RE)
#'     \item Resets all data at the beginning of each cruise; a new cruise is identifed using \code{days.gap}
#'     \item Mode letter is capitalized
#'     \item Glare: TRUE if HzSun = 11, 12 or 1 and VtSun = 2 or 3, or if HzSun = 12 and VtSun = 1; otherwise FALSE.
#'     \item If Glare is NA, i.e. if HorizSun or VertSun is NA, then Glare is FALSE
#'     \item RainFog is TRUE if 2, 3, or 4; otherwise FALSE
#'   }
#'   Note that das_process returns missing values as \code{NA} rather than \code{-1}
#'
#' @return data frame with 'carry-over info' columns added (e.g., Mode and Bft)
#'   OnEffort_RE is On/Off effort as determiend by R and E events: from R event
#'     to the event before the next E event are considered on effort
#'   Columns added to data frame:
#'     CruiseName, Cruise (cruise #), Shipname, OnEffort_RE, Mode, EffType,
#'     Bft, SwellHght, RainFog, HorizSun, VertSun, Glare, Vis, Course
#'
#' @seealso For more details about WinCruz and expected DAS data format, see
#'   \url{https://swfsc.noaa.gov/uploadedFiles/Divisions/PRD/WinCruz.pdf}
#'
#' @examples
#' # TODO
#' # x <- das_read("Data/1986-2006ETP.das")
#' # y <- das_process(x, reset.day = TRUE, days.gap = 10)
#' # y <- das_process("../DAS_files/RV-Data/1986/MOPS0989.das")
#'
#' @export
das_process.data.frame <- function(
  x, days.gap = 10, reset.event = TRUE, reset.day = TRUE,
  cruise.name = NA, cruise.num = NA, ship.name = NA, ...)
{
  #----------------------------------------------------------------------------
  ### Input checks
  das.df <- x

  # TODO: verbosely ignore arguments passed via ... ?

  # Argument length
  if (!all(sapply(list(cruise.name, cruise.num, ship.name, days.gap), length) == 1)) {
    stop("All arguments (except for x) must be of length 1")
  }

  # Input classes
  stopifnot(
    inherits(x, "data.frame"),
    inherits(days.gap, c("integer", "numeric")),
    days.gap > 0,
    inherits(reset.day, "logical"),
    is.na(cruise.name) | inherits(cruise.name, "character"),
    is.na(cruise.num) | inherits(cruise.num, c("integer", "numeric")),
    is.na(ship.name) | inherits(ship.name, "character")
  )

  # das.df has expected columns
  das.df.names <- c(
    'Event', 'EffortDot', 'DateTime', #'Yr', 'Mo', 'Da', 'Hr', 'Min',
    'Lat', 'Lon', 'Data1', 'Data2', 'Data3', 'Data4', 'Data5', 'Data6',
    'Data7', 'Data8', "file_das", "event_num", 'line_num'
  )
  if (!identical(names(das.df), das.df.names)) {
    warning("x is expected to have the following column names:\n",
            paste(das.df.names, collapse = ", "))
  }

  # # Cruise name, cruise number, and ship name are in expected list
  # if (!is.na(cruise.name) & !(cruise.name %in% c("MOPS", "STAR"))) {
  #   warning("cruise.name is expected to be one of either ",
  #           "\"MOPS\" or \"STAR\"")
  # }
  #
  # cruise.num.exp <- c(
  #   989, 990, 1080, 1081, 1164, 1165, 1267, 1268, 1369, 1370, 1610, 1611,
  #   1612, 1613, 1614, 1615, 1616, 1623, 1624, 1630, 1631
  # )
  # if (!is.na(cruise.num) & !(cruise.num %in% cruise.num.exp)) {
  #   warning("cruise.num is expected to be one of:\n",
  #           paste(cruise.num.exp, collapse = ", "))
  # }
  #
  # if (!is.na(ship.name) & !(ship.name %in% c("DSJ", "END", "MAC", "MACII"))) {
  #   warning("ship.name is expected to be one of:\n",
  #           "\"DSJ\", \"END\", \"MAC\", \"MACII\"")
  # }


  #----------------------------------------------------------------------------
  # Prep; determine R to E effort
  nDAS <- nrow(das.df)

  ndx.R <- which(das.df$Event == "R")
  ndx.E <- which(das.df$Event == "E")

  if (length(ndx.E) != length(ndx.R)) {
    stop("Error: There are not an equal number of 'R' and 'E' events",
         "in the provided DAS file")
  }
  if (!all(ndx.E - ndx.R > 0) | !all(head(ndx.E, -1) < ndx.R[-1])) {
    stop("Error: Not all 'R' events are follow by 'E' events")
  }

  OnEffort.idx <- unlist(mapply(function(i, j) {
    head(i:j, -1)
  }, ndx.R, ndx.E, SIMPLIFY = FALSE))
  OnEffort <- seq_len(nDAS) %in% OnEffort.idx
  rm(OnEffort.idx)


  #----------------------------------------------------------------------------
  # Determine 'reset' rows, i.e. rows that mark a new cruise in concatenated
  #   DAS file or a new day for conditions
  dt.na <- is.na(das.df$DateTime)

  ### Determine indices where time-date change is by more than 'day.gap' days
  ###   Used to recognize data from new cruise in concatenated DAS files
  time_diff <- rep(NA, nDAS)
  time_diff[!dt.na] <- c(NA, abs(diff(das.df$DateTime[!dt.na]))) / (60*60*24)
  # d <- c(NA, abs(diff(das.df$DateTime))) / (60*60*24)
  # all.equal(d[!is.na(d)], time_diff[!is.na(d)])

  idx.new.cruise <- c(1, which(time_diff > days.gap))

  ### Determine row numbers of new days in das.df;
  ###   these will include idxs of new cruises. Used when reset.day is TRUE
  idx.nona.new <- which(diff(das.df$Da[!dt.na]) != 0) + 1
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
  # Loop through data for 'carry-over info' that applies to subsequent events
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
  # Post-processing

  # ### Set all -9999 values as NA
  # if (reset.event) {
  #   Bft[Bft == -9999] <- NA             #Beaufort
  #   Course[Course == -9999] <- NA       #Course
  #   Cruise[Cruise == -9999] <- NA       #Cruise number
  #   Mode[Mode == -9999] <- NA           #Mode
  #   EffType[EffType == -9999] <- NA     #Effort type
  #   HorizSun[HorizSun == -9999] <- NA   #Horizontal sun
  #   VertSun[VertSun == -9999] <- NA     #Vertical sun
  #   RainFog[RainFog == -9999] <- NA     #RainFog
  #   SwellHght[SwellHght == -9999] <- NA #Swell height
  #   Vis[Vis == -9999] <- NA             #Visibility
  # }
  #
  # ### Post-for loop variable processing
  # Glare <- ifelse(
  #   is.na(HorizSun) | is.na(VertSun), FALSE, #FALSE rather than NA per JVR notes
  #   (HorizSun %in% c(11, 12, 1) & VertSun %in% c(2, 3)) | (HorizSun %in% 12 & VertSun %in% 1)
  # )
  # EffType <- as.character(EffType)
  # Mode <- as.character(toupper(Mode))
  # RainFog <- as.logical(ifelse(is.na(RainFog), NA, RainFog %in% c(2:4)))
  #
  # ### Check that cruise num from data matches supplied cruise num (if appl)
  # if (!is.na(cruise.num)) {
  #   c.nona <- sort(unique(Cruise[!is.na(Cruise)]))
  #   if (!all(c.nona %in% cruise.num)) {
  #     warning("Warning: Cruise number(s) extracted from DAS data ",
  #             "(printed below) do not match cruise number provided ",
  #             "via the cruise.num argument.\n",
  #             "Extracted cruise number(s): ", paste(c.nona, collapse = ", "))
  #   }
  #   Cruise <- cruise.num
  # }
  #
  # data.frame(
  #   das.df, CruiseName = as.character(cruise.name), as.numeric(Cruise),
  #   Shipname = as.character(ship.name), OnEffort_RE,
  #   Mode, EffType, Bft, SwellHght,
  #   RainFog, HorizSun, VertSun, Glare, Vis, Course,
  #   stringsAsFactors = FALSE
  # )

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

  ### Post-for loop variable processing
  tmp$Glare <- ifelse(
    is.na(tmp$HorizSun) | is.na(tmp$VertSun), FALSE, #FALSE rather than NA per JVR notes
    (tmp$HorizSun %in% c(11, 12, 1) & tmp$VertSun %in% c(2, 3)) | (tmp$HorizSun %in% 12 & tmp$VertSun %in% 1)
  )
  tmp$EffType <- as.character(tmp$EffType)
  tmp$Mode <- as.character(toupper(tmp$Mode))
  tmp$RainFog <- as.logical(ifelse(is.na(tmp$RainFog), NA, tmp$RainFog %in% c(2:4)))

  ### Check that cruise num from data matches supplied cruise num (if appl)
  if (!is.na(cruise.num)) {
    c.nona <- sort(unique(tmp$Cruise[!is.na(Cruise)]))
    if (!all(c.nona %in% cruise.num)) {
      warning("Warning: Cruise number(s) extracted from DAS data ",
              "(printed below) do not match cruise number provided ",
              "via the cruise.num argument.\n",
              "Extracted cruise number(s): ", paste(c.nona, collapse = ", "))
    }
    tmp$Cruise <- cruise.num
  }
  tmp$Cruise <- as.numeric(tmp$Cruise)

  browser()


  #----------------------------------------------------------------------------
  # Create and order data frame to return
  cols.tokeep <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "EffType", "Course", "Bft", "SwellHght",
    "RainFog", "HorizSun", "VertSun", "Glare", "Vis",
    "CruiseName", "Shipname",
    paste0("Data", 1:8), "file_das", "event_num", "line_num"
  )

  data.frame(
    das.df, tmp, CruiseName = as.character(cruise.name),
    Shipname = as.character(ship.name), OnEffort,
    stringsAsFactors = FALSE
  ) %>%
    select(!!cols.tokeep)
}
