#' DAS sightings
#'
#' Extract sightings and associated information from processed DAS data
#'
#' @param x an object of class \code{das_df},
#'   or a data frame that can be coerced to class \code{das_df}
#' @param ... ignored
#' @param return.format character; can be one of "default", "wide", "complete",
#'   or any partial match thereof (case sensitive). Formats described below
#' @param return.events character; event codes included in the output.
#'   Must be one or more of: "S", "K", "M", "G", "s", "k", "m", "g", "t", "p", "F"
#'   (case-sensitive). The default is all of these event codes
#'
#' @details DAS events contain specific information in the 'Data#' columns,
#'   with the information depending on the event code for that row.
#'   The output data frame contains columns with this specific information
#'   extracted to dedicated columns as described below.
#'   This function recognizes the following types of sightings:
#'   marine mammal sightings (event codes "S", "K", or "M"),
#'   marine mammal resights (codes "s", "k", "m"),
#'   marine mammal subgroup sightings (code "G"),
#'   marine mammal subgroup resights (code "g"),
#'   turtle sightings (code "t"),
#'   pinniped sightings (code "p")
#'   and fishing vessel sightings (code "F").
#'   Warnings are printed if all S, K, M, and G events (and only these events) are not
#'   followed by an A event and at least one numeric event.
#'   See \code{\link{das_format_pdf}} for more information about events and event formats,
#'
#'   The \code{return.events} argument simply provides a shortcut for
#'   filtering the output of \code{das_sight} by event codes
#'
#'   Abbreviations used in output column names: Gs = group size, Sp = species,
#'   Nm = nautical mile, Perc = percentage, Prob = probable,
#'   GsSchool = school-level group size info
#'
#'   This function makes the following assumptions, and alterations to the raw DAS data:
#'   \itemize{
#'     \item "A" events immediately following an S/K/M/G event have
#'     the same sighting number (Data1 value) as the S/K/M/G event
#'     \item The 'nSp' column is equivalent to the number of non-\code{NA} values across the
#'       'Data5', 'Data6', 'Data7', and 'Data8' columns
#'       for the pertinent "A" event
#'     \item The following data are coerced to a numeric using
#'       \code{\link[base:numeric]{as.numeric}}:
#'       Bearing, Reticle, DistNm, Cue, Method,
#'       species percentages, and group sizes (including for t, p, and F events).
#'       Note that if there are any formatting errors and these data are not numeric,
#'       the function will likely print a warning message
#'     \item The values for the following columns are capitalized using
#'       \code{\link[base:chartr]{toupper}}:
#'       'Birds', 'Photos', 'CalibSchool', 'PhotosAerial', 'Biopsy',
#'       'TurtleAge', and 'TurtleCapt'
#'   }
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information extracted from 'Data#' columns.
#'   See \code{\link{das_format_pdf}} for more information the sighting information.
#'   If \code{return.format} is "default", then there is one row for each species of each sighting;
#'   if \code{return.format} is "wide", then there is one row for each sighting event;
#'   if \code{return.format} is "complete", then there is one row for every
#'   group size estimate for each sighting.
#'
#'   The format-specific columns are described in their respective sections,
#'   while the sighting information columns included for all return formats are described here:
#'
#'   \tabular{lll}{
#'     \emph{Sighting information} \tab \emph{Column name} \tab \emph{Notes}\cr
#'     Sighting number                 \tab SightNo \tab Character\cr
#'     Subgroup code                   \tab Subgroup \tab Character\cr
#'     Daily sighting number           \tab SightNoDaily \tab See below\cr
#'     Observer that made the sighting \tab Obs\cr
#'     Standard observer               \tab ObsStd \tab Logical; \code{TRUE} if Obs is one of
#'       ObsL, Rec or ObsR, and \code{FALSE} otherwise (including if Obs is \code{NA})\cr
#'     Bearing to the sighting         \tab Bearing \tab Numeric; degrees, range 0 to 360\cr
#'     Number of reticle marks         \tab Reticle \tab Numeric\cr
#'     Distance (nautical miles)       \tab DistNm  \tab Numeric\cr
#'     Sighting cue                    \tab Cue\cr
#'     Sighting method                 \tab Method\cr
#'     Photos of school?               \tab Photos\cr
#'     Birds present with school?      \tab Birds\cr
#'     Calibration school?             \tab CalibSchool\cr
#'     Aerial photos taken?            \tab PhotosAerial\cr
#'     Biopsy taken?                   \tab Biopsy\cr
#'     Probable sighting               \tab Prob  \tab
#'       Logical indicating if sighting has associated ? event; \code{NA} for non-S/K/M/G events\cr
#'     Number of species in sighting   \tab nSp   \tab \code{NA} for non-S/K/M/G events\cr
#'     Mixed species sighting          \tab Mixed \tab Logical; \code{TRUE} if nSp > 1\cr
#'     Group size of school - best estimate \tab GsSchoolBest \tab See below\cr
#'     Group size of school - high estimate \tab GsSchoolHigh \tab See below\cr
#'     Group size of school - low estimate  \tab GsSchoolLow  \tab See below\cr
#'     Course (true heading) of school at resight \tab CourseSchool \tab \code{NA} for non-s/k/m events\cr
#'     Presence of associated JFR  \tab TurtleJFR  \tab
#'       \code{NA} for non-"t" events; JFR = jellyfish, floating debris, or red tide \cr
#'     Estimated turtle maturity   \tab TurtleAge  \tab \code{NA} for non-"t" events\cr
#'     Perpendicular distance (km) to sighting \tab PerpDistKm \tab
#'       Calculated via \code{(abs(sin(Bearing*pi/180) * DistNm) * 1.852)}\cr
#'   }
#'
#'   SightNoDaily is a running count of the number of S/K/M/G sightings that occurred on each day.
#'   It is formatted as 'YYYYMMDD'_'running count', e.g. "20050101_1".
#'   The GsSchoolBest, GsSchoolHigh, and GsSchoolLow columns are either:
#'   1) the arithmetic mean across observer estimates for the "default" and "wide" formats, or
#'   2) the individual observer estimates for the "complete" format.
#'   To convert the perpendicular distance back to nautical miles,
#'   one would divide PerpDistKm by 1.852
#'
#' @section The "default" format output:
#'   One row for each species of each sighting, with group size estimates
#'   calculated via an arithmetic average across observer estimates
#'
#'   Sighting information columns/formats present specifically in the "default" format output:
#'   \tabular{lll}{
#'     \emph{Sighting information} \tab \emph{Column name} \tab \emph{Notes}\cr
#'     Species code          \tab SpCode \tab Boat type or mammal, turtle, or pinniped species codes\cr
#'     Probable species code \tab SpProb \tab Probable mammal species codes, if applicable; otherwise \code{NA}\cr
#'     Group size of species - best estimate \tab GsSpBest \tab
#'       Product of GsSchoolBest and the (average) corresponding species percentage \cr
#'     Group size of species - high estimate \tab GsSpHigh \tab
#'       Product of GsSchoolHigh and the (average) corresponding species percentage \cr
#'     Group size of species - low estimate \tab GsSpLow \tab
#'       Product of GsSchoolLow and the (average) corresponding species percentage \cr
#'   }
#'
#' @section The "wide" and "complete" format outputs:
#'   The "wide" and "complete" options have very similar columns in their output date frames.
#'   There are two main differences: 1) the "wide" format has one row for each sighting event,
#'   while the complete format has a row for every observer estimate for each sightings, and thus
#'   2) in the "wide" format, all numeric information for which there are multiple observer estimates
#'   (school group size, species percentage, etc.) are averaged across estimated via
#'   an arithmetic mean (using \code{\link[base]{mean}} with \code{na.rm = TRUE})
#'
#'   With these formats, note that the species/type code and group size for
#'   turtle, pinniped, and boat sightings are in their own column
#'
#'   Sighting information columns present in the "wide" and "complete" format outputs:
#'   \tabular{lll}{
#'     \emph{Sighting information} \tab \emph{Column name} \tab \emph{Notes}\cr
#'     Observer code - estimate    \tab ObsEstimate \tab See below\cr
#'     Species 1 code              \tab SpCode1 \tab The species code for turtle and pinniped sightings\cr
#'     Species 2 code              \tab SpCode2\cr
#'     Species 3 code              \tab SpCode3\cr
#'     Species 4 code              \tab SpCode4\cr
#'     Percentage of Sp1 in school \tab SpPerc1\cr
#'     Percentage of Sp2 in school \tab SpPerc2\cr
#'     Percentage of Sp3 in school \tab SpPerc3\cr
#'     Percentage of Sp4 in school \tab SpPerc4\cr
#'     Species 1 probable code     \tab SpProb1 \tab Extracted from '?' event\cr
#'     Species 2 probable code     \tab SpProb2 \tab Extracted from '?' event\cr
#'     Species 3 probable code     \tab SpProb3 \tab Extracted from '?' event\cr
#'     Species 4 probable code     \tab SpProb4 \tab Extracted from '?' event\cr
#'     Group size of species 1     \tab GsSpBest1 \tab Present in "wide" output only; see below\cr
#'     Group size of species 2     \tab GsSpBest2 \tab Present in "wide" output only; see below\cr
#'     Group size of species 3     \tab GsSpBest3 \tab Present in "wide" output only; see below\cr
#'     Group size of species 4     \tab GsSpBest4 \tab Present in "wide" output only; see below\cr
#'     Turtle species        \tab TurtleSp    \tab \code{NA} for non-"t" events\cr
#'     Sighting group size   \tab TurtleGs    \tab \code{NA} for non-"t" events; i.e. number of turtles\cr
#'     Was turtle captured?  \tab TurtleCapt  \tab \code{NA} for non-"t" events\cr
#'     Pinniped species      \tab PinnipedSp  \tab \code{NA} for non-"p" events\cr
#'     Sighting group size   \tab PinnipedGs  \tab \code{NA} for non-"p" events; i.e. number of pinnipeds\cr
#'     Boat or gear type     \tab BoatType    \tab \code{NA} for non-"F" events\cr
#'     Sighting group size   \tab BoatGs      \tab \code{NA} for non-"F" events; i.e. number of boats\cr
#'   }
#'
#'   ObsEstimate refers to the code of the observer that made the corresponding estimate.
#'   For the "wide" format, ObsEstimate is a list-column of all of the observer codes
#'   that provided an estimate. The GsSpBest# columns are the product of
#'   GsSchoolBest and the corresponding species percentage
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' das_sight(y.proc)
#' das_sight(y.proc, return.format = "wide")
#' das_sight(y.proc, return.format = "complete")
#'
#' @export
das_sight <- function(x, ...) UseMethod("das_sight")


#' @name das_sight
#' @export
das_sight.data.frame <- function(x, ...) {
  das_sight(as_das_df(x), ...)
}


#' @name das_sight
#' @export
das_sight.das_df <- function(x, return.format = c("default", "wide", "complete"),
                             return.events = c("S", "K", "M", "G", "s", "k", "m", "g", "t", "p", "F"),
                             ...) {
  #----------------------------------------------------------------------------
  return.format <- match.arg(return.format)
  return.events <- match.arg(return.events, several.ok = TRUE)


  #----------------------------------------------------------------------------
  # Filter for sighting-related events
  event.sight <- c("S", "K", "M", "G", "s", "k", "m", "g", "t", "p", "F")
  event.sight.info <- c("A", "?", 1:8)

  sight.df <- x %>%
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>%
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))

  # Check that all GSKM events are followed by an A event
  idx.skmg <- which(x$Event %in% c("S", "K", "M", "G"))
  skmga.check1a <- which(x$Event[idx.skmg + 1] != "A")
  skmga.check1b <- isTRUE(all.equal(idx.skmg + 1, which(x$Event == "A")))
  skmga.check2 <- identical(x$Data1[idx.skmg], x$Data1[idx.skmg + 1])

  if (!(length(skmga.check1a) == 0 & skmga.check1b)) {
    warning("All 'G', 'S', 'K', and 'M' events (and only these events) ",
            "should be immediately followed by an 'A' event",
            immediate. = TRUE)
  } else if (!skmga.check2) {
    warning("The sighting number in some 'S', 'K', and 'M' events do not match ",
            "the sighting numbers of their corresponding 'A' events",
            immediate. = TRUE)
  }
  rm(skmga.check1a, skmga.check1b, skmga.check2)


  #----------------------------------------------------------------------------
  # Get applicable data for each type of sighting event

  #--------------------------------------------------------
  ### Data that is in all sighting events
  # SightNo is left as character because of entries such as "408A"
  sight.info.all <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    mutate(SightNo = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data1,
                               .data$Event %in% c("G", "g") ~ .data$Data1,
                               .data$Event %in% c("s", "k", "m") ~ .data$Data1),
           Subgroup = case_when(.data$Event %in% c("G", "g") ~ .data$Data2),
           Obs = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data2,
                           .data$Event == "G" ~ .data$Data3,
                           .data$Event == "t" ~ .data$Data1,
                           .data$Event == "p" ~ .data$Data1,
                           .data$Event == "F" ~ .data$Data1),
           ObsStd = pmap_lgl(list(.data$Obs, .data$ObsL, .data$Rec, .data$ObsR),
                             function(obs, o1, o2, o3) {
                               obs %in% c(o1, o2, o3)
                             }),
           Bearing = as.numeric(
             case_when(
               .data$Event %in% c("S", "K", "M", "G") ~ .data$Data5,
               .data$Event %in% c("s", "k", "m") ~ .data$Data2,
               .data$Event == "g" ~ .data$Data3,
               .data$Event == "t" ~ .data$Data3,
               .data$Event == "p" ~ .data$Data3,
               .data$Event == "F" ~ .data$Data2)),
           Reticle = as.numeric(
             case_when(
               .data$Event %in% c("S", "K", "M", "G") ~ .data$Data6,
               .data$Event %in% c("s", "k", "m") ~ .data$Data3,
               .data$Event == "g" ~ .data$Data4,
               .data$Event == "t" ~ .data$Data7,
               .data$Event == "p" ~ .data$Data6,
               .data$Event == "F" ~ .data$Data4)),
           DistNm = as.numeric(
             case_when(
               .data$Event %in% c("S", "K", "M", "G") ~ .data$Data7,
               .data$Event %in% c("s", "k", "m") ~ .data$Data4,
               .data$Event == "g" ~ .data$Data4,
               .data$Event == "t" ~ .data$Data4,
               .data$Event == "p" ~ .data$Data4,
               .data$Event == "F" ~ .data$Data3))) %>%
    group_by(day(.data$DateTime)) %>%
    mutate(SightNoDaily = paste(base::format(.data$DateTime, "%Y%m%d"),
                                cumsum(.data$Event %in% c("S", "K", "M", "G")),
                                sep = "_"),
           SightNoDaily = ifelse(.data$Event %in% c("S", "K", "M", "G"),
                                 .data$SightNoDaily, NA)) %>%
    ungroup() %>%
    select(.data$sight_cumsum, .data$SightNo, .data$Subgroup, .data$SightNoDaily,
           .data$Obs, .data$ObsStd, .data$Bearing, .data$Reticle, .data$DistNm)


  #--------------------------------------------------------
  ### Marine mammal (+subgroup) sightings; Events S, K, M, G
  sight.info.skmg1 <- sight.df %>%
    filter(.data$Event %in% c("S", "K", "M", "G")) %>%
    mutate(Cue = ifelse(.data$Event == "G", NA, as.numeric(.data$Data3)),
           Method = as.numeric(.data$Data4),
           CalibSchool = toupper(.data$Data10),
           PhotosAerial = toupper(.data$Data11),
           Biopsy = toupper(.data$Data12)) %>%
    select(.data$sight_cumsum, .data$Cue, .data$Method,
           .data$CalibSchool, .data$PhotosAerial, .data$Biopsy)

  # Data from A row
  sight.info.skmg2 <- sight.df %>%
    filter(.data$Event =="A") %>%
    mutate(Photos = toupper(.data$Data3),
           Birds = toupper(.data$Data4),
           nSp = unlist(
             pmap(list(.data$Data5, .data$Data6, .data$Data7, .data$Data8),
                  function(d5, d6, d7, d8) {
                    sum(!is.na(c(d5, d6, d7, d8)))
                  })),
           Mixed = .data$nSp > 1) %>%
    select(.data$sight_cumsum, .data$Photos, .data$Birds, .data$nSp, .data$Mixed,
           SpCode1 = .data$Data5, SpCode2 = .data$Data6,
           SpCode3 = .data$Data7, SpCode4 = .data$Data8)

  # Data from ? row, if any
  sight.info.skmg3 <- sight.df %>%
    filter(.data$Event %in% c("?")) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(Prob = TRUE,
              SpProb1 = .data$Data5, SpProb2 = .data$Data6,
              SpProb3 = .data$Data7, SpProb4 = .data$Data8)


  # Data from numeric events (groupsize and composition estimates)
  if (return.format == "complete") {
    sight.info.skmg4 <- sight.df %>%
      filter(.data$Event %in% as.character(1:8)) %>%
      mutate(GsSchoolBest = as.numeric(.data$Data2),
             GsSchoolHigh = as.numeric(.data$Data3),
             GsSchoolLow = as.numeric(.data$Data4),
             SpPerc1 = as.numeric(.data$Data5),
             SpPerc2 = as.numeric(.data$Data6),
             SpPerc3 = as.numeric(.data$Data7),
             SpPerc4 = as.numeric(.data$Data8)) %>%
      select(.data$sight_cumsum, ObsEstimate = .data$Data1,
             .data$GsSchoolBest, .data$GsSchoolHigh, .data$GsSchoolLow,
             .data$SpPerc1, .data$SpPerc2, .data$SpPerc3, .data$SpPerc4)

  } else { #if (return.format == "wide") {
    sight.info.skmg4 <- sight.df %>%
      filter(.data$Event %in% as.character(1:8)) %>%
      group_by(.data$sight_cumsum) %>%
      summarise(ObsEstimate = list(.data$Data1),
                GsSchoolBest = mean(as.numeric(.data$Data2), na.rm = TRUE),
                GsSchoolHigh = mean(as.numeric(.data$Data3), na.rm = TRUE),
                GsSchoolLow = mean(as.numeric(.data$Data4), na.rm = TRUE),
                SpPerc1 = mean(as.numeric(.data$Data5), na.rm = TRUE),
                SpPerc2 = mean(as.numeric(.data$Data6), na.rm = TRUE),
                SpPerc3 = mean(as.numeric(.data$Data7), na.rm = TRUE),
                SpPerc4 = mean(as.numeric(.data$Data8), na.rm = TRUE),
                GsSpBest1 = .data$GsSchoolBest * .data$SpPerc1 / 100,
                GsSpBest2 = .data$GsSchoolBest * .data$SpPerc2 / 100,
                GsSpBest3 = .data$GsSchoolBest * .data$SpPerc3 / 100,
                GsSpBest4 = .data$GsSchoolBest * .data$SpPerc4 / 100)
  }

  if (!all(sight.info.skmg1$sight_cumsum %in% sight.info.skmg4$sight_cumsum))
    warning("Not all S/K/M/G events have corresponding numeric (1:8) events")


  sight.info.skmg <- sight.info.skmg1 %>%
    left_join(sight.info.skmg2, by = "sight_cumsum") %>%
    left_join(sight.info.skmg3, by = "sight_cumsum") %>%
    left_join(sight.info.skmg4, by = "sight_cumsum") %>%
    mutate(Prob = ifelse(is.na(.data$Prob), FALSE, .data$Prob)) %>%
    select(.data$sight_cumsum, .data$Cue, .data$Method,
           .data$Photos, .data$Birds,
           .data$CalibSchool, .data$PhotosAerial, .data$Biopsy,
           .data$Prob, .data$nSp, .data$Mixed,
           .data$ObsEstimate,
           starts_with("SpCode"), starts_with("SpProb"), starts_with("SpPerc"),
           starts_with("GsSpBest"), starts_with("GsSchool"),
           everything())
  rm(sight.info.skmg1, sight.info.skmg2, sight.info.skmg3, sight.info.skmg4)

  sight.info.skmg[is.na(sight.info.skmg)] <- NA

  #--------------------------------------------------------
  ### Marine mammal (+subgroup) resights; Events s, k, m
  sight.info.resight <- sight.df %>%
    filter(.data$Event %in% c("s", "k", "m")) %>%
    mutate(CourseSchool = as.numeric(.data$Data5)) %>%
    select(.data$sight_cumsum, .data$CourseSchool)


  #--------------------------------------------------------
  ### Turtle sightings; Events t
  sight.info.t <- sight.df %>%
    filter(.data$Event == "t") %>%
    mutate(TurtleSp = .data$Data2,
           TurtleGs = as.numeric(.data$Data5),
           TurtleJFR = .data$Data6,
           TurtleAge = toupper(.data$Data8),
           TurtleCapt = toupper(.data$Data9)) %>%
    select(.data$sight_cumsum, .data$TurtleSp, .data$TurtleGs,
           .data$TurtleJFR, .data$TurtleAge, .data$TurtleCapt)


  #--------------------------------------------------------
  ### Pinnipeds; event p
  sight.info.p <- sight.df %>%
    filter(.data$Event == "p") %>%
    mutate(PinnipedSp = .data$Data2,
           PinnipedGs = as.numeric(.data$Data5)) %>%
    select(.data$sight_cumsum, .data$PinnipedSp, .data$PinnipedGs)


  #--------------------------------------------------------
  ### Fishing boats; Events F
  sight.info.f <- sight.df %>%
    filter(.data$Event == "F") %>%
    mutate(BoatType = .data$Data5,
           BoatGs = as.numeric(.data$Data6)) %>%
    select(.data$sight_cumsum, .data$BoatType, .data$BoatGs)


  #----------------------------------------------------------------------------
  # Format and return

  #--------------------------------------------------------
  ### Formatting for all options - now done for wide and complete
  to.return <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    select(-.data$Data1, -.data$Data2, -.data$Data3,
           -.data$Data4, -.data$Data5, -.data$Data6,
           -.data$Data7, -.data$Data8, -.data$Data9,
           -.data$Data10, -.data$Data11, -.data$Data12) %>%
    left_join(sight.info.all, by = "sight_cumsum") %>%
    left_join(sight.info.skmg, by = "sight_cumsum") %>%
    left_join(sight.info.resight, by = "sight_cumsum") %>%
    left_join(sight.info.t, by = "sight_cumsum") %>%
    left_join(sight.info.p, by = "sight_cumsum") %>%
    left_join(sight.info.f, by = "sight_cumsum") %>%
    select(-.data$sight_cumsum)


  #--------------------------------------------------------
  ### Formatting for default method
  if (return.format == "default") {
    # Split multi-species sightings into multiple rows as necessary
    to.return$idx <- seq_len(nrow(to.return))

    to.return.multi <- to.return %>%
      filter(.data$Event %in% c("S", "K", "M", "G")) %>%
      mutate(GsSpHigh1 = .data$SpPerc1 * .data$GsSchoolHigh / 100,
             GsSpHigh2 = .data$SpPerc2 * .data$GsSchoolHigh / 100,
             GsSpHigh3 = .data$SpPerc3 * .data$GsSchoolHigh / 100,
             GsSpHigh4 = .data$SpPerc4 * .data$GsSchoolHigh / 100,
             GsSpLow1 = .data$SpPerc1 * .data$GsSchoolLow / 100,
             GsSpLow2 = .data$SpPerc2 * .data$GsSchoolLow / 100,
             GsSpLow3 = .data$SpPerc3 * .data$GsSchoolLow / 100,
             GsSpLow4 = .data$SpPerc4 * .data$GsSchoolLow / 100) %>%
      group_by(.data$idx) %>%
      summarise(Sp1_list = list(c(.data$SpCode1, .data$SpProb1, .data$GsSpBest1,
                                  .data$GsSpHigh1, .data$GsSpLow1)),
                Sp2_list = list(c(.data$SpCode2, .data$SpProb2, .data$GsSpBest2,
                                  .data$GsSpHigh2, .data$GsSpLow2)),
                Sp3_list = list(c(.data$SpCode3, .data$SpProb3, .data$GsSpBest3,
                                  .data$GsSpHigh3, .data$GsSpLow3)),
                Sp4_list = list(c(.data$SpCode4, .data$SpProb4, .data$GsSpBest4,
                                  .data$GsSpHigh4, .data$GsSpLow4))) %>%
      gather(.data$Sp1_list, .data$Sp2_list, .data$Sp3_list, .data$Sp4_list,
             key = "sp_list_name", value = "sp_list", na.rm = TRUE) %>%
      mutate(SpCode = map_chr(.data$sp_list, function(i) i[1]),
             SpProb = map_chr(.data$sp_list, function(i) i[2]),
             GsSpBest = as.numeric(map_chr(.data$sp_list, function(i) i[3])),
             GsSpHigh = as.numeric(map_chr(.data$sp_list, function(i) i[4])),
             GsSpLow = as.numeric(map_chr(.data$sp_list, function(i) i[5]))) %>%
      filter(!is.na(.data$SpCode)) %>%
      select(.data$idx, .data$SpCode, .data$SpProb, .data$GsSpBest, .data$GsSpHigh, .data$GsSpLow) %>%
      arrange(.data$idx)

    # Names and order of columns to return
    sight.names <- setdiff(
      c(names(sight.df), names(sight.info.all), #names(sight.info.skmg)[1:9],
        "Cue", "Method", "Photos", "Birds", "CalibSchool", "PhotosAerial", "Biopsy",
        "Prob", "nSp", "Mixed", "SpCode", "SpProb",
        "GsSpBest", "GsSpHigh", "GsSpLow",
        "GsSchoolBest", "GsSchoolHigh", "GsSchoolLow",
        names(sight.info.resight), names(sight.info.t),
        names(sight.info.p), names(sight.info.f)),
      c("sight_cumsum", paste0("Data", 1:12))
    )

    # Finalize return data frame, consolidating columns as possible
    to.return <- to.return %>%
      select(-.data$SpCode1, -.data$SpCode2, -.data$SpCode3, -.data$SpCode4,
             -.data$SpProb1, -.data$SpProb2, -.data$SpProb3, -.data$SpProb4,
             -.data$SpPerc1, -.data$SpPerc2, -.data$SpPerc3, -.data$SpPerc4,
             -.data$GsSpBest1, -.data$GsSpBest2, -.data$GsSpBest3, -.data$GsSpBest4) %>%
      full_join(to.return.multi, by = "idx") %>%
      arrange(.data$idx) %>%
      select(!!sight.names) %>%
      mutate(SpCode = case_when(.data$Event %in% c("S", "K", "M", "G") ~ .data$SpCode,
                                .data$Event == "t" ~ .data$TurtleSp,
                                .data$Event == "p" ~ .data$PinnipedSp,
                                .data$Event == "F" ~ .data$BoatType),
             GsSchoolBest = case_when(.data$Event %in% c("S", "K", "M", "G") ~ .data$GsSchoolBest,
                                      .data$Event == "t" ~ .data$TurtleGs,
                                      .data$Event == "p" ~ .data$PinnipedGs,
                                      .data$Event == "F" ~ .data$BoatGs),
             GsSpBest = ifelse(.data$Event %in% c("t", "p", "F"), .data$GsSchoolBest, .data$GsSpBest)) %>%
      select(-.data$TurtleSp, -.data$TurtleGs, -.data$PinnipedSp,
             -.data$PinnipedGs, -.data$BoatType, -.data$BoatGs)
  }


  #--------------------------------------------------------
  ### Calculate perp dist and return
  to.return %>%
    mutate(PerpDistKm = abs(sin(.data$Bearing*pi/180) * .data$DistNm) * 1.852) %>%
    filter(.data$Event %in% return.events)
}
