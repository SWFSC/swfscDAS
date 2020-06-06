#' DAS sightings
#'
#' Extract sightings and associated information from processed DAS data
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}},
#'   or a data frame that can be coerced to a \code{das_df} object
#' @param ... ignored
#' @param returnformat character; one of "default", "long", or "comprehensive"
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
#'   Warnings are printed if all S, K, M, and G events (and only these events) are followed by
#'   an A event and at least one numeric event.
#'   See \code{\link{das_format_pdf}} for more information about events and event formats.
#'
#'   \code{returnformat} "default": One row per species; multi-species sighitngs are split into multiple lines.
#'     Turtle num/sp and Boat num combined into Sp and Gs columns
#'   \code{returnformat} "wide": One column for everything, one row for each sighting event. Values averaged for different observers
#'   \code{returnformat} "comprehensive": TODO
#'
#'   Abbreviations used in column names: Gs = group size, Sp = species,
#'   Nm = nautical mile, Perc = percentage, Prob = probable
#'
#'   This function makes the following assumptions, and alterations to the raw data:
#'   \itemize{
#'     \item "G", "S", "K", and "M" events, and only these events,
#'       are immediately followed by an "A" event.
#'     \item "A" events following an S/K/M/G event have the same Data1 (sighting number)
#'       as the S/K/M/G event.
#'     \item The 'nSp' column is equivalent to the number of non-\code{NA}
#'       'Data5', 'Data6', 'Data7', and 'Data8' values
#'       for the corresponding "A" event are not \code{NA}, and \code{FALSE} otherwise.
#'       This column has a non-\code{NA} value for only "S", "K", "M", and "G" events
#'     \item The 'Prob' column is \code{TRUE} is the sighting has an associated "?" event,
#'       and \code{FALSE} otherwise.
#'       This column has a non-\code{NA} value for only "S", "K", and "M" events
#'     \item The 'GsSchoolBest', 'GsSchoolHigh', and 'GsSchoolLow' columns are the arithmetic mean of
#'       the 'Data2', 'Data3', and 'Data4' columns, respectively, with \code{na.rm = TRUE}
#'       for the associated "1"-"8" events
#'     \item The 'SpPerc1', 'SpPerc2', 'SpPerc3', and 'SpPerc4' values are the
#'       arithemtic means of the 'Data5', 'Data6', 'Data7', and 'Data8' columns (with \code{na.rm = TRUE}),
#'       respectively, for the associated "1"-"8" events
#'     \item The 'GsSpBest#' values are the product of the 'GsSchoolBest' and the respective 'SpPerc#' columns
#'     \item The values for the following columns are capitalized using
#'       \code{\link[base:chartr]{toupper}}:
#'       'Birds', 'Photos', 'CalibSchool', 'PhotosAerial', 'Biopsy',
#'       'TurtleAge', and 'TurtleCapt'
#'   }
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information extracted from 'Data#' columns (described below).
#'   See \code{\link{das_format_pdf}} for more information the sighting information.
#'   If \code{returnformat} is "default", then there is one row for each species of each sighting;
#'   if \code{returnformat} is "wide", then there is one row for each sighting event;
#'   if \code{returnformat} is "complete", then there is one row for every
#'   group size estimate for each sighting.
#'   The specific columns in each output are described below.
#'
#'   Sighting information columns in every format:
#'   \tabular{lll}{
#'     \emph{Sighting information}     \tab \emph{Column name} \tab \emph{Notes}\cr
#'     Sighting number                 \tab SightNo\cr
#'     Subgroup code                   \tab Subgroup\cr
#'     Observer that made the sighting \tab Obs\cr
#'     Standard observer               \tab Obs_std \tab Logical; \code{TRUE} if Obs is one of ObsL, Rec or ObsR,
#'       and \code{FALSE} if Obs is \code{NA}\cr
#'     Bearing to the sighting         \tab Bearing \tab Degrees, range 0 to 360\cr
#'     Number of reticle marks         \tab Reticle\cr
#'     Distance (nautical miles)       \tab DistNm\cr
#'     Sighting cue                    \tab Cue\cr
#'     Sighting method                 \tab Method\cr
#'     Photos of school?               \tab Photos\cr
#'     Birds present with school?      \tab Birds\cr
#'     Calibration school?             \tab CalibSchool\cr
#'     Aerial photos taken?            \tab PhotosAerial\cr
#'     Biopsy taken?                   \tab Biopsy\cr
#'     Probable sighting               \tab Prob  \tab Logical; \code{NA} for non-S/K/M/G events\cr
#'     Number of species in sighting   \tab nSp   \tab \code{NA} for non-S/K/M/G events\cr
#'     Mixed species sighting          \tab Mixed \tab Logical; \code{TRUE} if nSp > 1\cr
#'     Course (true heading) of school at resight \tab CourseSchool \tab \code{NA} for non-s/k/m events\cr
#'     Presence of associated JFR  \tab TurtleJFR  \tab \code{NA} for non-"t" events; JFR = jellyfish, floating debris, or red tide \cr
#'     Estimated turtle maturity   \tab TurtleAge  \tab \code{NA} for non-"t" events\cr
#'     Perpendicular distance (km) to sighting \tab PerpDistKm \tab Calculated via \code{(abs(sin(Bearing*pi/180) * DistNm) * 1.852)}\cr
#'   }
#'   To convert the perpendicular distance back to nautical miles,
#'   one would divide PerpDistKm by 1.852.
#'
#'   Sighting information columns present specifically in the "default" format output:
#'   \tabular{lll}{
#'     School group size - best estimate \tab GsSchoolBest \tab Arithemtic mean of all best estimates\cr
#'     School group size - high estimate \tab GsSchoolHigh \tab Arithmetic mean of all high estimates\cr
#'     School group size - low estimate  \tab GsSchoolLow  \tab Arithmetic mean of all low estimates\cr
#'     Species code \tab SpCode  \tab For mammal, turtle, and pinniped species codes, as well as boat type\cr
#'     Species-specific group size - best estimate \tab GsSpBest \tab
#'       GsSchoolBest multiplied by the (average) corresponding species percentage \cr
#'   }
#'
#'   Sighting information columns present specifically in the "wide" format output:
#'   \tabular{lll}{
#'     Species 1 code                \tab SpCode1\cr
#'     Species 2 code                \tab SpCode2\cr
#'     Species 3 code                \tab SpCode3\cr
#'     Species 4 code                \tab SpCode4\cr
#'     Percentage of Sp1 in sighting \tab SpPerc1\cr
#'     Percentage of Sp2 in sighting \tab SpPerc2\cr
#'     Percentage of Sp3 in sighting \tab SpPerc3\cr
#'     Percentage of Sp4 in sighting \tab SpPerc4\cr
#'     Species 1 group size          \tab GsSpBest1\cr
#'     Species 2 group size          \tab GsSpBest2\cr
#'     Species 3 group size          \tab GsSpBest3\cr
#'     Species 4 group size          \tab GsSpBest4\cr
#'     Species 1 probable code       \tab SpProb1 \tab Extracted from '?' event\cr
#'     Species 2 probable code       \tab SpProb2 \tab Extracted from '?' event\cr
#'     Species 3 probable code       \tab SpProb3 \tab Extracted from '?' event\cr
#'     Species 4 probable code       \tab SpProb4 \tab Extracted from '?' event\cr
#'     Turtle species             \tab TurtleSp    \tab \code{NA} for non-"t" events\cr
#'     Number of turtles          \tab TurtleNum   \tab \code{NA} for non-"t" events\cr
#'     Was turtle captured?       \tab TurtleCapt  \tab \code{NA} for non-"t" events\cr
#'     Pinniped species           \tab PinnipedSp  \tab \code{NA} for non-"p" events\cr
#'     Number of pinnipeds        \tab PinnipedNum \tab \code{NA} for non-"p" events\cr
#'     Boat or gear type          \tab BoatType    \tab \code{NA} for non-"F" events\cr
#'     Number of boats            \tab BoatNum     \tab \code{NA} for non-"F" events\cr
#'   }
#'
#'   Sighting information columns present specifically in the "complete" format output:
#'   \tabular{lll}{
#'     TODO
#'   }
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' das_sight(y.proc)
#' das_sight(y.proc, returnformat = "wide")
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
das_sight.das_df <- function(x, returnformat = c("default", "wide", "comprehensive"),
                             ...) {
  #----------------------------------------------------------------------------
  returnformat <- match.arg(returnformat)


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
           Obs_std = pmap_lgl(list(.data$Obs, .data$ObsL, .data$Rec, .data$ObsR),
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
    select(.data$sight_cumsum, .data$SightNo, .data$Subgroup,
           .data$Obs, .data$Obs_std, .data$Bearing, .data$Reticle, .data$DistNm)


  #--------------------------------------------------------
  ### Marine mammal (+subgroup) sightings; Events S, K, M
  # Other data are extracted in sight.info.all
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

  # Data from grouped
  sight.info.skmg3 <- sight.df %>%
    filter(.data$Event %in% c("?")) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(Prob = TRUE,
              SpProb1 = .data$Data5, SpProb2 = .data$Data6,
              SpProb3 = .data$Data7, SpProb4 = .data$Data8)

  sight.info.skmg4 <- sight.df %>%
    filter(.data$Event %in% as.character(1:8)) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(GsSchoolBest = mean(as.numeric(.data$Data2), na.rm = TRUE),
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

  num.vec <- c(nrow(sight.info.skmg1), nrow(sight.info.skmg2),
               nrow(sight.info.skmg3), nrow(sight.info.skmg4))

  if (!isTRUE(all.equal(nrow(sight.info.skmg1), nrow(sight.info.skmg4))))
    warning("Not all S/K/M/G events have corresponding numeric (1:8) events; ",
            "please check the data using `das_check`")


  sight.info.skmg <- sight.info.skmg1 %>%
    left_join(sight.info.skmg2, by = "sight_cumsum") %>%
    left_join(sight.info.skmg3, by = "sight_cumsum") %>%
    left_join(sight.info.skmg4, by = "sight_cumsum") %>%
    mutate(Prob = ifelse(is.na(.data$Prob), FALSE, .data$Prob)) %>%
    select(.data$sight_cumsum, .data$Cue, .data$Method,
           .data$Photos, .data$Birds,
           .data$CalibSchool, .data$PhotosAerial, .data$Biopsy,
           .data$Prob, .data$nSp, .data$Mixed,
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
           TurtleNum = as.numeric(.data$Data5),
           TurtleJFR = .data$Data6,
           TurtleAge = toupper(.data$Data8),
           TurtleCapt = toupper(.data$Data9)) %>%
    select(.data$sight_cumsum, .data$TurtleSp, .data$TurtleNum,
           .data$TurtleJFR, .data$TurtleAge, .data$TurtleCapt)


  #--------------------------------------------------------
  ### Pinnipeds; event p
  sight.info.p <- sight.df %>%
    filter(.data$Event == "p") %>%
    mutate(PinnipedSp = .data$Data2,
           PinnipedNum = as.numeric(.data$Data5)) %>%
    select(.data$sight_cumsum, .data$PinnipedSp, .data$PinnipedNum)


  #--------------------------------------------------------
  ### Fishing boats; Events F
  sight.info.f <- sight.df %>%
    filter(.data$Event == "F") %>%
    mutate(BoatType = .data$Data5,
           BoatNum = as.numeric(.data$Data6)) %>%
    select(.data$sight_cumsum, .data$BoatType, .data$BoatNum)


  #----------------------------------------------------------------------------
  # Format and return
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


  if (returnformat == "default") {
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
                                 .data$Event == "t" ~ .data$TurtleNum,
                                 .data$Event == "p" ~ .data$PinnipedNum,
                                 .data$Event == "F" ~ .data$BoatNum),
             GsSpBest = ifelse(.data$Event %in% c("t", "p", "F"), .data$GsSchoolBest, .data$GsSpBest)) %>%
      select(-.data$TurtleSp, -.data$TurtleNum, -.data$PinnipedSp,
             -.data$PinnipedNum, -.data$BoatType, -.data$BoatNum)


  } else if (returnformat == "comprehensive") {
    stop("comprehensive option has not been implemented")
  }


  to.return %>%
    mutate(PerpDistKm = abs(sin(.data$Bearing*pi/180) * .data$DistNm) * 1.852)
}
