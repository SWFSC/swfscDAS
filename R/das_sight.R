#' DAS sightings
#'
#' Extract sightings and associated information from processed DAS data
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}},
#'   or a data frame that can be coerced to a \code{das_df} object
#' @param mixed.multi logical; indicates if mixed-species sightings
#'   should be output in multiple rows
#'
#' @details DAS events contain specific information in the 'Data#' columns,
#'   with the information depending on the event code for that row.
#'   The output data frame contains columns with this specific information
#'   extracted to dedicated columns as described below.
#'   This function recognizes the following types of sightings:
#'   marine mammal sightings (event codes "S", "K", or "M"),
#'   marine mammal resights (codes "s" or "k"),
#'   marine mammal subgroup sightings (code "G"),
#'   marine mammal subgroup resights (code "g"),
#'   turtle sightings (code "t"),
#'   and fishing vessel sightings (code "F").
#'   See \code{\link{das_format_pdf}} for more information about events and event formats.
#'
#'   Abbreviations used in column names: Gs = group size, Sp = species,
#'   Nm = nautical mile, Perc = percentage
#'
#'   This function makes the following assumptions, and alterations to the raw data:
#'   \itemize{
#'     \item "G", "S", "K", and "M" events, and only these events,
#'       are immediately followed by an "A" event.
#'     \item "A" events following an S/K/M event have the same Data1 (sighting number)
#'       as the S/K/M event.
#'       "A" events following a "G" event have a character (e.g. "A" or "B")
#'     \item The 'nSp' column is equivalent to the number of non-\code{NA}
#'       'Data5', 'Data6', 'Data7', and 'Data8' values
#'       for the corresponding "A" event are not \code{NA}, and \code{FALSE} otherwise.
#'       This column has a non-\code{NA} value for only "S", "K", and "M" events
#'     \item The 'Prob' column is \code{TRUE} is the sighting has an associated "?" event,
#'       and \code{FALSE} otherwise.
#'       This column has a non-\code{NA} value for only "S", "K", and "M" events
#'     \item The 'GsTotal' column is the mean of the 'Data2' columns (with \code{NA}s removed)
#'       for the associated "1"-"8" events
#'     \item The 'Sp1Perc', 'Sp2Perc', 'Sp3Perc', and 'Sp4Perc' values are the
#'       means of the 'Data5', 'Data6', 'Data7', and 'Data8' columns (with \code{na.rm = TRUE}),
#'       respectively, for the associated "1"-"8" events
#'     \item The 'GsSp1', 'GsSp2', 'GsSp3', and 'GsSp4' values are the product of
#'       the 'GsTotal' and the respective '...Perc' columns
#'     \item The values for the following columns were capitalized using
#'       \code{\link[base:chartr]{toupper}}:
#'       'Birds', 'Photos', 'TurtleAge', and 'TurtleCapt'
#'   }
#'
#'   Outstanding questions/todo:
#'   \itemize{
#'     \item Should any columns be converted to logicals, e.g. 'Birds', 'Photos', and 'TurtleCapt'?
#'     \item Should \code{NA} values for columns 'Sp1', 'Sp1Perc', 'GsSp1', etc.,
#'       be changed from \code{NA} to \code{0}?
#'     \item TODO: Add flag for option to have comprehensive output of group size estimations
#'   }
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information extracted from 'Data#' columns (described below).
#'   See \code{\link{das_format_pdf}} for more information the sighting information.
#'   The data frame has one row for each sighting,
#'   or one row for each species of each sighting if \code{mixed.multi} is \code{TRUE}.
#'
#'   Added sighting information columns:
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
#'     Probable sighting               \tab Prob  \tab Logical\cr
#'     Number of species in sighting   \tab nSp   \tab \code{NA} for non-S/K/M/G events\cr
#'     Mixed species sighting          \tab Mixed \tab Logical; \code{TRUE} if nSp > 1\cr
#'     Total group size                \tab GsTotal \tab Only different from GsSp if mixed species sighting\cr
#'     Species  code                 \tab Sp      \tab Only present when \code{mixed.multi = TRUE}\cr
#'     Species-specific group size   \tab GsSp    \tab Only present when \code{mixed.multi = TRUE}\cr
#'     Species 1 code                \tab Sp1     \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 2 code                \tab Sp2     \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 3 code                \tab Sp3     \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 4 code                \tab Sp4     \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 1-specific group size \tab Sp1Perc \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 2-specific group size \tab Sp2Perc \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 3-specific group size \tab Sp3Perc \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 4-specific group size \tab Sp4Perc \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 1 group size          \tab GsSp1   \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 2 group size          \tab GsSp2   \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 3 group size          \tab GsSp3   \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Species 4 group size          \tab GsSp4   \tab Only present when \code{mixed.multi = FALSE}\cr
#'     Course of resight group    \tab ResightCourse \tab \code{NA} for non-resight events\cr
#'     Turtle species             \tab TurtleSp   \tab \code{NA} for non-"t" events\cr
#'     Number of turtles          \tab TurtleNum  \tab \code{NA} for non-"t" events\cr
#'     Presence of associated JFR \tab TurtleJFR  \tab \code{NA} for non-"t" events; JFR = jellyfish, floating debris, or red tide \cr
#'     Estimated turtle maturity  \tab TurtleAge  \tab \code{NA} for non-"t" events\cr
#'     Was turtle captured?       \tab TurtleCapt \tab \code{NA} for non-"t" events\cr
#'     Boat or gear type          \tab BoatType   \tab \code{NA} for non-"F" events\cr
#'     Number of boats            \tab BoatNum    \tab \code{NA} for non-"F" events\cr
#'   }
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' das_sight(y.proc)
#' das_sight(y.proc, mixed.multi = TRUE)
#'
#' @export
das_sight <- function(x, mixed.multi) UseMethod("das_sight")


#' @name das_sight
#' @export
das_sight.data.frame <- function(x, mixed.multi = FALSE) {
  das_sight(as_das_df(x), mixed.multi)
}


#' @name das_sight
#' @export
das_sight.das_df <- function(x, mixed.multi = FALSE) {
  #----------------------------------------------------------------------------
  # Filter for sighting-related events
  event.sight <- c("S", "K", "M", "G", "s", "k", "g", "t", "F")
  event.sight.info <- c("A", "?", 1:8)

  sight.df <- x %>%
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>%
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))

  # Check that all GSKM events are followed by an A event
  idx.skm <- which(x$Event %in% c("S", "K", "M"))
  idx.g <- which(x$Event %in% c("G"))
  skmga.check1a <- all(x$Event[c(idx.skm, idx.g) + 1] == "A")
  skmga.check1b <- isTRUE(all.equal(sort(c(idx.skm, idx.g)) + 1, which(x$Event == "A")))
  skmga.check2 <- identical(x$Data1[idx.skm], x$Data1[idx.skm + 1])
  # skmga.check3 <- identical(x$Data2[idx.g], x$Data1[idx.g + 1])

  if (!(skmga.check1a & skmga.check1b)) {
    stop("All 'G', 'S', 'K', and 'M' events (and only these events) ",
         "must be immediately followed by an 'A' event")
  } else if (!skmga.check2) {
    stop("The sighting number in some 'S', 'K', and 'M' events do not match ",
         "the sighting numbers of their corresponding 'A' events")
  }
  rm(skmga.check1a, skmga.check1b, skmga.check2) #skmga.check3


  #----------------------------------------------------------------------------
  # Get applicable data for each type of sighting event

  # TODO: Don't filter for A events - slice based on idx above?

  #--------------------------------------------------------
  ### Data that is in all sighting events
  # SightNo is left as character because of entries such as "408A"
  sight.info.all <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    mutate(SightNo = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data1,
                               .data$Event =="G" ~ .data$Data1,
                               .data$Event %in% c("s", "k") ~ .data$Data1),
           Subgroup = case_when(.data$Event == "G" ~ .data$Data2,
                                .data$Event == "g" ~ .data$Data1),
           Obs = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data2,
                           .data$Event =="G" ~ .data$Data3,
                           .data$Event == "t" ~ .data$Data1,
                           .data$Event == "F" ~ .data$Data1),
           Obs_std = pmap_lgl(list(.data$Obs, .data$ObsL, .data$Rec, .data$ObsR),
                              function(obs, o1, o2, o3) {
                                obs %in% c(o1, o2, o3)
                                # ifelse(is.na(obs), NA, obs %in% c(o1, o2, o3))
                              }),
           Bearing = as.numeric(
             case_when(
               .data$Event %in% c("S", "K", "M", "G") ~ .data$Data5,
               .data$Event %in% c("s", "k", "g") ~ .data$Data2,
               .data$Event == "t" ~ .data$Data3,
               .data$Event == "F" ~ .data$Data2)),
           Reticle = as.numeric(
             case_when(
               .data$Event %in% c("S", "K", "M", "G") ~ .data$Data6,
               .data$Event %in% c("s", "k", "g") ~ .data$Data3,
               .data$Event == "t" ~ .data$Data7,
               .data$Event == "F" ~ .data$Data4)),
           DistNm = as.numeric(
             case_when(
               .data$Event %in% c("S", "K", "M", "G") ~ .data$Data7,
               .data$Event %in% c("s", "k", "g") ~ .data$Data4,
               .data$Event == "t" ~ .data$Data4,
               .data$Event == "F" ~ .data$Data3))) %>%
    select(.data$sight_cumsum, .data$SightNo, .data$Subgroup,
           .data$Obs, .data$Obs_std, .data$Bearing, .data$Reticle, .data$DistNm)


  #--------------------------------------------------------
  ### Marine mammal (+subgroup) sightings; Events S, K, M
  # Other data are extracted in sight.info.all
  sight.info.skmg1 <- sight.df %>%
    filter(.data$Event %in% c("S", "K", "M", "G")) %>%
    mutate(Cue = ifelse(.data$Event == "G", NA, as.numeric(.data$Data3)),
           Method = as.numeric(.data$Data4)) %>%
    select(.data$sight_cumsum, .data$Cue, .data$Method)

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
    select(.data$sight_cumsum, .data$Photos, .data$Birds, .data$nSp,
           .data$Mixed, Sp1 = .data$Data5, Sp2 = .data$Data6,
           Sp3 = .data$Data7, Sp4 = .data$Data8)

  # Data from grouped
  sight.info.skmg3 <- sight.df %>%
    filter(.data$Event %in% c("S", "K", "M", "G")) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(Prob = any("?" %in% .data$Event))

  sight.info.skmg4 <- sight.df %>%
    filter(.data$Event %in% as.character(1:8)) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(GsTotal = mean(as.numeric(.data$Data2), na.rm = TRUE),
              Sp1Perc = mean(as.numeric(.data$Data5), na.rm = TRUE),
              Sp2Perc = mean(as.numeric(.data$Data6), na.rm = TRUE),
              Sp3Perc = mean(as.numeric(.data$Data7), na.rm = TRUE),
              Sp4Perc = mean(as.numeric(.data$Data8), na.rm = TRUE),
              GsSp1 = .data$GsTotal * .data$Sp1Perc / 100,
              GsSp2 = .data$GsTotal * .data$Sp2Perc / 100,
              GsSp3 = .data$GsTotal * .data$Sp3Perc / 100,
              GsSp4 = .data$GsTotal * .data$Sp4Perc / 100) #%>%
  # replace_na(list(Sp2Perc = 0, Sp3Perc = 0, Sp4Perc = 0))
  # @importFrom tidyr replace_na

  num.vec <- c(nrow(sight.info.skmg1), nrow(sight.info.skmg2),
               nrow(sight.info.skmg3), nrow(sight.info.skmg4))
  if (!isTRUE(all.equal(nrow(sight.info.skmg1), nrow(sight.info.skmg2))))
    stop("Unequal number of S/K/M/G and A events. ",
         "This should have been caught earlier?")
  if (!isTRUE(all.equal(nrow(sight.info.skmg1), nrow(sight.info.skmg4))))
    warning("Not all S/K/M/G events have corresponding numeric (1:8) events; ",
            "please check the data using `das_check`")


  sight.info.skmg <- sight.info.skmg1 %>%
    left_join(sight.info.skmg2, by = "sight_cumsum") %>%
    left_join(sight.info.skmg3, by = "sight_cumsum") %>%
    left_join(sight.info.skmg4, by = "sight_cumsum") %>%
    select(.data$sight_cumsum, .data$Cue, .data$Method,
           .data$Photos, .data$Birds,
           .data$Prob, .data$nSp, .data$Mixed, .data$GsTotal, everything())
  rm(sight.info.skmg1, sight.info.skmg2, sight.info.skmg3, sight.info.skmg4)


  #--------------------------------------------------------
  ### Marine mammal (+subgroup) resights; Events s, k, m, g
  sight.info.resight <- sight.df %>%
    filter(.data$Event %in% c("s", "k", "m", "g")) %>%
    mutate(ResightCourse = as.numeric(.data$Data5)) %>%
    select(.data$sight_cumsum, .data$ResightCourse)


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
  ### Fishing boats; Events F
  sight.info.f <- sight.df %>%
    filter(.data$Event == "F") %>%
    mutate(BoatType = .data$Data5,
           BoatNum = as.numeric(.data$Data6)) %>%
    select(.data$sight_cumsum, .data$BoatType, .data$BoatNum)


  #----------------------------------------------------------------------------
  to.return <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    select(-.data$Data1, -.data$Data2, -.data$Data3,
           -.data$Data4, -.data$Data5, -.data$Data6,
           -.data$Data7, -.data$Data8, -.data$Data9) %>%
    left_join(sight.info.all, by = "sight_cumsum") %>%
    left_join(sight.info.skmg, by = "sight_cumsum") %>%
    left_join(sight.info.resight, by = "sight_cumsum") %>%
    left_join(sight.info.t, by = "sight_cumsum") %>%
    left_join(sight.info.f, by = "sight_cumsum") %>%
    # mutate(nSp = ifelse(is.na(.data$nSp), 0, .data$nSp),
    #        Mixed = .data$nSp > 1) %>%
    select(-.data$sight_cumsum)

  # Split multi-species sightings into multiple rows, if necessary
  if (mixed.multi) {
    to.return$idx <- seq_len(nrow(to.return))

    to.return.multi <- to.return %>%
      filter(.data$Event %in% c("S", "K", "M", "G")) %>%
      group_by(.data$idx) %>%
      summarise(Sp1_list = list(c(.data$Sp1, .data$GsSp1)),
                Sp2_list = list(c(.data$Sp2, .data$GsSp2)),
                Sp3_list = list(c(.data$Sp3, .data$GsSp3)),
                Sp4_list = list(c(.data$Sp4, .data$GsSp4))) %>%
      gather(.data$Sp1_list, .data$Sp2_list, .data$Sp3_list, .data$Sp4_list,
             key = "sp_list_name", value = "sp_list", na.rm = TRUE) %>%
      mutate(Sp = map_chr(.data$sp_list, function(i) i[1]),
             GsSp = as.numeric(map_chr(.data$sp_list, function(i) i[2]))) %>%
      filter(!is.na(.data$Sp)) %>%
      select(.data$idx, .data$Sp, .data$GsSp) %>%
      arrange(.data$idx)

    # Names and order of columns to reurn
    names1 <- c(names(sight.df), names(sight.info.all), names(sight.info.skmg))
    names1 <- names1[!(names1 %in% c("sight_cumsum", paste0("Data", 1:9)))]
    names1 <- names1[!grepl("Sp", names1) | names1 == "nSp"]

    names2 <- c(
      names(sight.info.resight), names(sight.info.t), names(sight.info.f)
    )
    names2 <- names2[!(names2 %in% "sight_cumsum")]

    sight.names <- c(names1, "Sp", "GsSp", names2)

    # Finalize mixed.multi return data frame
    to.return <- to.return %>%
      select(-.data$Sp1, -.data$Sp2, -.data$Sp3, -.data$Sp4,
             -.data$Sp1Perc, -.data$Sp2Perc, -.data$Sp3Perc, -.data$Sp4Perc,
             -.data$GsSp1, -.data$GsSp2, -.data$GsSp3, -.data$GsSp4) %>%
      full_join(to.return.multi, by = "idx") %>%
      select(!!sight.names)
  }

  to.return
}
