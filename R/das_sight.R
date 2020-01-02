#' DAS sightings
#'
#' Extract sightings and associated information from processed DAS data
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}}.
#'  Can also be a data frame that can be coerced to a \code{das_df} object
#' @param mixed.multi logical; indicates if mixed-species sightings should be output in multiple rows
#'
#' @importFrom dplyr %>% .data arrange case_when everything filter full_join group_by left_join mutate starts_with summarise
#' @importFrom purrr map_chr pmap
#' @importFrom tidyr gather
#'
#' @details DAS events contain specific information in the 'Data#' columns,
#'   with the information depending on the event code for that row.
#'   The output data frame contains columns with this specific information extracted to dedicated columns as described below.
#'   This function recognizes the following types of sightings: marine mammal sightings (event codes "S", "K", or "M"),
#'   marine mammal resights (codes "s" or "K"), turtle sightings (code "t"), and fishing vessel sightings (code "F").
#'   See the format PDF, \code{\link{das_format_pdf}}, for the required format for each of these events.
#'
#'   \tabular{llllll}{
#'     \emph{Information} \tab \emph{Mammal sighting} \tab \emph{Mammal resight} \tab \emph{Turtle} \tab
#'       \emph{Vessel} \tab \emph{Output column name}\cr
#'     Sighting number \tab Data1 \tab \tab sight_no\cr
#'     Observer \tab Data2 \tab \tab Data1 \tab Data1 \tab Obs\cr
#'     Cue \tab Data3 \tab Data2 \tab angle_declination\cr
#'     Group size (best estimate) \tab Data4 \tab NA \tab todo \cr
#'     Species code 1 \tab Data5 \tab Data3 \tab todo \cr
#'     Species code 2 \tab Data6 \tab NA \tab todo \cr
#'     Species code 3 \tab Data7 \tab NA \tab todo \cr
#'     Turtle size (ft) \tab NA \tab Data4 \tab todo\cr
#'     Travel direction (deg) \tab NA \tab Data5 \tab todo\cr
#'     Tail visible? \tab NA \tab Data6 \tab todo\cr
#'   }
#'
#'   This function makes the following assumptions, and alterations to the raw data:
#'   \itemize{
#'     \item "S", "K", and "M" events, and only these events, are immediately followed by an "A" event
#'     \item The 'Mixed' column is \code{TRUE} if two or more of the 'Data5', 'Data6', 'Data7', and 'Data8' values
#'       for the corresponding "A" event are not \code{NA}, and \code{FALSE} otherwise.
#'       This column has a non-\code{NA} value for only "S", "K", and "M" events
#'     \item The 'Prob' column is \code{TRUE} is the sighting has an associated "?" event,
#'       and \code{FALSE} otherwise.
#'       This column has a non-\code{NA} value for only "S", "K", and "M" events
#'     \item The 'GS_total_best' column is the mean of the 'Data2' columns (with \code{NA}s removed) for the associated "1"-"8" events
#'     \item The 'Sp1_perc', 'Sp2_perc', 'Sp3_perc', and 'Sp4_perc' columns are the
#'       means of the 'Data5', 'Data6', 'Data7', and 'Data8' columns (with \code{NA}s removed), respectively,
#'       for the associated "1"-"8" events
#'     \item The values for the following columns were capitalized using \code{\link[base:chartr]{toupper}}:
#'       'Birds', 'Photos', 'turtle_age', and 'turtle_captured'
#'   }
#'
#'   Outstanding questions:
#'   \itemize{
#'     \item Should any columns be converted to logicals,
#'       e.g. 'Birds', 'Photos', 'turtle_age', and 'turtle_captured'?
#'     \item Should \code{NA} values for columns 'Sp1', 'Sp1_perc', 'GS_Sp1', etc.,
#'       be changed from \code{NA} to \code{0}?
#'   }
#'
#' @return Data frame with 1) the columns from \code{x} and 2) columns with sighting information
#'   (observer, species, etc.) extracted from \code{Data#} columns as specified in Details.
#'   The data frame has one row for each sighting,
#'   or one row for each species of each sighting if \code{mixed.multi} is \code{TRUE}.
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' das_sight(y.proc)
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
  event.sight <- c("S", "K", "M", "s", "k", "t", "F")
  event.sight.info <- c("A", "?", 1:8)

  sight.df <- x %>%
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>%
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))

  # Check that all SKM events are followed by an A event
  skma.check <- identical(
    sight.df$Data1[sight.df$Event %in% c("S", "K", "M")],
    sight.df$Data1[sight.df$Event == "A"]
  )
  if (!skma.check)
    stop("All 'S', 'K', and 'M' events (and only these events) ",
         "must be immediately followed by an 'A' event")
  rm(skma.check)


  #----------------------------------------------------------------------------
  # Get applicable data for each type of sighting event

  #--------------------------------------------------------
  ### Data that is in all sighting events
  sight.info.all <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    mutate(Obs = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data2,
                           .data$Event == "t" ~ .data$Data1,
                           .data$Event == "F" ~ .data$Data1),
           Bearing = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data5,
                               .data$Event %in% c("s", "k") ~ .data$Data2,
                               .data$Event == "t" ~ .data$Data3,
                               .data$Event == "F" ~ .data$Data2),
           Reticle = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data6,
                               .data$Event %in% c("s", "k") ~ .data$Data3,
                               .data$Event == "t" ~ .data$Data7,
                               .data$Event == "F" ~ .data$Data4),
           Dist_nm = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data7,
                               .data$Event %in% c("s", "k") ~ .data$Data4,
                               .data$Event == "t" ~ .data$Data4,
                               .data$Event == "F" ~ .data$Data3)) %>%
    select(.data$sight_cumsum,
           .data$Obs, .data$Bearing, .data$Reticle, .data$Dist_nm)


  #--------------------------------------------------------
  ### Marine mammal initial sightings; Events S, K, M
  # SightNo is left as character because of entries such as "408A"
  # Obs, bearing, Reticle, and DistNm are extracted in sight.info.all
  sight.info.skm1 <- sight.df %>%
    filter(.data$Event %in% c("S", "K", "M")) %>%
    mutate(SightNo = .data$Data1,
           Cue = as.numeric(.data$Data3), Method = as.numeric(.data$Data4)) %>%
    select(.data$sight_cumsum, .data$SightNo, .data$Cue, .data$Method)

  # Data from A row
  sight.info.skm2 <- sight.df %>%
    filter(.data$Event =="A") %>%
    mutate(Birds = toupper(.data$Data4),
           Mixed = unlist(
             pmap(list(.data$Data5, .data$Data6, .data$Data7, .data$Data8),
                  function(d5, d6, d7, d8) {
                    sum(!is.na(c(d5, d6, d7, d8))) > 1
                  })),
           Photos = toupper(.data$Data3)) %>%
    select(.data$sight_cumsum, .data$Photos, .data$Birds, .data$Mixed,
           Sp1 = .data$Data5, Sp2 = .data$Data6, Sp3 = .data$Data7,
           Sp4 = .data$Data8)

  # Data from grouped
  sight.info.skm3 <- sight.df %>%
    group_by(.data$sight_cumsum) %>%
    summarise(Prob = any("?" %in% .data$Event))

  sight.info.skm4 <- sight.df %>%
    filter(.data$Event %in% as.character(1:8)) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(GS_total_best = mean(as.numeric(.data$Data2), na.rm = TRUE),
              Sp1_perc = mean(as.numeric(.data$Data5), na.rm = TRUE),
              Sp2_perc = mean(as.numeric(.data$Data6), na.rm = TRUE),
              Sp3_perc = mean(as.numeric(.data$Data7), na.rm = TRUE),
              Sp4_perc = mean(as.numeric(.data$Data8), na.rm = TRUE),
              GS_Sp1 = .data$GS_total_best * .data$Sp1_perc / 100,
              GS_Sp2 = .data$GS_total_best * .data$Sp2_perc / 100,
              GS_Sp3 = .data$GS_total_best * .data$Sp3_perc / 100,
              GS_Sp4 = .data$GS_total_best * .data$Sp4_perc / 100) #%>%
  # replace_na(list(Sp2_perc = 0, Sp3_perc = 0, Sp4_perc = 0))
  # @importFrom tidyr replace_na

  sight.info.skm <- sight.info.skm1 %>%
    left_join(sight.info.skm2, by = "sight_cumsum") %>%
    left_join(sight.info.skm3, by = "sight_cumsum") %>%
    left_join(sight.info.skm4, by = "sight_cumsum") %>%
    select(.data$sight_cumsum, .data$SightNo, .data$Cue, .data$Method, .data$Photos, .data$Birds,
           .data$Mixed, .data$Prob, .data$GS_total_best, everything())
  rm(sight.info.skm1, sight.info.skm2, sight.info.skm3, sight.info.skm4)


  #--------------------------------------------------------
  ### Marine mammal resights; Events s, k, m
  sight.info.resight <- sight.df %>%
    filter(.data$Event %in% c("s", "k", "m")) %>%
    mutate(resight_course = as.numeric(.data$Data5)) %>%
    select(.data$sight_cumsum, .data$resight_course)


  #--------------------------------------------------------
  ### Turtle sightings; Events t
  sight.info.t <- sight.df %>%
    filter(.data$Event == "t") %>%
    mutate(turtle_sp = .data$Data2,
           turtle_num = as.numeric(.data$Data5),
           turtle_jfr = .data$Data6,
           turtle_age = toupper(.data$Data8),
           turtle_captured = toupper(.data$Data9)) %>%
    select(.data$sight_cumsum, .data$turtle_sp, .data$turtle_num,
           .data$turtle_jfr, .data$turtle_age, .data$turtle_captured)


  #--------------------------------------------------------
  ### Fishing boats; Events F
  sight.info.f <- sight.df %>%
    filter(.data$Event == "F") %>%
    mutate(boat_type = .data$Data5,
           boat_num = as.numeric(.data$Data6)) %>%
    select(.data$sight_cumsum, .data$boat_type, .data$boat_num)


  #----------------------------------------------------------------------------
  to.return <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    left_join(sight.info.all, by = "sight_cumsum") %>%
    left_join(sight.info.skm, by = "sight_cumsum") %>%
    left_join(sight.info.resight, by = "sight_cumsum") %>%
    left_join(sight.info.t, by = "sight_cumsum") %>%
    left_join(sight.info.f, by = "sight_cumsum") %>%
    select(-.data$sight_cumsum)

  # Split multi-species sightings into multiple rows, if necessary
  if (mixed.multi) {
    to.return$idx <- seq_len(nrow(to.return))

    to.return.multi <- to.return %>%
      filter(.data$Event %in% c("S", "K", "M")) %>%
      group_by(.data$idx) %>%
      summarise(Sp1_list = list(c(.data$Sp1, .data$GS_Sp1)),
                Sp2_list = list(c(.data$Sp2, .data$GS_Sp2)),
                Sp3_list = list(c(.data$Sp3, .data$GS_Sp3)),
                Sp4_list = list(c(.data$Sp4, .data$GS_Sp4))) %>%
      gather(.data$Sp1_list, .data$Sp2_list, .data$Sp3_list, .data$Sp4_list,
             key = "Sp_list_name", value = "Sp_list", na.rm = TRUE) %>%
      mutate(Species = map_chr(.data$Sp_list, function(i) i[1]),
             GS_species = as.numeric(map_chr(.data$Sp_list, function(i) i[2]))) %>%
      filter(!is.na(.data$Species)) %>%
      select(.data$idx, .data$Species, .data$GS_species) %>%
      arrange(.data$idx)

    to.return %>%
      select(-starts_with("Sp"), -starts_with("GS_Sp")) %>%
      full_join(to.return.multi, by = "idx") %>%
      select(1:40, .data$Species, .data$GS_species, .data$resight_course,
             starts_with("turtle"), starts_with("boat"))

    # TODO: Make unit test ensuring that first 40 column names of das_sight() are always:
    # c("Event", "DateTime", "Lat", "Lon", "OnEffort", "Cruise", "Mode", "EffType", "Course",
    #   "Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Glare", "Vis",
    #   "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9",
    #   "EventNum", "file_das", "line_num", "Obs", "Bearing", "Reticle", "Dist_nm",
    #   "SightNo", "Cue", "Method", "Photos", "Birds", "Mixed", "Prob", "GS_total_best")


  } else {
    to.return
  }
}
