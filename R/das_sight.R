#' DAS sightings
#'
#' Extract sightings and associated information from processed DAS data
#'
#' @param x \code{das_df} object; output from \code{\link{das_process}}
#' @param mixed.multi logical; indicates if mixed-species sightings should be output in multiple rows
#'
#' @importFrom dplyr %>% .data case_when filter group_by left_join mutate summarise
#' @importFrom purrr pmap
#'
#' @details This function requires that the following assumptions be true:
#'
#'   \itemize{
#'     \item All (and only) "S", "K", and "M" events are immediately followed by an "A" event
#'     \item TODO
#'   }
#'
#' @return Data frame with ...
#'   Associated information (observer, species, etc.) is extracted
#'   from \code{Data#} columns as specified in Details.
#'   Other columns from \code{x} will be included in the output
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
das_sight.data.frame <- function(x, mixed.multi) {
  das_sight(as_das_df(x), mixed.multi)
}


#' @name das_sight
#' @export
das_sight.das_df <- function(x, mixed.multi) {
  #----------------------------------------------------------------------------
  # stop("This function is still under development and should not be used ",
  #      "without consulting the author (sam.woodman@noaa.gov")


  #----------------------------------------------------------------------------
  # Filter for sighting-related events
  event.sight <- c("S", "K", "M", "s", "k", "t", "F")
  event.sight.info <- c("A", "?", 1:8)

  sight.df <- x %>%
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>%
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))

  # Check that all SKM events are followed by an A event
  stopifnot(
    identical(sight.df$Data1[sight.df$Event %in% c("S", "K", "M")],
              sight.df$Data1[sight.df$Event == "A"])
  )

  #----------------------------------------------------------------------------
  # Get applicable data for each type of sighting event

  #--------------------------------------------------------
  ### Data that is in all sighting events
  sight.info.all <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    mutate(Obs = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data2,
                           # .data$Event %in% c("s", "k") ~ NA,
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
                               .data$Event == "F" ~ .data$Data3),
    ) %>%
    select(.data$sight_cumsum,
           .data$Obs, .data$Bearing, .data$Reticle, .data$Dist_nm)


  #--------------------------------------------------------
  ### Marine mammal initial sightings; Events S, K, M
  # SightNo is left as character because of entries such as "408A"
  sight.info.skm1 <- sight.df %>%
    filter(.data$Event %in% c("S", "K", "M")) %>%
    mutate(SightNo = .data$Data1,
           # Obs = as.numeric(.data$Data2),
           # Bearing = as.numeric(.data$Data5),
           # Reticle = as.numeric(.data$Data6),
           # DistNm = as.numeric(.data$Data7),
           Cue = as.numeric(.data$Data3), Method = as.numeric(.data$Data4)) %>%
    select(.data$sight_cumsum, .data$SightNo, .data$Cue, .data$Method)
  # .data$Obs, .data$Bearing, .data$Reticle, .data$DistNm)

  # Data from A row
  sight.info.skm2 <- sight.df %>%
    filter(.data$Event =="A") %>%
    mutate(Birds = toupper(.data$Data4),
           Mixed = unlist(
             pmap(list(.data$Data5, .data$Data6, .data$Data7, .data$Data8),
                  function(d5, d6, d7, d8) {
                    sum(!is.na(c(d5, d6, d7, d8))) > 1
                  })),
           Photos = .data$Data3) %>%
    select(.data$sight_cumsum, .data$Photos, .data$Birds, .data$Mixed,
           Spp1 = .data$Data5, Spp2 = .data$Data6, Spp3 = .data$Data7,
           Spp4 = .data$Data8)

  # Data from grouped
  sight.info.skm3 <- sight.df %>%
    group_by(.data$sight_cumsum) %>%
    summarise(Prob = any("?" %in% .data$Event))

  sight.info.skm4 <- sight.df %>%
    filter(.data$Event %in% as.character(1:8)) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(TotalGS_best = mean(as.numeric(.data$Data2), na.rm = TRUE),
              Sp1_perc = mean(as.numeric(.data$Data5), na.rm = TRUE),
              Sp2_perc = mean(as.numeric(.data$Data6), na.rm = TRUE),
              Sp3_perc = mean(as.numeric(.data$Data7), na.rm = TRUE),
              Sp4_perc = mean(as.numeric(.data$Data8), na.rm = TRUE))

  sight.info.skm <- sight.info.skm1 %>%
    left_join(sight.info.skm2, by = "sight_cumsum") %>%
    left_join(sight.info.skm3, by = "sight_cumsum") %>%
    left_join(sight.info.skm4, by = "sight_cumsum")
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
           turtle_age = .data$Data8,
           turtle_captured = as.logical(.data$Data9)) %>%
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

  to.return

  # if (mixed.multi) {
  #   to.return2 <- to.return %>%
  #     tidyr::gather(Spp1, Spp2, Spp3, Spp4, key = "Spp_all", value = "Spp_code", na.rm = TRUE)
  #
  # } else {
  #   to.return
  # }


  #----------------------------------------------------------------------------
}
