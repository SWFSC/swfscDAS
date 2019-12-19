#' DAS sightings
#'
#' Extract sightings and associated information from processed DAS data
#'
#' @param das.df data frame; processed DAS data.. TODO
#' @param mixed.multi logical; indicates if mixed-species sightings should be output in multiple rows
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr .data
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
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
#'   Other columns from \code{das.df} will be included in the output
#'
#' @examples
#' #TODO
#'
#' @export
das_sight <- function(das.df, mixed.multi) {
  #----------------------------------------------------------------------------
  stop("This function is still under development and should not be used ",
       "without consulting the author (sam.woodman@noaa.gov")

  ### Filter for and extract sighting data
  event.sight <- c("S", "K", "M", "s", "k", "t", "F")
  event.sight.info <- c("A", "?", 1:8)

  ### Filter for sighting-related data
  sight.df <- das.df %>%
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>%
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))

  # ### (temporary) Sanity check
  # event.SKM <- sight.df$Event %in% c("S", "K", "M")
  # event.A <- sight.df$Event == "A"
  # stopifnot(
  #   identical(sight.df$Data1[event.SKM], sight.df$Data1[event.A])
  # )
  # rm(event.SKM, event.A)

  # TODO: temporary
  # sight.df <- sight.df %>%
  #   filter(Event %in% c("S", "K", "M", event.sight.info))
  #----------------------------------------------------------------------------
  # ### Only keep rows for desired sighting events (i.e. 'S', 'M', and/or 'K')
  # das.df.sight <- das.df.sight %>%
  #   filter(SKM_cumsum %in% SKM_cumsum[Event %in% sight.event])

  ### Get applicable data for each sighting event
  # Data from das_process() and from S/K/M row
  # SightNo is character because some sighting numbers are e.g. "408A"
  sight.info1 <- sight.df %>%
    filter(.data$Event %in% c("S", "K", "M")) %>%
    mutate(SightNo = .data$Data1, Obs = as.numeric(.data$Data2),
           Cue = as.numeric(.data$Data3), Method = as.numeric(.data$Data4),
           Bearing = as.numeric(.data$Data5),
           Reticle = as.numeric(.data$Data6),
           DistNm = as.numeric(.data$Data7)) %>%
    select(.data$sight_cumsum, .data$SightNo, .data$Obs, .data$Cue,
           .data$Method, .data$Bearing, .data$Reticle, .data$DistNm)

  # Data from A row
  sight.info2 <- sight.df %>%
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
  sight.info3 <- sight.df %>%
    group_by(.data$sight_cumsum) %>%
    summarise(Prob = any("?" %in% .data$Event))

  sight.info4 <- sight.df %>%
    filter(.data$Event %in% as.character(1:8)) %>%
    group_by(.data$sight_cumsum) %>%
    summarise(TotalGS_best = mean(as.numeric(.data$Data2), na.rm = TRUE),
              Sp1_perc = mean(as.numeric(.data$Data5), na.rm = TRUE),
              Sp2_perc = mean(as.numeric(.data$Data6), na.rm = TRUE),
              Sp3_perc = mean(as.numeric(.data$Data7), na.rm = TRUE),
              Sp4_perc = mean(as.numeric(.data$Data8), na.rm = TRUE))

  sight.skm <- sight.info1 %>%
    left_join(sight.info2, by = "sight_cumsum") %>%
    left_join(sight.info3, by = "sight_cumsum") %>%
    left_join(sight.info4, by = "sight_cumsum")


  #----------------------------------------------------------------------------
  to.return <- sight.df %>%
    filter(.data$Event %in% event.sight) %>%
    left_join(sight.skm, by = "sight_cumsum") %>%
    select(-.data$sight_cumsum)

  to.return

  # if (mixed.multi) {
  #   to.return2 <- to.return %>%
  #     tidyr::gather(Spp1, Spp2, Spp3, Spp4, key = "Spp_all", value = "Spp_code", na.rm = TRUE)
  #
  # } else {
  #   to.return
  # }


  # #----------------------------------------------------------------------------
  # # Get applicable information for each type of sighting
  #
  # ### All sightings
  # sight.info1 <- sight.df %>%
  #   filter(Event %in% event.sight) %>%
  #   mutate(Obs = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data2,
  #                          .data$Event %in% c("s", "k") ~ NA,
  #                          .data$Event == "t" ~ .data$Data1,
  #                          .data$Event == "F" ~ .data$Data1),
  #          Bearing = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data5,
  #                              .data$Event %in% c("s", "k") ~ .data$Data2,
  #                              .data$Event == "t" ~ .data$Data3,
  #                              .data$Event == "F" ~ .data$Data2),
  #          Reticle = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data6,
  #                              .data$Event %in% c("s", "k") ~ .data$Data3,
  #                              .data$Event == "t" ~ .data$Data7,
  #                              .data$Event == "F" ~ .data$Data4),
  #          Dist_nm = case_when(.data$Event %in% c("S", "K", "M") ~ .data$Data7,
  #                              .data$Event %in% c("s", "k") ~ .data$Data4,
  #                              .data$Event == "t" ~ .data$Data4,
  #                              .data$Event == "F" ~ .data$Data3)) %>%
  #   select(sight_cumsum, Obs, Bearing, Reticle, Dist_nm)
  #
  # ### Marine mammal ('S', 'K', 'M')
  #
  # ### Marine mammal resight ('s', 'k')
  #
  # ### Turtle ('t')
  # sight.info4 <- sight.df %>%
  #   filter(Event == "t") %>%
  #   mutate(sp_turtle = .data$Data2,
  #          num_turtle = .data$Data5,
  #          jfr = .data$Data6)
  #
  #
  # ### Fishing vessel ('F')
}
