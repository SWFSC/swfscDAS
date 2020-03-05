#' Convert randpicks file
#'
#' Convert randpicks file from segchopr format to swfscAirDAS format
#'
#' @param x.randpicks Data frame with two columns;
#' randpick values formatted for segchopr that correspond to \code{x.segdata}
#' @param x.segdata Data frame; segdata that corresponds to \code{x.randpicks}
#' @param seg.km numeric; target segment length used when creating \code{x.segdata}
#'
#' @importFrom dplyr %>% .data bind_cols group_by mutate n right_join select summarise
#'
#' @details Past DAS processing code used a different randpicks format,
#'   where only the generated random values were recorded.
#'   This function 'converts' a data frame with those values to a data frame
#'   in the format required by this package
#'
#' @return Data frame with one line for each continuous effort section,
#'   and two columns: \code{effort_section} and \code{randpicks}
#'
#' @examples
#' #TODO
#' #randpicks_convert(eab.randpicks, eab.segdata, 5)
#'
#' @export
randpicks_convert <- function(x.randpicks, x.segdata, seg.km) {
  # For each continuous effort section, determine the number of segments
  x.segdata.summ <- x.segdata %>%
    mutate(cont_eff_sect = cumsum(.data$stlin == 1)) %>%
    group_by(.data$cont_eff_sect) %>%
    summarise(count = n(),
              dist_sum = sum(.data$dist))

  # Check that number of randpicks equals the number of
  #   continuous effort sections >= seg.km
  check1 <- isTRUE(all.equal(sum(x.segdata$stlin == 1), nrow(x.segdata.summ)))
  check2 <- isTRUE(all.equal(sum(x.segdata.summ$dist_sum >= seg.km),
                             nrow(x.randpicks)))
  if (!check1 | !check2)
    stop("Error - the provided randpicks and segdata files are not compatible")

  x.randpicks %>%
    bind_cols(filter(x.segdata.summ, .data$dist_sum > seg.km)) %>%
    mutate(pos_value = ceiling(.data$RandPick * .data$count)) %>%
    select(effort_section = .data$cont_eff_sect, .data$pos_value) %>%
    right_join(data.frame(effort_section = seq_len(nrow(x.segdata.summ))),
               by = "effort_section") %>%
    select(.data$effort_section, randpicks = .data$pos_value)
}
