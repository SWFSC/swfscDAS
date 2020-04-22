#' Convert randpicks file
#'
#' Convert randpicks file from segchopr format to swfscAirDAS format
#'
#' @param x.randpicks Data frame with two columns;
#' randpick values formatted for segchopr that correspond to \code{x.segdata}
#' @param x.segdata Data frame; segdata that corresponds to \code{x.randpicks}
#' @param seg.km numeric; target segment length used when creating \code{x.segdata}
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
  x.summ.check <- x.segdata.summ %>%
    mutate(dist_max = .data$count * seg.km + 0.5 * seg.km,
           dist_check = .data$dist_max >= .data$dist_sum)

  if (!all(x.summ.check$dist_check))
    warning("Error in segdata distances - please report this as an issue")

  # Check that number of randpicks equals the number of
  #   continuous effort sections >= seg.km
  check1 <- isTRUE(all.equal(sum(x.segdata$stlin == 1), nrow(x.segdata.summ)))
  check2 <- isTRUE(all.equal(sum(x.segdata.summ$dist_sum >= seg.km),
                             nrow(x.randpicks)))
  if (!check1 | !check2)
    stop("Error - the provided randpicks and segdata files are not compatible")


  # Prep segdata summary info for joining with randpicks
  rand.out <- x.segdata.summ %>%
    filter(.data$dist_sum > seg.km) %>%
    bind_cols(x.randpicks) %>%
    mutate(pos_value = ceiling(.data$RandPick * .data$count))

  # 'Expand' randpicks data to include all continuous effort sections
  x.segdata.summ %>%
    select(effort_section = .data$cont_eff_sect) %>%
    left_join(rand.out, by = c("effort_section" = "cont_eff_sect")) %>%
    select(.data$effort_section, randpicks = .data$pos_value)
}
