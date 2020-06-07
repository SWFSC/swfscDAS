#' Process and summarize shipboard DAS data
#'
#' This package contains functions designed for processing and analyzing DAS data
#' generated using the WinCruz program by the Southwest Fisheries Science Center.
#' Functionality currently includes reading DAS data into a data frame,
#' processing this data (extracting state and condition information for each DAS event),
#' and summarizing sighting and effort information.
#'
#' @name swfscDAS-package
#' @aliases swfscDAS
#' @docType package
#' @title Southwest Fisheries Science Center DAS
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://smwoodman.github.io/swfscDAS/}
#'
#' @importFrom dplyr arrange between bind_cols case_when desc everything filter full_join group_by
#'   left_join mutate n right_join select slice starts_with summarise ungroup
#' @importFrom lubridate year month day tz
#' @importFrom magrittr %>%
#' @importFrom parallel clusterExport detectCores parLapplyLB stopCluster
#' @importFrom readr cols col_character fwf_positions read_fwf
#' @importFrom rlang !! .data
#' @importFrom purrr map_chr pmap pmap_lgl
#' @importFrom stats na.omit runif
#' @importFrom swfscMisc bearing destination distance setupClusters
#' @importFrom tidyr gather
#' @importFrom utils head read.csv write.csv
#'
#' @keywords package
NULL

setOldClass("das_dfr")
setOldClass("das_df")
