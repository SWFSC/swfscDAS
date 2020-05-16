#' Summarize DAS sightings by effort segment
#'
#' Summarize number of sightings and animals for selected species by segment
#'
#' @param x.list list; output of \code{\link{das_effort}}
#' @param sp.codes character; species code(s) to include in segdata output
#'
#' @details This function takes the output of \code{\link{das_effort}} and
#'   adds columns for the number of sightings (nSI) and number of animals (ANI)
#'   for selected species (selected via \code{sp.codes}) for each segment
#'   to the segdata element of \code{x.list}.
#'   However, only sightings with an included value of \code{TRUE}
#'   (included is a column in siteinfo) are included in the summaries.
#'   Having this step separate from \code{\link{das_effort}} allows users to
#'   personalize the included values as desired for their analysis.
#'
#' @return A list, identical to \code{x.list} except for
#'   1) the nSI and ANI columns added to \code{x.list$segdata},
#'   one each for each element of \code{sp.codes}, and
#'   2) the included column of \code{x.list$siteinfo}, which has been set as
#'   \code{FALSE} for sightings of species not listed in \code{sp.codes}
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#' y.cond <- das_effort(
#'   y.proc, method = "condition",
#'   conditions = "Bft", seg.min.km = 0.05, num.cores = 1
#' )
#'
#' y.cond$siteinfo <- y.cond$siteinfo[y.cond$siteinfo$Event %in% c("S", "t"), ]
#' y.cond$siteinfo$included <- TRUE
#'
#' das_effort_sight(y.cond, sp.codes = c("013", "076", "DC"))
#'
#' @export
das_effort_sight <- function(x.list, sp.codes) {
  ### Input checks
  stopifnot(
    inherits(x.list, "list"),
    inherits(sp.codes, "character"),
    identical(names(x.list), c("segdata", "siteinfo", "randpicks")),
    "included" %in% names(x.list$siteinfo)
  )

  ### Prep
  segdata <- x.list$segdat
  siteinfo <- x.list$siteinfo
  randpicks <- x.list$randpicks

  ### Processing
  # Prep sp.codes
  sp.codes <- sort(sp.codes)
  if (!all(sp.codes %in% siteinfo$Sp))
    warning("The following species codes are not present in the provided data: ",
            paste(sp.codes[!(sp.codes %in% siteinfo$Sp)], collapse = ", "))

  # Make data frame with nSI and ANI columns, and join it with segdata
  segdata$seg_idx <- paste(segdata$section_id, segdata$section_sub_id, sep = "_")
  siteinfo <- left_join(siteinfo, select(segdata, .data$segnum, .data$seg_idx),
                        by = "segnum")

  segdata.col1 <- select(segdata, .data$seg_idx)
  siteinfo.forsegdata.list <- lapply(sp.codes, function(i, siteinfo, d1) {
    d0 <- siteinfo %>%
      filter(.data$included, .data$Sp == i) %>%
      group_by(.data$seg_idx) %>%
      summarise(nSI = length(.data$Sp),
                ANI = sum(.data$GsSp))

    names(d0) <- c("seg_idx", paste0(i, "_", names(d0)[-1]))

    z <- full_join(d1, d0, by = "seg_idx") %>% select(-.data$seg_idx)
    z[is.na(z)] <- 0

    z
  }, siteinfo = siteinfo, d1 = segdata.col1)

  siteinfo.forsegdata.df <- bind_cols(segdata.col1, siteinfo.forsegdata.list)


  ### Clean up and return
  segdata <- segdata %>%
    left_join(siteinfo.forsegdata.df, by = "seg_idx") %>%
    select(-.data$seg_idx)

  siteinfo <- siteinfo %>%
    mutate(included = ifelse(.data$Sp %in% sp.codes, .data$included, FALSE)) %>%
    select(-.data$seg_idx)

  list(segdata = segdata, siteinfo = siteinfo, randpicks = randpicks)
}
