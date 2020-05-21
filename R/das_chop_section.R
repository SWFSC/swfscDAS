#' Chop DAS data - section
#'
#' Chop DAS data into effort segments by continuous effort section
#'
#' @param x \code{das_df} object,
#'   or a data frame that can be coerced to a \code{das_df} object.
#'   This data must be filtered for 'OnEffort' events;
#'   see the Details section below
#' @param ... ignored
#' @param conditions see \code{\link{das_effort}}
#' @param distance.method character; see \code{\link{das_effort}}.
#'   Default is \code{NULL} since these distances should have already been calculated
#' @param num.cores see \code{\link{das_effort}}
#'
#' @details This function is simply a wrapper for \code{\link{das_chop_equal}}.
#'   It calls \code{\link{das_chop_equal}}, with \code{seg.km} set to a
#'   value larger than the longest continuous effort section in \code{x}.
#'   Thus, the effort is 'chopped' into the continuous effort sections and then summarized.
#'
#'   See the Examples section for an example where the two methods give the same output.
#'   Note that the longest continuous effort section in the sample data is ~22km.
#'
#' @return See \code{\link{das_chop_equal}}. The randpicks values will all be \code{NA}
#'
#' @examples
#' \dontrun{
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' y.eff1 <- das_effort(
#'   y.proc, method = "equallength", seg.km = 25, num.cores = 1
#' )
#'
#' y.eff2 <- das_effort(y.proc, method = "section", num.cores = 1)
#'
#'
#' all.equal(y.eff1, y.eff2)
#' }
#'
#' @keywords internal
#'
#' @seealso das_chop_condition, das_chop_equal
#'
#' @export
das_chop_section <- function(x, ...) UseMethod("das_chop_section")


#' @name das_chop_section
#' @export
das_chop_section.data.frame <- function(x, ...) {
  das_chop_section(as_das_df(x), ...)
}


#' @name das_chop_section
#' @export
das_chop_section.das_df <- function(x, conditions, distance.method = NULL,
                                          num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!all(x$OnEffort | x$Event %in% c("O", "E")))
    stop("x must be filtered for on effort events; see `?das_chop_equal")

  conditions <- .das_conditions_check(conditions, "section")


  #----------------------------------------------------------------------------
  # Calculate distance between points if necessary
  if (!("dist_from_prev" %in% names(x))) {
    if (is.null(distance.method))
      stop("If the distance between consectutive points (events) ",
           "has not already been calculated, ",
           "then you must provide a valid argument for distance.method")

    x$dist_from_prev <- .dist_from_prev(x, distance.method)
  }

  #----------------------------------------------------------------------------
  # ID continuous effort sections, make randpicks, and get max section length
  if (!("cont_eff_section" %in% names(x))) {
    x$cont_eff_section <- cumsum(x$Event %in% c("R"))
  }

  randpicks.df <- data.frame(
    effort_section = sort(unique(x$cont_eff_section)),
    randpicks = NA
  )

  x.summ <- x %>%
    mutate(ces_dup = duplicated(.data$cont_eff_section),
           dist_from_prev_sect = ifelse(.data$ces_dup, .data$dist_from_prev, NA)) %>%
    group_by(.data$cont_eff_section) %>%
    summarise(dist_sum = sum(.data$dist_from_prev_sect, na.rm = TRUE))

  # Call das_chop_equal using max section length + 1
  das_chop_equal(
    x %>% select(-.data$cont_eff_section),
    conditions = conditions,
    seg.km = max(x.summ$dist_sum) + 1, randpicks.load = randpicks.df,
    num.cores = num.cores
  )
}
