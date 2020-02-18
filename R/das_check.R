#' Check DAS file
#'
#' Check that DAS file has accepted values
#'
#' @param file filename(s) of one or more DAS files
#' @param file.out filename to which to write the error log;
#'   default is \code{NULL}
#'
#' @importFrom dplyr left_join
#' @importFrom utils write.csv
#'
#' @details
#' Precursor to DASCHECK. Checks that the following is true:
#' \itemize{
#'   \item Event codes are one of the following: #, *, ?, 1, 2, 3, 4, 5, 6, 7, 8,
#'     A, B, C, E, F, k, K, N, P, Q, R, s, S, t, V, W, g, p, X, Y, Z.
#'   \item The effort dot matches effort determined using B, R, and E events
#'   \item And event/column pairs meet the following requirements:
#' }
#'
#' \tabular{llll}{
#'   \emph{Item} \tab \emph{Event} \tab \emph{Column} \tab \emph{Requirement}\cr
#'   Cruise number  \tab B \tab Data1 \tab Can be converted to a numeric value\cr
#'   Mode           \tab B \tab Data2 \tab Must be one of C, P, c, p, or NA (blank)\cr
#'   Effort type    \tab R \tab Data1 \tab Must be one of F, N, S, or NA (blank)\cr
#'   Course         \tab N \tab Data1 \tab Can be converted to a numeric value\cr
#'   Beaufort       \tab V \tab Data1 \tab Can be converted to a numeric value\cr
#'   Swell height   \tab V \tab Data2 \tab Can be converted to a numeric value\cr
#'   Rain or fog    \tab W \tab Data1 \tab Can be converted to a numeric value\cr
#'   Horizontal sun \tab W \tab Data2 \tab Can be converted to a numeric value\cr
#'   Vertical sun   \tab W \tab Data3 \tab Can be converted to a numeric value\cr
#'   Visibility     \tab W \tab Data5 \tab Can be converted to a numeric value\cr
#'   Sighting (mammal) \tab S, K, M \tab Data3-7    \tab Can be converted to a numeric value\cr
#'   Photos \tab A \tab Data3 \tab Must be one of N, Y, n, y, or NA (blank)\cr
#'   Birds  \tab A \tab Data4 \tab Must be one of N, Y, n, y, or NA (blank)\cr
#'   Resighting      \tab s, k    \tab Data2-5    \tab Can be converted to a numeric value\cr
#'   Turtle sighting \tab t       \tab Data3-5, 7 \tab Can be converted to a numeric value\cr
#'   JFR             \tab t \tab Data6 \tab Must be one of F, J, N, R, or NA (blank)\cr
#'   Fishing vessel  \tab F       \tab Data2-4    \tab Can be converted to a numeric value\cr
#'   Sighting info \tab 1-8     \tab Data2-8    \tab Can be converted to a numeric value\cr
#'   Sighting info \tab 1-8 \tab Data9 \tab The Data9 column must be NA (blank) for events 1-8\cr
#' }
#'
#' Outstanding questions:
#' \itemize{
#'   \item Documentation says Data8 and Data9 for SKM events be numeric, but currently ~5000 lines are not
#'   \item What to add?
#'   \item How to check that everything is right-justified?
#' }
#'
#' @return
#' A data frame with five columns: the file name, line number,
#' index (row number) from the \code{das_read(file)} data frame,
#' 'ID' (columns 4-39 from the DAS file), and description of the issue
#'
#' If \code{file.out} is not \code{NULL}, then the error log is also
#' written to a text file
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' das_check(y)
#'
#' @export
das_check <- function(file, file.out = NULL) {
  error.out <- data.frame(
    File = NA, LineNum = NA, Idx = NA, ID = NA, Description = NA,
    stringsAsFactors = FALSE
  )
  x <- das_read(file)
  x.lines <- substr(readLines(file), 4, 39)
  stopifnot(nrow(x) == length(x.lines))


  #----------------------------------------------------------------------------
  ### Check event codes
  event.acc <- c("#", "*", "?", 1:8, "A", "B", "C", "E", "F", "k", "K", "N",
                 "P", "Q", "R", "s", "S", "t", "V", "W",
                 "g", "p", "X", "Y", "Z")
  ev.which <- which(!(x$Event %in% event.acc))
  error.out <- rbind(
    error.out,
    list(x$file_das[ev.which], x$line_num[ev.which], ev.which,
         x.lines[ev.which],
         rep("The event code is not recognized", length(ev.which)))
  )


  #----------------------------------------------------------------------------
  ### Check that effort dot matches effort determined by B/R to E events
  ndx.B <- which(x$Event == "B")
  ndx.R <- which(x$Event == "R")
  ndx.E <- which(x$Event == "E")

  if (length(ndx.E) != length(ndx.R)) {
    error.out <- rbind(
      error.out,
      list(NA, NA, NA, NA,
           paste("Error: There are not an equal number of 'R' and 'E'",
                 "events in the provided DAS file"))
    )

  } else if (!all(ndx.E - ndx.R > 0) | !all(head(ndx.E, -1) < ndx.R[-1])) {
    e.which <- which(
      !all(ndx.E - ndx.R > 0) | !all(head(ndx.E, -1) < ndx.R[-1])
    )
    error.out <- rbind(
      error.out,
      list(x$file_das[e.which], x$line_num[e.which], e.which, x.lines[e.which],
           "Error: Not all 'R' events are followed by 'E' events")
    )

  } else {
    x.eff.idx <- unlist(mapply(function(i, j) {
      i:(j-1)
    }, ndx.R, ndx.E, SIMPLIFY = FALSE))
    ndx.B.preR <- ndx.B[(ndx.B + 1) %in% ndx.R]
    x.eff <- seq_len(nrow(x)) %in% c(x.eff.idx, ndx.B.preR)

    e.which <- which(
      (x.eff != x$EffortDot) & !(x$Event %in% c("?", 1:8, "#"))
    )

    error.out <- rbind(
      error.out,
      list(x$file_das[e.which], x$line_num[e.which], e.which, x.lines[e.which],
           rep("Effort dot does not match B/R to E effort", length(e.which)))
    )
  }


  #----------------------------------------------------------------------------
  ### Check that value type of values in Data# columns are as expected for
  ###   columns that are added in das_process

  # Variables are code named as "z".event code'.'data# column' for
  #   line numbers with weird info,
  #   and "txt".event code'.'data# column' for the txt to go in error.out

  # Cruise number
  idx.b.1 <- .check_numeric(x, "B", "Data1")
  txt.b.1 <- "Cruise number (Data1 of B events) cannot be converted to a numeric"

  # Mode
  idx.b.2 <- .check_character(x, "B", "Data2", c("C", "P", "c", "p", NA))
  txt.b.2 <- "Effort type (Data2 of B events) is not one of C, P, c, p, or NA"

  # Effort type
  idx.r.1 <- .check_character(x, "R", "Data1", c("F", "N", "S", NA))
  txt.r.1 <- "Effort type (Data1 of R events) is not one of F, N, S, or NA"

  # Course
  idx.n.1 <- .check_numeric(x, "N", "Data1")
  txt.n.1 <- "Course (Data1 of N events) cannot be converted to a numeric"

  # Beaufort
  idx.v.1 <- .check_numeric(x, "V", "Data1")
  txt.v.1 <- "Beaufort (Data1 of V events) cannot be converted to a numeric"

  # Swell Height
  idx.v.2 <- .check_numeric(x, "V", "Data2")
  txt.v.2 <- "Swell height (Data2 of V events) cannot be converted to a numeric"

  # RainFog
  idx.w.1 <- .check_numeric(x, "W", "Data1")
  txt.w.1 <- "Rain/fog (Data1 of W events) cannot be converted to a numeric"

  # Horizontal sun
  idx.w.2 <- .check_numeric(x, "W", "Data2")
  txt.w.2 <- "Horizontal sun (Data2 of W events) cannot be converted to a numeric"

  # Vertical sun
  idx.w.3 <- .check_numeric(x, "W", "Data3")
  txt.w.3 <- "Vertical sun (Data3 of W events) cannot be converted to a numeric"

  # Visibility
  idx.w.5 <- .check_numeric(x, "W", "Data5")
  txt.w.5 <- "Visibility (Data5 of W events) cannot be converted to a numeric"


  # Add text to error.out as needed and return
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.b.1, txt.b.1),
    .check_list(x, x.lines, idx.b.2, txt.b.2),
    .check_list(x, x.lines, idx.r.1, txt.r.1),
    .check_list(x, x.lines, idx.n.1, txt.n.1),
    .check_list(x, x.lines, idx.v.1, txt.v.1),
    .check_list(x, x.lines, idx.v.2, txt.v.2),
    .check_list(x, x.lines, idx.w.1, txt.w.1),
    .check_list(x, x.lines, idx.w.2, txt.w.2),
    .check_list(x, x.lines, idx.w.3, txt.w.3),
    .check_list(x, x.lines, idx.w.5, txt.w.5)
  )


  #----------------------------------------------------------------------------
  ### Check Data# columns for sightings data format
  # Marine mammal sightings (SKM)
  idx.skm.num <- .check_numeric(x, c("S", "K", "M"), paste0("Data", 3:7)) #3:9
  txt.skm.num <- paste(
    "At least one of the Data3-9 columns for S, K, and M events",
    "cannot be converted to a numeric"
  )

  # Auxillary info (A)
  idx.a.3 <- .check_character(x, "A", "Data3", c("N", "Y", "n", "y", NA))
  txt.a.3 <- "Photos (Data3 of A events) is not one of N, Y, n, y, or NA"

  idx.a.4 <- .check_character(x, "A", "Data4", c("N", "Y", "n", "y", NA))
  txt.a.4 <- "Birds (Data4 of A events) is not one of N, Y, n, y, or NA"

  # Resights (s and k)
  idx.res.num <- .check_numeric(x, c("s", "k"), paste0("Data", 2:5))
  txt.res.num <- paste(
    "At least one of the Data2-5 columns for s and k events",
    "cannot be converted to a numeric"
  )

  # Turtle
  idx.t.num <- .check_numeric(x, "t", paste0("Data", c(3:5, 7)))
  txt.t.num <- paste(
    "At least one of the Data3-5/Data7 columns for t events",
    "cannot be converted to a numeric"
  )

  idx.t.6 <- .check_character(x, "t", "Data6", c("F", "J", "N", "R", NA))
  txt.t.6 <- "Assocaited JFR (Data6 of t events) is not one of F, J, N, R, or NA"

  # Fishing boat
  idx.f.num <- .check_numeric(x, "F", paste0("Data", 2:4))
  txt.f.num <- paste(
    "At least one of the Data2-4 columns for F events",
    "cannot be converted to a numeric"
  )

  # Numeric events (1-8)
  idx.num.num <- .check_numeric(x, 1:8, paste0("Data", 2:8))
  txt.num.num <- paste(
    "At least one of the Data2-8 columns for 1-8 events",
    "cannot be converted to a numeric"
  )

  idx.num.9 <- .check_character(x, 1:8, "Data9", c(NA))
  txt.num.9 <- "The data9 column for 1-8 events is not NA (blank)"


  # Add to error.out
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.skm.num, txt.skm.num),
    .check_list(x, x.lines, idx.res.num, txt.res.num),
    .check_list(x, x.lines, idx.t.num, txt.t.num),
    .check_list(x, x.lines, idx.f.num, txt.f.num),
    .check_list(x, x.lines, idx.num.num, txt.num.num),
    .check_list(x, x.lines, idx.a.3, txt.a.3),
    .check_list(x, x.lines, idx.a.4, txt.a.4),
    .check_list(x, x.lines, idx.t.6, txt.t.6),
    .check_list(x, x.lines, idx.num.9, txt.num.9)
  )


  #----------------------------------------------------------------------------
  # Remove first line and return
  if (nrow(error.out) == 1) {
    to.return <- data.frame(
      File = NA, LineNum = NA, Idx = NA, ID = NA,
      Description = "No errors found",
      stringsAsFactors = FALSE
    )
  } else {
    to.return <- error.out[-1, ]
  }
  row.names(to.return) <- seq_len(nrow(to.return))

  if (!is.null(file.out)) write.csv(to.return, file = file.out)

  to.return
}
