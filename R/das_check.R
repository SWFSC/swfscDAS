#' Check DAS file
#'
#' Check that DAS file has accepted values
#'
#' @param file filename(s) of one or more DAS files
#' @param file.out filename to which to write the error log; default is NULL
#'
#' @importFrom dplyr left_join
#'
#' @details Precursor to DASCHECK. Checks that the following is true:
#'
#'   \itemize{
#'     \item Event codes are one of the following: #, *, ?, 1, 2, 3, 4, 5, 6, 7, 8,
#'       A, B, C, E, F, k, K, N, P, Q, R, s, S, t, V, W, g, p, X, Y, Z.
#'     \item
#'   }
#'   \tabular{lrr}{
#'     \emph{Item}  \tab \emph{Columns} \tab \emph{Format}\cr
#'     Event number \tab 1-3\cr
#'   }
#'
#' @return A list...
#'   If file.out is not NULL, then the error log is also written to a text file
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' das_check(y)
#'
#' @export
das_check <- function(file, file.out = NULL) {
  error.out <- data.frame(ID = NA, Description = NA)
  x <- das_read(file)
  x.proc <- das_process(x)
  x.all <- left_join(
    x, x.proc,
    by = c("Event", "DateTime", "Lat", "Lon", "Data1", "Data2", "Data3",
           "Data4", "Data5", "Data6", "Data7", "Data8", "Data9",
           "EventNum", "file_das", "line_num")
  )
  x.lines <- substr(readLines(file), 4, 39)
  stopifnot(nrow(x) == length(x.lines))


  #----------------------------------------------------------------------------
  ### Check event codes
  event.acc <- c("#", "*", "?", 1:8, "A", "B", "C", "E", "F", "k", "K", "N",
                 "P", "Q", "R", "s", "S", "t", "V", "W",
                 "g", "p", "X", "Y", "Z")
  if (!all(x$Event %in% event.acc))
    stop("The following lines in the DAS file (from line_num in output ",
         "DAS data frame) contain unexpected event codes:\n",
         paste(x$line_num[!(x$Event %in% event.acc)], collapse = ", "),
         "\nExpected event codes (case sensitive): ",
         paste(event.acc, collapse = ", "))


  #----------------------------------------------------------------------------
  ### Check that effort dot matches effort determined by B/R to E events
  err.eff.which <- which(
    x.all$OnEffort != x.all$EffortDot & !(x.all$Event %in% c("?", 1:8))
  )
  #^ will be of length 0 if none, so nothing will be added to error.out

  error.out <- rbind(
    error.out,
    list(x.lines[err.eff.which],
         rep("Effort dot does not match B/R to E effort", length(err.eff.which)))
  )


  #----------------------------------------------------------------------------
  ### Check that value type of values in Data# columns are as expected for
  ###   columns that are added in das_process

  # Variables are code named as "z".event code'.'data# column' for
  #   line numbers with weird info,
  #   and "txt".event code'.'data# column' for the txt to go in error.out

  # Cruise number
  z.b.1 <- .check_numeric(x.proc, "B", "Data1")
  txt.b.1 <- "Cruise number (Data1 of B events) cannot be converted to a numeric"

  # Mode
  z.b.2 <- .check_character(x.proc, "B", "Data2", c("C", "P", "c", "p", NA))
  txt.b.2 <- "Effort type (Data2 of B events) is not one of C, P, c, p, or NA"

  # Effort type
  z.r.1 <- .check_character(x.proc, "R", "Data1", c("F", "N", "S", NA))
  txt.r.1 <- "Effort type (Data1 of R events) is not one of F, N, S, or NA"

  # Course
  z.n.1 <- .check_numeric(x.proc, "N", "Data1")
  txt.n.1 <- "Course (Data1 of N events) cannot be converted to a numeric"

  # Beaufort
  z.v.1 <- .check_numeric(x.proc, "V", "Data1")
  txt.v.1 <- "Beaufort (Data1 of V events) cannot be converted to a numeric"

  # Swell Height
  z.v.2 <- .check_numeric(x.proc, "V", "Data2")
  txt.v.2 <- "Swell height (Data2 of V events) cannot be converted to a numeric"

  # RainFog
  z.w.1 <- .check_numeric(x.proc, "W", "Data1")
  txt.w.1 <- "Rain/fog (Data1 of W events) cannot be converted to a numeric"

  # Horizontal sun
  z.w.2 <- .check_numeric(x.proc, "W", "Data2")
  txt.w.2 <- "Horizontal sun (Data2 of W events) cannot be converted to a numeric"

  # Vertical sun
  z.w.3 <- .check_numeric(x.proc, "W", "Data3")
  txt.w.3 <- "Vertical sun (Data3 of W events) cannot be converted to a numeric"

  # Visibility
  z.w.5 <- .check_numeric(x.proc, "W", "Data5")
  txt.w.5 <- "Visibility (Data5 of W events) cannot be converted to a numeric"


  # Add text to error.out as needed and return
  error.out <- rbind(
    error.out,
    list(z.b.1, rep(txt.b.1, length(z.b.1))),
    list(z.b.2, rep(txt.b.2, length(z.b.2))),
    list(z.r.1, rep(txt.r.1, length(z.r.1))),
    list(z.n.1, rep(txt.n.1, length(z.n.1))),
    list(z.v.1, rep(txt.v.1, length(z.v.1))),
    list(z.v.2, rep(txt.v.2, length(z.v.2))),
    list(z.w.1, rep(txt.w.1, length(z.w.1))),
    list(z.w.2, rep(txt.w.2, length(z.w.2))),
    list(z.w.3, rep(txt.w.3, length(z.w.3))),
    list(z.w.5, rep(txt.w.5, length(z.w.5)))
  )


  #----------------------------------------------------------------------------
  # Check Data# columns for sightings


  #----------------------------------------------------------------------------
  to.return <- tail(error.out, -1)
  row.names(to.return) <- seq_len(nrow(to.return))

  if (!is.null(file.out)) write.csv(to.return, file = file.out)

  to.return
}
