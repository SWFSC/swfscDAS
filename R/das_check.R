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
#' # file <- system.file("das_sample.das", package = "swfscDAS")
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' das_check(y)
#'
#' das_check("../DAS/AllDas.das")
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

  browser()

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
  #^ will be of length 0 if none, so nothign will be added to error.out

  error.out <- rbind(
    error.out,
    list(x.lines[err.eff.which],
         rep("Effort dot does not match B/R to E effort", length(err.eff.which)))
  )

  #----------------------------------------------------------------------------
  ### Check that columns added in das_process are as expected
  # table(x.proc$Cruise, useNA = "always")
  # table(x.proc$Mode, useNA = "always")
  # table(x.proc$EffType, useNA = "always")
  # table(x.proc$Course, useNA = "always")
  # table(x.proc$Bft, useNA = "always")
  # table(x.proc$SwellHght, useNA = "always")
  # table(x.proc$RainFog, useNA = "always")
  # table(x.proc$HorizSun, useNA = "always")
  # table(x.proc$VertSun, useNA = "always")
  # table(x.proc$Glare, useNA = "always")
  # table(x.proc$Vis, useNA = "always")

  #----------------------------------------------------------------------------
  # Check that value type of values in Data# columns are as expected

  #----------------------------------------------------------------------------
  tail(error.out, -1)
}
