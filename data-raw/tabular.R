###############################################################################
# From https://r-pkgs.org/man.html
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste", c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'   ")))

  paste("#' \\tabular{", paste(col_align, collapse = ""), "}{\n#'   ", contents, "\n#' }\n", sep = "")
}

cat(tabular(mtcars[1:5, 1:5]))

###############################################################################
# das_sight

### columns in all outputs
# "The following sighting information columns are included in all return formats:"
sight.all.list <- list(
  c("\\emph{Sighting information}", "\\emph{Column name}", "\\emph{Notes}"),
  c("Sighting number", "SightNo", "Character"),
  c("Subgroup code", "Subgroup" , "Character"),
  c("Daily sighting number", "SightNoDaily", "See below"),
  c("Observer that made the sighting", "Obs", ""),
  c("Standard observer", "ObsStd", "Logical; \\code{TRUE} if Obs is one of ObsL, Rec or ObsR, and \\code{FALSE} otherwise"),
  c("Bearing to the sighting", "Bearing", "Numeric; degrees, expected range 0 to 360"),
  c("Number of reticle marks", "Reticle", "Numeric"),
  c("Distance (nautical miles)", "DistNm", "Numeric"),
  c("Sighting cue", "Cue", ""),
  c("Sighting method", "Method", ""),
  c("Photos of school?", "Photos", ""),
  c("Birds present with school?", "Birds", ""),
  c("Calibration school?", "CalibSchool", ""),
  c("Aerial photos taken?", "PhotosAerial", ""),
  c("Biopsy taken?", "Biopsy", ""),
  c("Probable sighting", "Prob", "Logical indicating if sighting has associated ? event; \\code{NA} for non-S/K/M/G events"),
  c("Number of species in sighting", "nSp", "\\code{NA} for non-S/K/M/G events"),
  c("Mixed species sighting", "Mixed", "Logical; \\code{TRUE} if nSp > 1"),
  c("Group size of school - best estimate", "GsSchoolBest", "See below"),
  c("Group size of school - high estimate", "GsSchoolHigh", "See below"),
  c("Group size of school - low estimate", "GsSchoolLow", "See below"),
  c("Course (true heading) of school at resight", "CourseSchool", "\\code{NA} for non-s/k/m events"),
  c("Presence of associated JFR", "TurtleJFR", "\\code{NA} for non-\"t\" events; JFR = jellyfish, floating debris, or red tide"),
  c("Estimated turtle maturity", "TurtleAge", "\\code{NA} for non-\"t\" events"),
  c("Perpendicular distance (km) to sighting", "PerpDistKm", "Calculated via \\code{(abs(sin(Bearing*pi/180) * DistNm) * 1.852)}")
)
cat(tabular(data.frame(t(as.data.frame(sight.all.list)))))


### columns in wide and complete output
# "Sighting information columns present in the "wide" and "complete" format outputs:"
sight.wc.list <- list(
  c("\\emph{Sighting information}", "\\emph{Column name}", "\\emph{Notes}"),
  c("Observer code - estimate", "ObsEstimate", "See below"),
  c("Species 1 code", "SpCode1", ""),
  c("Species 2 code", "SpCode2", ""),
  c("Species 3 code", "SpCode3", ""),
  c("Species 4 code", "SpCode4", ""),
  c("Species 1 probable code", "SpCodeProb1" , "Extracted from '?' event"),
  c("Species 2 probable code", "SpCodeProb2", "Extracted from '?' event"),
  c("Species 3 probable code", "SpCodeProb3", "Extracted from '?' event"),
  c("Species 4 probable code", "SpCodeProb4", "Extracted from '?' event"),
  c("Percentage of Sp 1 in school", "SpPerc1", ""),
  c("Percentage of Sp 2 in school", "SpPerc2", ""),
  c("Percentage of Sp 3 in school", "SpPerc3", ""),
  c("Percentage of Sp 4 in school", "SpPerc4", ""),
  c("Group size of species 1", "GsSpBest1", "Present in \"wide\" output only; see below"),
  c("Group size of species 2", "GsSpBest2", "Present in \"wide\" output only; see below"),
  c("Group size of species 3", "GsSpBest3", "Present in \"wide\" output only; see below"),
  c("Group size of species 4", "GsSpBest4", "Present in \"wide\" output only; see below"),
  c("Turtle species", "TurtleSp", "\\code{NA} for non-\"t\" events"),
  c("Turtle group size", "TurtleGs", "\\code{NA} for non-\"t\" events"),
  c("Was turtle captured?", "TurtleCapt", "\\code{NA} for non-\"t\" events"),
  c("Pinniped species", "PinnipedSp", "\\code{NA} for non-\"p\" events"),
  c("Pinniped group size", "PinnipedGs", "\\code{NA} for non-\"p\" events"),
  c("Boat or gear type", "BoatType", "\\code{NA} for non-\"F\" events"),
  c("Number of boats", "BoatGs", "\\code{NA} for non-\"F\" events")
)
cat(tabular(data.frame(t(as.data.frame(sight.wc.list)))))


### das_check table
check.list <- list(
  c("\\emph{Item}", "\\emph{Event}", "\\emph{Column}", "\\emph{Requirement}"),
  c("Cruise number", "B", "Data1", "Can be converted to a numeric value"),
  c("Mode", "B ", "Data2", "Must be one of C, P, c, p, or NA (blank)"),
  c("Echo sounder", "B", "Data4", "Must be one of Y, N, y, n, or NA (blank)"),
  c("Effort type", "R", "Data1", "Must be one of F, N, S, or NA (blank)"),
  c("ESW sides", "R", "Data2", "Effective strip width; must be one of F, H, or NA (blank)"),
  c("Course", "N", "Data1", "Can be converted to a numeric value"),
  c("Speed", "N", "Data2", "Can be converted to a numeric value"),
  c("Beaufort", "V", "Data1", "Must be a whole number between 0 and 9"),
  c("Swell height", "V", "Data2", "Can be converted to a numeric value"),
  c("Wind speed", "V", "Data5", "Can be converted to a numeric value"),
  c("Rain or fog", "W", "Data1", "Must be between 0 and 5 and either a whole number or have decimal value .5"),
  c("Horizontal sun", "W", "Data2", "Must be a whole number between 0 and 12"),
  c("Vertical sun", "W", "Data3", "Must be a whole number between 0 and 12"),
  c("Visibility", "W", "Data5", "Can be converted to a numeric value"),
  c("Sighting (mammal)", "S, K, M", "Data3-7", "Can be converted to a numeric value"),
  c("Sighting (mammal)", "G", "Data5-7", "Can be converted to a numeric value"),
  c("Sighting cue (mammal)", "S, K, M", "Data3", "Must be a whole number between 1 and 6"),
  c("Sighting method (mammal)", "S, K, M, G", "Data4", "Must be a whole number between 1 and 7"),
  c("Bearing (mammal)", "S, K, M, G", "Data5", "Must be a whole number between 0 and 360"),
  c("Photos ", "A", "Data3", "Must be one of N, Y, n, y, or NA (blank)"),
  c("Birds  ", "A", "Data4", "Must be one of N, Y, n, y, or NA (blank)"),
  c("Calibration school", "S, K, M", "Data10", "Must be one of N, Y, n, y, or NA (blank)"),
  c("Aerial photos taken", "S, K, M", "Data11", "Must be one of N, Y, n, y, or NA (blank)"),
  c("Biopsy taken", "S, K, M", "Data12", "Must be one of N, Y, n, y, or NA (blank)"),
  c("Species codes", "A", "Data5-8", "If a species codes file is provided, must be one of the provided codes"),
  c("Resight", "s, k", "Data2-5", "Can be converted to a numeric value"),
  c("Turtle species", "t", "Data2", "If a species codes file is provided, must be one of the provided codes"),
  c("Turtle sighting", "t", "Data3-5, 7", "Can be converted to a numeric value"),
  c("Turtle JFR", "t", "Data6", "Must be one of F, J, N, R, or NA (blank)"),
  c("Fishing vessel", "F", "Data2-4", "Can be converted to a numeric value"),
  c("Sighting info", "1-8", "Data2-8", "Can be converted to a numeric value"),
  c("Sighting info", "1-8", "Data9", "The Data9 column must be NA (blank) for events 1-8")
)
cat(tabular(data.frame(t(as.data.frame(check.list)))))
