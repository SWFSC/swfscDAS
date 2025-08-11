#' Read DAS SpCodes file
#'
#' Read DAS SpCodes file
#'
#' @name das_spcodes
#'
#' @param file character; filename of .dat file from which to read
#'   species codes
#' @param skip integer; default is 3.
#'   Number of lines to skip when reading SpCodes file.
#'   See [readr::read_fwf()] for more details
#'
#' @details
#' Provide a standardized function to read a shipboard DAS SpCodes file.
#' Methods described in 'returns'
#'
#' @returns Data frame with four columns:
#' * SpCode: species code, columns 1 to 4
#' * Abbr: species abbreviation, columns 6 to 15
#' * SciName: species scientific name, columns 18 to 57
#' * CommonName: species common name, columns 58 until the end of the line
#'
#' @examples
#' sp.codes.file <- system.file("extdata", "SpCodes_sample.dat", package = "swfscDAS")
#' das_spcodes_read(sp.codes.file)
#'
#' @export
das_spcodes_read <- function(file, skip = 0) {
  read_fwf(
    file,
    # col_positions = fwf_positions(start = c(1, 10, 43), end = c(6, 42, NA)),
    col_positions = fwf_cols(
      SpCode = c(1, 4),
      Abbr = c(6, 15),
      SciName = c(18, 57),
      CommonName = c(58, NA_integer_)
    ),
    col_types = cols(.default = col_character()),
    trim_ws = TRUE,
    skip = skip,
    skip_empty_rows = FALSE
  )

  # sp.acc.df <- read_fwf(
  #   sp.codes,
  #   col_positions = fwf_positions(start = c(1, 6, 18, 58), end = c(4, 15, 57, NA)),
  #   col_types = cols(.default = col_character()),
  #   trim_ws = TRUE, skip = 0, skip_empty_rows = FALSE
  # )
}
