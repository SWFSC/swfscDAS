library(gdata)
library(stringr)

# Make sample SpCodes.dat
spcodes <- str_pad(c("013", "016", "018", "037", "075", "076", "277", "DC", "LV"),
                   width = 4, side = "left")
spcodes.df <- data.frame(
  Code = spcodes,
  blank1 = NA,
  Abbr = paste0("Abbr", seq_along(spcodes)),
  blank2 = NA,
  SciName = paste0("SciName", seq_along(spcodes)),
  CommonName = paste0("CommonName", seq_along(spcodes))
)

gdata::write.fwf(
  spcodes.df, file = "inst/SpCodes_sample.dat", na = "", sep = "", colnames = FALSE,
  justify = "left", width = c(4, 1, 10, 2, 40, 15)
)


# Make sample Ships.dat
ships.df <- data.frame(
  Cruise = c(500, 1000), blank = NA, Ship = c("SHIP1", "SHIP2"),
  stringsAsFactors = FALSE
)

gdata::write.fwf(
  ships.df, file = "inst/Ship_sample.dat", na = "", sep = "", colnames = FALSE,
  justify = "left", width = c(4, 2, 5)
)
