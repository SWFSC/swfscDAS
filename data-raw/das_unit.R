# Create simple, bare-bones DAS file for unit testing

library(dplyr)
library(gdata)
library(lubridate)
library(stringr)
library(swfscDAS)
library(tibble)

source("data-raw/das_sample_funcs.R")

# Sample
samp.orig <- das_read(system.file("das_sample.das", package = "swfscDAS"))

# DAS file contents:
#   Event order: B, R, P, V, N, W, *, #, t, V, S, A, 1, 2, E, *, R, C, F, s, E, B, R, S, A, ?, 1, E

dt1 <- ymd_hms("2000-01-01 08:00:00")
dt2 <- ymd_hms("2000-01-02 08:00:00")

lat1 <- 0
lat2 <- 1
lon1 <- -110
lon2 <- -111

# cat(paste(names(samp.orig), collapse = "\", \""))
names.cols <- c(
  "Event", "EffortDot", "DateTime", "Lat", "Lon",
  "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9",
  "EventNum", "file_das", "line_num"
)


x <- data.frame(
  Event = "B", EffortDot = TRUE, DateTime = dt1, Lat = lat1, Lon = lon1,
  Data1 = "1", Data2 = "C", Data3 = "7", Data4 = "Y", Data5 = NA, Data6 = NA, Data7 = NA, Data8 = NA, Data9 = NA
) %>%
  add_row(Event = "R", EffortDot = TRUE, DateTime = dt1, Lat = lat1, Lon = lon1,
          Data1 = "1", Data2 = "C", Data3 = "7", Data4 = "Y", Data5 = NA, Data6 = NA, Data7 = NA, Data8 = NA, Data9 = NA) %>%
  mutate(EventNum = NA, file_das = "das_unit.das", line_num = seq_along(.))



y <- tribble(
  ~x1,   ~x2,              ~x3,         ~x4,         ~x5, ~d1, ~d2, ~d3, ~d4, ~d5, ~d6, ~d7, ~d8, ~d9,
  "B", TRUE,  dt1 + minutes(0), lat1 + 0.00, lon1 + 0.00,
  "R", TRUE,  dt1 + minutes(0), lat1 + 0.00,
  "P", TRUE,  dt1 + minutes(0), lat1 + 0.00,
  "V", TRUE,  dt1 + minutes(0), lat1 + 0.00,
  "N", TRUE,  dt1 + minutes(0), lat1 + 0.00,
  "W", TRUE,  dt1 + minutes(0), lat1 + 0.01,
  "*", TRUE,  dt1 + minutes(1), lat1 + 0.01,
  "#", TRUE,  dt1 + minutes(1), lat1 + 0.01,
  "t", TRUE,  dt1 + minutes(1), lat1 + 0.01,
  "V", TRUE,  dt1 + minutes(1), lat1 + 0.01,
  "S", TRUE,  dt1 + minutes(2), lat1 + 0.02,
  "A", TRUE,  dt1 + minutes(2), lat1 + 0.02,
  "1", TRUE,  dt1 + minutes(2), lat1 + 0.02,
  "2", TRUE,  dt1 + minutes(2), lat1 + 0.02,
  "E", FALSE, dt1 + minutes(3), lat1 + 0.03,
  "*", FALSE, dt1 + minutes(4), lat1 + 0.04,
  "R", TRUE,  dt1 + minutes(5), lat1 + 0.05,
  "C", TRUE,  dt1 + minutes(5), lat1 + 0.05,
  "F", TRUE,  dt1 + minutes(6), lat1 + 0.06,
  "s", TRUE,  dt1 + minutes(7), lat1 + 0.07,
  "E", FALSE, dt1 + minutes(8), lat1 + 0.08,
  "B", TRUE,  dt2 + minutes(0), lat2 + 0.00,
  "R", TRUE,  dt2 + minutes(0), lat2 + 0.00,
  "S", TRUE,  dt2 + minutes(1), lat2 + 0.01,
  "A", TRUE,  dt2 + minutes(1), lat2 + 0.01,
  "?", TRUE,  dt2 + minutes(1), lat2 + 0.01,
  "1", TRUE,  dt2 + minutes(1), lat2 + 0.01,
  "E", FALSE, dt2 + minutes(2), lat2 + 0.02,
)



