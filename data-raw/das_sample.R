### code to prepare `das_sample` dataset goes here
# Read in real DAS file and 'jigger' data for sample file

###############################################################################
library(dplyr)
library(gdata)
library(lubridate)
library(stringr)
library(swfscDAS)


###############################################################################
# Browse through STAR1630.das for section with most event types
x.all <- das_read("../DAS_files/STAR1630.das")

window.size <- 300
window.start <- seq(1, nrow(x.all) - window.size, by = 10)

tmp <- list()
for (i in window.start) {
  x.curr <- x.all[i:(i + window.size), ]
  tmp <- c(tmp, list(sort(unique(x.curr$Event))))
  rm(x.curr)
}; rm(i)

table(sapply(tmp, length))
tmp.good <- which(sapply(tmp, length) >= max(sapply(tmp, length)))
x.sel <- lapply(window.start[tmp.good], function(i) {
  x.all[i:(i + window.size), ]
})
table(x.all$Event)
lapply(x.sel, function(j) table(j$Event))


###############################################################################
#------------------------------------------------------------------------------
# Extract 'original' data that will be jiggered
x.orig <- x.sel[[2]]
rm(list = ls()[!(ls() %in% "x.orig")])

source("data-raw/das_sample_funcs.R")

#------------------------------------------------------------------------------
idx1 <- head(which(x.orig$Event == "B"), 1)
idx2 <- tail(which(x.orig$Event == "E"), 1)

x <- x.orig %>%
  # Slice from first B event to before last E event
  slice(idx1:idx2) %>%
  # Adjust dates and lat/lons
  mutate(DateTime = DateTime + days(round(runif(1, min = 5, max = 10) * 365, 0)),
         Lat = Lat + runif(1, min = 30, max = 40),
         Lon = Lon - runif(1, min = 50, max = 60))


#------------------------------------------------------------------------------
# 'Randomize' data reported in DAS file
# Change the following:
#   Cruise number in B events
#   Remove specific comments
#   Mammal sightings species and group size
#   Turtle sightings species and number

# Set cruise number and remove a specific comments
stopifnot(x$Event[which(grepl("j3", x$Data3))] == "C") #x[165, ]
x <- x %>%
  mutate(Data1 = ifelse(Event == "B", 1000, Data1)) %>%
  slice(-which(grepl("j3", x$Data3)))


# Jitter marine mammal species code
event.A <- which(x$Event == "A")
event.prob <- which(x$Event == "?")

x$Data5[event.A] <- c("018", "076", "037", "016", "013", "075", "018", "016")
x$Data6[event.A] <- c(NA, NA, NA, NA, "016", NA, "277", "277")
x$Data5[event.prob] <- "016"
x$Data6[event.prob] <- "016"


# Jitter group sizes
event.num <- which(x$Event %in% 1:7)

x$Data2[event.num] <- as.character(as.numeric(x$Data2[event.num]) + rpois(length(event.num), 1.5))
x$Data3[event.num] <- as.character(vapply(event.num, function(i) {
  max(as.numeric(x$Data2[i]), as.numeric(x$Data3[i]) + rpois(1, 4))
}, 1))
x$Data4[event.num] <- as.character(vapply(event.num, function(i) {
  min(as.numeric(x$Data2[i]), abs(as.numeric(x$Data3[i]) - rpois(1, 4)))
}, 1))

stopifnot(
  all(na.omit(as.numeric(x$Data2[event.num]) <= as.numeric(x$Data3[event.num]))),
  all(na.omit(as.numeric(x$Data2[event.num]) >= as.numeric(x$Data4[event.num])))
)


# Jitter turtle species and number
event.t <- which(x$Event == "t")
x$Data2[event.t] <- sample(c("LV", "DC"), length(event.t), replace = TRUE)
x$Data5[event.t] <- sample(c(1, 2), length(event.t), replace = TRUE)


#------------------------------------------------------------------------------
# Final steps: 'reset' file name, start event numbers at 1 for each B, ...
e.rownum <- which(!(x$Event %in% c(1:9, "?")))
e.rownum1 <- sum(e.rownum < which(x$Event == "B")[2])
e.vals <- c(seq_len(e.rownum1), 1:(length(e.rownum) - e.rownum1))

stopifnot(length(e.vals) == length(e.rownum))
x$EventNum[!(x$Event %in% c(1:9, "?"))] <- e.vals
rm(e.rownum, e.rownum1, e.vals)

# ... and set file name and line numbers, post inserting/deleting rows
x$file_das <- "das_sample.das"
x$line_num <- seq_len(nrow(x))


#------------------------------------------------------------------------------
# Write DAS data to file
raw_das_fwf(x, file = "inst/das_sample2.das", 10)

# usethis::use_data("das_sample")

###############################################################################
