### code to prepare `das_sample` dataset goes here
# Read in real DAS file and 'jigger' data for sample file

###############################################################################
library(dplyr)
library(gdata)
library(lubridate)
library(stringr)
library(swfscDAS)


###############################################################################
# Browse through ALLDAS for section with most event types
x.all <- das_read("../DAS_files/AllDas.das")
x.all <- x.all[-753324, ]

window.size <- 200
window.start <- seq(1, nrow(x.all) - window.size, by = 50)

# system.time({ #~15s for window size of 300
tmp <- list()
for (i in window.start) {
  x.curr <- x.all[i:(i + window.size), ]
  tmp <- c(tmp, list(sort(unique(x.curr$Event))))
  rm(x.curr)
}; rm(i)
# })

table(sapply(tmp, length))
tmp.good <- which(sapply(tmp, length) >= 24)
x.sel <- lapply(window.start[tmp.good], function(i) {
  x.all[i:(i + window.size), ]
})
table(x.all$Event)
lapply(x.sel, function(j) table(j$Event))


###############################################################################
#------------------------------------------------------------------------------
# # Get 'original' data that will be jiggered
x.orig <- x.sel[[4]][29:193, ]
# saveRDS(x.orig, file = "data-raw/x_orig.rds")
# x.orig <- readRDS("data-raw/x_orig.rds")

rm(list = ls()[!(ls() %in% "x.orig")])
source("data-raw/das_sample_funcs.R")

#------------------------------------------------------------------------------
x <- head(x.orig, 1) %>%
  # Add initial B line
  mutate(Event = "B", event_num = event_num - 1,
         Data1 = "1611", Data2 = "C", Data3 = "7", Data4 = "Y") %>%
  bind_rows(x.orig) %>%
  # Adjust dates and lat/lons
  mutate(DateTime = DateTime + days(round(runif(1, min = 10, max = 20) * 365, 0)),
         Lat = Lat + runif(1, min = 20, max = 30),
         Lon = Lon - runif(1, min = 10, max = 20)) %>%
  # 'Reset' file
  mutate(file_das = "das_sample.das")


#------------------------------------------------------------------------------
# 'Randomize' data reported in DAS file
# TODO: Change:
#   DONE Cruise number in B events
#   DONE Specific comments
#   Sightings: Sight no (S, K, A), species, groupsize
#     Make sure resighting info matches
#   Turtle sighings: species, number
#   DONE Add F event?
#   DONE Remove Q events

# Items not changed: TO REVIEW WITH JIM
#   P events, as well as sighting observer codes
#   V and W events
#   N events, but Data (esp Data1, ship course) is going to not match with 'random' positions..


# Add F event, remove Q events, and set cruise number
# stopifnot(x$Event[50] == "*", x$Event[136] == "N")
x <- x %>%
  add_row(Event = "F", EffortDot = FALSE, Lat = x[136, "Lat"], Lon = x[136, "Lon"],
          DateTime = x[136, "DateTime"],
          Data1 = "126", Data2 = "000", Data3 = "2.7", Data4 = "0.5",
          file_das = "das_sample.das",
          .after = 136) %>%
  add_row(Event = "t", EffortDot = TRUE, Lat = x[50, "Lat"], Lon = x[50, "Lon"],
          DateTime = x[50, "DateTime"],
          Data1 = "001", Data2 = "DC", Data3 = "180", Data4 = "0.2",
          Data5 = "1", Data6 = "J", Data7 = "12.2", Data8 = "A", Data9 = "N",
          file_das = "das_sample.das",
          .after = 50) %>%
  filter(Event != "Q") %>%
  mutate(Data1 = ifelse(Event == "B", 1000, Data1))

# Remove extra Data9 bit from sighting
# TODO: Check with Jim about this..?
x$Data9[x$Event == "S" & x$Data1 == "081"] <- "2.0"

# Adjust species code and groupsizes; make on sighitng multi-species
event.A <- which(x$Event == "A")
event.1 <- which(x$Event == "1")
event.num <- which(x$Event %in% 1:7)

x$Data5[event.A] <- c("075", "018", "079", "079", "096")
x$Data6[event.A] <- c(NA, "016", NA, NA, NA)
x$Data5[which(x$Event == "?")] <- "074"

x$Data2[event.num] <- as.character(as.numeric(x$Data2[event.num]) + rpois(length(event.num), 1.5))
x$Data3[event.num] <- as.character(as.numeric(x$Data2[event.num]) + rpois(length(event.num), 1))
x$Data4[event.num] <- as.character(as.numeric(x$Data2[event.num]) - rpois(length(event.num), 1))
x$Data4[event.num] <- ifelse(as.numeric(x$Data4[event.num]) < 1, 1, x$Data4[event.num])

x[event.1[2], paste0("Data", 1:6)] <- c("001", "20", "30", "10", "60", "40")

# Remove initials from comment
stopifnot(sum(x$Event == "C") == 2)
tmp <- x$Data9[which(x$Event == "C")[2]]
x$Data9[which(x$Event == "C")[2]] <- str_sub(tmp, end = nchar(tmp) - 8)
rm(tmp)



#------------------------------------------------------------------------------
# Final step: set event numbers, starting at 1 for each B, ...
event.num.which <- which(!(x$Event %in% c(1:9, "?")))
event1 <- sum(event.num.which < which(x$Event == "B")[2])
event.num <- c(seq_len(event1), 1:(length(event.num.which) - event1))
stopifnot(length(event.num) == length(event.num.which))
x$event_num[!(x$Event %in% c(1:9, "?"))] <- event.num
rm(event.num, event.num.which, event1)

# ... and set line numbers, post inserting/deleting rows
x$line_num <- seq_len(nrow(x))


#------------------------------------------------------------------------------
# Write DAS data to file
raw_das_fwf(x, file = "inst/das_sample.das", 47)

# usethis::use_data("das_sample")


###############################################################################
# # Testing
# x <- das_read("../DAS_files/STAR1630.das")
# raw_das_fwf(x, file = "data-raw/STAR1630_test.das", 130)
# x2 <- das_read("data-raw/STAR1630_test.das")
#
# all.equal(x, x2)
# # Data column differences are due to original files not having extra spaces
# #   for comment rows after the text ends

###############################################################################
