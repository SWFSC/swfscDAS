# Functions used in data-raw/das_sample

#------------------------------------------------------------------------------
chr_z <- function(i, j = 2) sprintf(paste0("%0", j, "d"), i)


#------------------------------------------------------------------------------
str_pad_data <- function(i, j1 = 5) {
  ifelse(toupper(x$Event) == "C", i, str_pad(i, width = j1, side = "left"))
}


#------------------------------------------------------------------------------
# Adapted from sp::dd2dms
dd2dms_df <- function (dd, NS = FALSE) {
  sdd <- sign(dd)
  # WS <- ifelse(sdd < 0, TRUE, FALSE)
  dd <- abs(dd)
  deg <- as(floor(dd), "integer")
  dd <- (dd - deg) * 60
  mins <- as(floor(dd), "integer")
  sec <- (dd - mins) * 60
  tst <- abs(sec - 60) > sqrt(.Machine$double.eps)
  sec <- ifelse(tst, sec, 0)
  mins <- ifelse(tst, mins, mins + 1)
  tst <- mins < 60
  mins <- ifelse(tst, mins, 0)
  deg <- ifelse(tst, deg, deg + 1)
  # dms <- new("DMS", WS = WS, deg = deg, min = mins, sec = sec,
  #            NS = NS)
  # tst <- validObject(dms)
  # if (is.logical(tst) & tst)
  #   return(dms)
  # else stop(tst)
  # dms

  deg.char <- dplyr::case_when(
    sdd < 0 & NS ~ "S",
    sdd > 0 & NS ~ "N",
    sdd > 0 & !NS ~ "E",
    sdd < 0 & !NS ~ "W",
  )

  data.frame(
    deg_char = deg.char, deg = deg, min = mins, sec = sec,
    stringsAsFactors = FALSE
  )
}


#------------------------------------------------------------------------------
raw_das_fwf <- function(x, file, data9len = 100) {
  ### Inputs:
  # x: data.frame; output of das_read()
  # file: character; file path to whcih to write fixed-width file
  # data9len: numeric; width of 'data8' data in output text file;
  #   final element of 'width' argument of gdata::write.fwf

  ### Ouput: Writes fwf to path specified by 'file'

  if (data9len < 5) warning("data9len < 5")

  ### Process output of das_read
  na.paste <- c("NA", "NANA", "NANANA")
  x.proc <- x %>%
    mutate(EffortDot = ifelse(EffortDot, ".", " "),
           tm_hms = paste0(chr_z(hour(DateTime)), chr_z(minute(DateTime)),
                           chr_z(second(DateTime))),
           da_mdy = paste0(chr_z(month(DateTime)), chr_z(day(DateTime)),
                           substr(chr_z(year(DateTime)), 3, 4)),
           tm_hms = ifelse(tm_hms %in% na.paste, "", tm_hms),
           da_mdy = ifelse(da_mdy %in% na.paste, "", da_mdy))

  x.lat <- dd2dms_df(x$Lat, NS = TRUE)
  x.lon <- dd2dms_df(x$Lon, NS = FALSE)

  x.df <- data.frame(
    x.proc$event_num, x.proc$Event, x.proc$EffortDot,
    x.proc$tm_hms, " ", x.proc$da_mdy, " ",
    Lat1 = x.lat$deg_char, Lat2 = chr_z(x.lat$deg, 2), ":",
    Lat3 = sprintf("%05.2f", x.lat$min + x.lat$sec/60), " ",
    Lon1 = x.lon$deg_char, Lon2 = chr_z(x.lon$deg, 3), ":",
    Lon3 = sprintf("%05.2f", x.lon$min + x.lon$sec/60),
    str_pad_data(x$Data1), str_pad_data(x$Data2), str_pad_data(x$Data3),
    str_pad_data(x$Data4), str_pad_data(x$Data5), str_pad_data(x$Data6),
    str_pad_data(x$Data7), str_pad_data(x$Data8), str_pad_data(x$Data9),
    stringsAsFactors = FALSE
  )
  # For debugging, as necessary
  names(x.df) <- c(
    "event_num", "Event", "EffortDot", "tm_hms", "blank1", "da_mdy", "blank2",
    "Lat1", "Lat2", "c1", "Lat3", "blank3", "Lon1", "Lon2", "c1", "Lon3",
    paste0("Data", 1:9)
  )
  which.nona <- which(names(x.df) %in% c("Event", paste0("Data", 1:9)))
  x.df[is.na(x.df$event_num), -which.nona] <- NA

  fwf.width <- c(3, 1, 1, 6, 1, 6, 1,
                 1, 2, 1, 5, 1,
                 1, 3, 1, 5,
                 rep(5, 8), data9len)
  stopifnot(length(fwf.width) == ncol(x.df))

  ### Write to fwf
  gdata::write.fwf(
    x.df, file = file, na = "", sep = "", colnames = FALSE,
    justify = "left", width = fwf.width
  )
}

### Testing raw_das_fwf
# x <- das_read("../DAS_files/STAR1630.das")
# raw_das_fwf(x, file = "data-raw/STAR1630_test.das", 130)
# x2 <- das_read("data-raw/STAR1630_test.das")
#
# all.equal(x, x2)
# # Data column differences are due to original files not having extra spaces
# #   for comment rows after the text ends
#------------------------------------------------------------------------------
