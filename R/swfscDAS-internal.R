# Internal, helper functions for swfscDAS
#   Internals all begin with "."

###############################################################################
# Helper functions for das_process
.das_process_num <- function(init.val, das.df, col.name, event.curr, event.na) {
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )

  toreturn
}

.das_process_chr <- function(init.val, das.df, col.name, event.curr, event.na) {
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )

  toreturn
}

###############################################################################
# Helper functions for das_check
.check_numeric <- function(z, event.code, z.col) {
  # z: das_dfr object
  # event.code: character; event code(s) by which to filter z
  # z.col: Column(s) which to check; must be one of the Data# columns
  ### Output: indices of z that cannot be converted to a numeric

  stopifnot(
    inherits(z, "das_dfr"),
    z.col %in% paste0("Data", 1:9)
  )

  z$idx <- seq_len(nrow(z))
  z.out <- c()

  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]

      z1.na <- is.na(z.vec)
      z2.na <- is.na(suppressWarnings(as.numeric(z.vec)))
      stopifnot(all(which(z1.na) %in% which(z2.na)))

      z.out <- c(z.out, z.curr$idx[z2.na != z1.na])
    }
  }

  sort(unique(z.out))
}


.check_character <- function(z, event.code, z.col, vals.accepted) {
  # z: das_dfr object
  # event.code: character; event code(s) by which to filter z
  # z.col: Column(s) which to check
  # vals.accepted: character; accepted (expected) value(s)
  ### Output: indices of z where z.col is not one of vals.accepted

  stopifnot(
    inherits(z, "das_dfr"),
    z.col %in% paste0("Data", 1:9)
  )

  z$idx <- seq_len(nrow(z))
  z.out <- c()

  for (i in event.code) {
    z.curr <- z[z$Event == i, ]
    z.vec <- z.curr[[z.col]]

    z.out <- c(z.out, z.curr$idx[!(z.vec %in% vals.accepted)])
  }

  sort(unique(z.out))
}


.check_list <- function(z1, z2, z3, z4) {
  # z1: x
  # z2: x.lines
  # z3: idx.
  # z4: txt.
  ### Output: list formatted to be added to error.out

  stopifnot(inherits(z1, "das_dfr"))
  list(z1$file_das[z3], z1$line_num[z3], z3, z2[z3], rep(z4, length(z3)))
}


###############################################################################
# Helper functions for airdas_effort_segdata

### Extract unique (and sorted) characters from a string
# stackoverflow.com/questions/31814548
fn_uniqchars <- function(x) sort(unique(strsplit(x, "")[[1]]))


### Keep running sum of data (conditions) multiplied by distance ratio
# Requires that names of cond.list elements are the same as
#   the column names in curr.df
fn_aggr_conditions <- function(data.list, curr.df, idx, dist.perc) {
  stopifnot(
    all(names(data.list) %in% names(curr.df)),
    idx <= nrow(curr.df)#,
    # dist.perc >= 0
  )

  if (is.na(dist.perc)) {
    lapply(data.list, function(i) NA)

  } else if (dist.perc != 0) {
    tmp <- lapply(names(data.list), function(k, dist.perc) {
      z <- data.list[[k]]
      if (inherits(z, c("numeric", "integer"))) {
        z + (dist.perc * curr.df[[k]][idx])
      } else if (inherits(z, "character")) {
        paste(fn_uniqchars(paste0(z, curr.df[[k]][idx])), collapse = "")
      } else if (inherits(z, "logical")) {
        browser()
      } else {
        print("class not recognized")
        browser()
      }
    }, dist.perc = dist.perc)

    names(tmp) <- names(data.list)
    tmp

  } else {
    data.list
  }
}


###############################################################################
# Functions for doing < / > / <= / >= comparisons with floating points
.less <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i < y) & !isTRUE(all.equal(i, y))}, as.logical(1))
  # (x < y) & !isTRUE(all.equal(x, y))
}

.greater <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i > y) & !isTRUE(all.equal(i, y))}, as.logical(1))
  # (x > y) & !isTRUE(all.equal(x, y))
}

.less_equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i < y) | isTRUE(all.equal(i, y))}, as.logical(1))
  # (x < y) | isTRUE(all.equal(x, y))
}

.greater_equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i > y) | isTRUE(all.equal(i, y))}, as.logical(1))
  # (x > y) | isTRUE(all.equal(x, y))
}

.equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) isTRUE(all.equal(i, y)), as.logical(1))
  # isTRUE(all.equal(x, y))
}


###############################################################################
# as.numeric with suppressed warnings
.sup_num <- function(x) suppressWarnings(as.numeric(x))


# Return indices of values made NA when coercing to numeric
.numeric_na <- function(x) {
  x.na <- which(is.na(x))
  x.num.na <- which(is.na(suppressWarnings(as.numeric(x))))

  x.num.na[!(x.num.na %in% x.na)]
}


###############################################################################
# Great circle distance function from segchopr

# FUNCTION to calculate the great circle distance (in km) between two lat/lons
# From EAB and KAF
.fn.grcirclkm <- function(lat1, lon1, lat2, lon2) {
  R <- pi/180      #angle in radians = angle in degrees * R
  D <- 180/pi      #angle in degrees = angle in radains * D
  dist <- 0

  NAcheck <- sum(is.na(c(lat1,lon1,lat2,lon2)))
  if (NAcheck == 0) {             #only continue if no NA positions
    if ((lat1 != lat2) | (lon1 != lon2))  {
      dlat1 <- lat1 * R              # convert to radian values:
      dlng1 <- lon1 * R
      dlat2 <- lat2 * R
      dlng2 <- lon2 * R
      las <- sin(dlat1) * sin(dlat2);   # compute distance
      lac <- cos(dlat1) * cos(dlat2) * cos(dlng1 - dlng2)
      laf <- las + lac
      if (laf < -1) {
        laf <- -1
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)))
      } else if (laf < 1) {
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)));
      } else {
        stop('laf value out of bounds')
      }
      dist <- (dacos * D * 60) * 1.852           #calculate distance in km
    }
  }

  dist
}

###############################################################################
