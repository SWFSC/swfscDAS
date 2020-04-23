# Internal helper functions for swfscDAS
#   All functions begin with "."
# Some internal functions are exported to be used internally in swfscAirDAS;
#   these functions are in 'swfscDAS-internal-AirDAS.R'


###############################################################################
# Helper functions for das_check

# Check that specified values are numeric
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


# Check that specified values are a certain character
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
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]

      z.out <- c(z.out, z.curr$idx[!(z.vec %in% vals.accepted)])
    }
  }

  sort(unique(z.out))
}


# Check that specified values are NA
.check_isna <- function(z, event.code, z.col) {
  # z: airdas_dfr or airdas_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that is NA

  stopifnot(
    inherits(z, "das_df") | inherits(z, "das_dfr"),
    z.col %in% paste0("Data", 1:9), # | (identical(event.code, "1") & z.col %in% c("DateTime", "Lat", "Lon")),
    "idx" %in% names(z)
  )

  z.out <- c()
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]

      z.out <- c(z.out, z.curr$idx[!is.na(z.vec)])
    }
  }

  sort(unique(z.out))
}


# Format info for output
.check_list <- function(z1, z2, z3, z4) {
  # z1: x.proc
  # z2: x.lines
  # z3: idx.
  # z4: txt.
  ### Output: list formatted to be added to error.out

  stopifnot(inherits(z1, "das_df"))
  z1.rows <- which(z1$idx %in% z3)
  list(z1$file_das[z1.rows], z1$line_num[z1.rows], z1$Cruise[z1.rows],
       z2[z3], rep(z4, length(z3)))
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
