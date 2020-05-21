# Internal helper functions for swfscDAS
# Some internal functions are exported to be used internally in swfscAirDAS;
#   these functions have the name 'swfscAirDAS-internals'
# Some function-specifc functions are in their functions's R file


###############################################################################
# Internals for use in effort-processing functions

#------------------------------------------------------------------------------
#' @name swfscAirDAS-internals
#' @param z ignore
#' @param z.distance.method ignore
#' @export
.dist_from_prev <- function(z, z.distance.method = c("greatcircle", "lawofcosines", "haversine", "vincenty")) {
  ### Inputs
  # z: data frame of class das_df
  # z.distance.method: distance.method from das_effort()

  ### Output: numeric of distance (km) to previous event; first element is NA

  # Input check
  z.distance.method <- match.arg(z.distance.method)

  # Check for NA Lat/Lon
  z.llna <- which(is.na(z$Lat) | is.na(z$Lon))
  if (length(z.llna) > 0)
    stop("Error in das_effort: Some unexpected events ",
         "have NA values in the Lat and/or Lon columns, ",
         "and thus the distance between each point cannot be determined. ",
         "Please remove or fix these events before running this function. ",
         "These events are in the following lines of the original file:\n",
         paste(z$line_num[z.llna], collapse = ", "))

  # Calculate distances
  if (identical(z.distance.method, "greatcircle")) {
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      swfscDAS::distance_greatcircle(y1, x1, y2, x2)
    },
    y1 = head(z$Lat, -1), x1 = head(z$Lon, -1), y2 = z$Lat[-1], x2 = z$Lon[-1],
    SIMPLIFY = TRUE)

  } else if (z.distance.method %in% c("lawofcosines", "haversine", "vincenty")) {
    dist.from.prev <- mapply(function(x1, y1, x2, y2) {
      swfscMisc::distance(y1, x1, y2, x2, units = "km", method = z.distance.method)
    },
    y1 = head(z$Lat, -1), x1 = head(z$Lon, -1), y2 = z$Lat[-1], x2 = z$Lon[-1],
    SIMPLIFY = TRUE)

  } else {
    stop("Error in distance calcualtion - ",
         "please pass an accepted argument to distance.method")
  }

  # Return distances, with inital NA since this are distances from previous point
  c(NA, dist.from.prev)
}


#------------------------------------------------------------------------------
# Check that conditions are valid for DAS data
.das_conditions_check <- function(x, x.method) {
  # x: character; condition name(s)
  # x.method: character; method argument from effort function
  # Output: x, or an error message

  stopifnot(x.method %in% c("condition", "equallength", "section"))

  conditions.acc <- c(
    "Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Glare", "Vis"
  )

  if (is.null(x)) {
    x <- if (x.method == "condition") {
      c("Bft", "SwellHght", "RainFog", "HorizSun", "VertSun", "Glare", "Vis")
    } else {
      c("Bft", "SwellHght", "HorizSun", "VertSun", "Glare", "Vis")
    }

  } else {
    if (!all(x %in% conditions.acc))
      stop("Please ensure that all 'conditions' are ",
           "one of the following accepted values:\n",
           paste(conditions.acc, collapse  = ", "))

    if (!("Bft" %in% x))  stop("The conditions argument must include 'Bft'")
  }

  x
}


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
