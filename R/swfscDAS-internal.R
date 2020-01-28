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
  # z: das_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that cannot be converted to a numeric

  stopifnot(
    inherits(z, "das_dfr"),
    z.col %in% paste0("Data", 1:9)
  )

  z$idx <- seq_len(nrow(z))

  z.curr <- z[z$Event == event.code, ]
  z.vec <- z.curr[[z.col]]

  z1.na <- is.na(z.vec)
  z2.na <- is.na(suppressWarnings(as.numeric(z.vec)))
  stopifnot(all(which(z1.na) %in% which(z2.na)))

  z.curr$idx[z2.na != z1.na]
}


.check_character <- function(z, event.code, z.col, vals.accepted) {
  # z: das_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check
  # vals.accepted: character; accepted (expected) values
  ### Output: indices of z where z.col is not one of vals.accepted

  stopifnot(
    inherits(z, "das_dfr"),
    z.col %in% paste0("Data", 1:9)
  )

  z$idx <- seq_len(nrow(z))

  z.curr <- z[z$Event == event.code, ]
  z.vec <- z.curr[[z.col]]

  z.curr$idx[!(z.vec %in% vals.accepted)]
}


.check_numeric_sight <- function(z, event.code, z.col) {
  # z: das_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
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


.check_list <- function(z1, z2, z3, z4) {
  # z1: x.proc
  # z2: x.lines
  # z3: idx.
  # z4: txt.
  ### Output: list formatted to be added to error.out

  list(z1$line_num[z3], z2[z3], rep(z4, length(z3)))
}

###############################################################################
