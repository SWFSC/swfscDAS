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
    inherits(z, "das_df"),
    z.col %in% paste0("Data", 1:9)
  )

  z.curr <- z[z$Event == event.code, ]
  z.vec <- z.curr[[z.col]]

  z1.na <- which(is.na(z.vec))
  z2.na <- which(is.na(suppressWarnings(as.numeric(z.vec))))

  z$line_num[z2.na[!(z2.na %in% z1.na)]]
}


.check_character <- function(z, event.code, z.col, vals.accepted) {
  # z: das_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check
  # vals.accepted: character; accepted (expected) values
  ### Output: indices of z where z.col is not one of vals.accepted

  stopifnot(
    inherits(z, "das_df"),
    z.col %in% paste0("Data", 1:9)
  )

  z.curr <- z[z$Event == event.code, ]
  z.vec <- z.curr[[z.col]]

  z.curr$line_num[!(z.vec %in% vals.accepted)]
}



###############################################################################
