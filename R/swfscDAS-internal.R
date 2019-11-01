# Internal, helper functions for swfscDAS

###############################################################################
# Helper functions for das_process
das_process_help_num <- function(init.val, das.df, col.name, event.curr, event.na) {
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )

  toreturn
}

das_process_help_chr <- function(init.val, das.df, col.name, event.curr, event.na) {
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )

  toreturn
}

# das_process_help_last <- function(i, obj.curr, obj.last) {
#   if (is.na(obj.curr[i])) RainFog[i] <- LastRF else LastRF <- RainFog[i]
# }

###############################################################################
