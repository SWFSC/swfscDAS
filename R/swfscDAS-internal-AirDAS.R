#' Internal functions for swfscAirDAS
#'
#' These functions are exported only to be used internally by swfscAirDAS.
#' They implement functionality that is used when processing both
#' DAS and AirDAS data
#'
#' @name swfscAirDAS-internals
#' @param init.val ignore
#' @param das.df ignore
#' @param col.name ignore
#' @param event.curr ignore
#' @param event.na ignore
#' @export
.process_num <- function(init.val, das.df, col.name, event.curr, event.na) {
  # Helper function for _process function - extract numeric values
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )

  toreturn
}

#' @name swfscAirDAS-internals
#' @export
.process_chr <- function(init.val, das.df, col.name, event.curr, event.na) {
  # Helper function for _process function - extract character values
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )

  toreturn
}



#' @name swfscAirDAS-internals
#' @param data.list ignore
#' @param curr.df ignore
#' @param idx ignore
#' @param dist.perc ignore
#' @export
.fn_aggr_conditions <- function(data.list, curr.df, idx, dist.perc) {
  # Helper functions for _segdata function
  #   Keep running sum of data (conditions) multiplied by distance ratio
  #   Requires that names of cond.list elements are the same as
  #   the column names in curr.df

  stopifnot(
    all(names(data.list) %in% names(curr.df)),
    idx <= nrow(curr.df)
  )

  # Extract and sort unique characters from a string;
  #   stackoverflow.com/questions/31814548
  .fn_uniqchars <- function(x) sort(unique(strsplit(x, "")[[1]]))


  if (is.na(dist.perc)) {
    lapply(data.list, function(i) NA)

  } else if (dist.perc != 0) {
    tmp <- lapply(names(data.list), function(k, dist.perc) {
      z <- data.list[[k]]
      if (inherits(z, c("numeric", "integer"))) {
        z + (dist.perc * curr.df[[k]][idx])
      } else if (inherits(z, "character")) {
        paste(.fn_uniqchars(paste0(z, curr.df[[k]][idx])), collapse = "")
      } else if (inherits(z, "logical")) {
        stop(".fn_aggr_conditions error - cannot average logical data. ",
             "Please report this as an issue")
      } else {
        stop(".fn_aggr_conditions error - unrecognized data class. ",
             "Please report this as an issue")
      }
    }, dist.perc = dist.perc)

    names(tmp) <- names(data.list)
    tmp

  } else {
    data.list
  }
}


