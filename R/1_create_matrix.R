#' Create SCOPE time series input
#'
#' This function combines constant and variable parameters into the SCOPE input format.
#'
#' @param par_variable A data.frame with environmental variables. Must include a `Date` column (POSIXct or character).
#' @param par_constant A named list or data.frame of constant leaf/canopy traits.
#' @param tz Time zone for formatting (default: system timezone).
#' @return A data.frame ready for export to SCOPE format.
#' @export
#'
create_matrix <- function(par_variable, par_constant, tz = Sys.timezone()) {
  stopifnot("Date" %in% names(par_variable))

  # Format time column
  t <- strftime(par_variable$Date, format = "%Y%m%d%H%M", tz = tz)

  # Expand constants across rows
  par_constant_df <- as.data.frame(lapply(par_constant, rep, times = nrow(par_variable)))

  # Combine
  df <- cbind(t = t, par_constant_df, par_variable[setdiff(names(par_variable), "Date")])

  # Collapse list-columns if any
  list_cols <- sapply(df, is.list)
  if (any(list_cols)) {
    df[list_cols] <- lapply(df[list_cols], function(x) sapply(x, paste, collapse = ","))
  }

  return(df)
}

# End