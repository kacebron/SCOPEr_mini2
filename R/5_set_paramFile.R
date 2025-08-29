#' Modify set_parameter_filenames.csv
#'
#' This function generates the content for `set_parameter_filenames.csv`
#' and allows you to switch between different input data CSV files
#' used by SCOPE. By default, the parameter file points to
#' `"input_data_latin_hypercube.csv"`, but you can change it to
#' `"input_data_default.csv"` or any other input file that follows
#' the expected structure.
#'
#' @param input Character. Name of the input data CSV file to be used by SCOPE.
#' Default is `"input_data_latin_hypercube.csv"`.
#' You can also specify `"input_data_default.csv"` to run individual simulations
#' or time series experiments.
#'
#' @return A character vector representing the rows of `set_parameter_filenames.csv`.
#' Writing to file is not done inside the function, allowing users to
#' manage file outputs explicitly (e.g. with [writeLines()] or [utils::write.table()]).
#'
#' @examples
#' # Default: points to latin hypercube input
#' set_paramFile()
#'
#' # Switch to default input data file
#' set_paramFile(input = "input_data_default.csv")
#'
#' # Write to file (outside the function)
#' # lines <- set_paramFile("input_data_default.csv")
#' # writeLines(lines, "./SCOPE/set_parameter_filenames.csv", useBytes = TRUE)
#'
#' @seealso [set_options()], [set_filenames()], [input_data_default()]
#' @export
set_paramFile <- function(input = "input_data_latin_hypercube.csv") {
  # Hardcoded template
  template <- c(
    "setoptions.csv",
    "filenames.csv",
    "input_data_latin_hypercube.csv"
  )

  # Replace the 3rd line with user input
  template[3] <- input

  return(template)
}
