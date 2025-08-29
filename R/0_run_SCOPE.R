#' Runs SCOPE.m in R
#'
#' This function runs SCOPE.m in R language.
#'
#' @param SCOPE_path filepath where SCOPE.m is located
#'
#' @importFrom matlabr run_matlab_script
#'
#' @return The function accepts a string parameter, path, that represents the directory path where SCOPE.m is located. If the path is not specified, the function prompts the user to choose the file location interactively. Once the path is determined, the function saves it to an R environment variable named SCOPE_PATH, which can be used in subsequent function calls to avoid re-locating SCOPE.m.
#'
#' @return Additionally, the function creates an R session hook that removes the SCOPE_PATH variable when the R session is terminated, ensuring that the path is not persisted across different sessions.
#'
#' @return This function can be called with a directory path as an argument, or without arguments to prompt the user to select the file interactively. Once the path is determined, the function saves it to the SCOPE_PATH variable in the global environment. The on.exit() hook ensures that the SCOPE_PATH variable is removed when the R session is terminated.
#'
#' @export

run_scope <- function(SCOPE_path = NULL) {

  # Ensure SCOPE_PATH is removed at the end of the R session
  if (!exists(".scope_exit_hook_set", envir = .GlobalEnv)) {
    reg.finalizer(.GlobalEnv, function(e) {
      if (exists("SCOPE_path", envir = .GlobalEnv)) rm("SCOPE_path", envir = .GlobalEnv)
    }, onexit = TRUE)
    assign(".scope_exit_hook_set", TRUE, envir = .GlobalEnv)
  }

  # Check if a path was provided
  if (!is.null(SCOPE_path)) {
    if (!file.exists(SCOPE_path)) {
      stop("The provided SCOPE_path does not exist: ", SCOPE_path)
    }
    message("Using provided path for SCOPE.m")
  } else if (exists("SCOPE_path", envir = .GlobalEnv)) {
    SCOPE_path <- get("SCOPE_path", envir = .GlobalEnv)
    if (!file.exists(SCOPE_path)) {
      message("Previously set SCOPE_path does not exist, please select the file.")
      SCOPE_path <- file.choose()
      assign("SCOPE_path", SCOPE_path, envir = .GlobalEnv)
    } else {
      message("File path for SCOPE.m has already been set!")
    }
  } else {
    # Prompt the user to select the SCOPE.m file path
    current_dir <- getwd()
    message(sprintf("Please locate SCOPE.m in your file directory from %s", current_dir))
    SCOPE_path <- file.choose()
    assign("SCOPE_path", SCOPE_path, envir = .GlobalEnv)
  }

  # Run the SCOPE.m script
  matlabr::run_matlab_script(SCOPE_path,
                             verbose = TRUE,
                             desktop = FALSE,
                             splash = FALSE,
                             display = FALSE,
                             wait = TRUE,
                             single_thread = FALSE
  )
}

# End
