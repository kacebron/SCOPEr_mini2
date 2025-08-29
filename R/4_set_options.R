#' Generate or modify setoptions.csv content
#'
#' This function creates the template of `setoptions.csv` from scratch
#' and updates the numeric flags according to the user's arguments.
#' It returns the data frame so writing to disk can be handled outside
#' the function (e.g., with [utils::write.table()] or [readr::write_csv()]).
#'
#' @param verify Logical. Enable verification step. Default: `TRUE`.
#' @param simulation Integer. Simulation flag (typically `0` or `1`). Default: `0`.
#' @param lite Logical. Use "lite" option. Default: `TRUE`.
#' @param calc_fluor Logical. Enable fluorescence calculations. Default: `TRUE`.
#' @param calc_planck Logical. Enable Planck law calculations. Default: `TRUE`.
#' @param calc_xanthophyllabs Logical. Enable xanthophyll absorption calculations. Default: `TRUE`.
#' @param soilspectrum Logical. Use soil spectrum. Default: `FALSE`.
#' @param fluorescence_model Logical. Enable fluorescence model. Default: `FALSE`.
#' @param applTcorr Logical. Apply temperature correction. Default: `TRUE`.
#' @param saveCSV Logical. Save results as CSV. Default: `TRUE`.
#' @param mSCOPE Logical. Use mSCOPE module. Default: `FALSE`.
#' @param calc_directional Logical. Enable directional calculations. Default: `FALSE`.
#' @param calc_vert_profiles Logical. Enable vertical profiles. Default: `FALSE`.
#' @param soil_heat_method Integer. Soil heat calculation method. Default: `2`.
#' @param calc_rss_rbs Logical. Enable RSS/RBS calculations. Default: `FALSE`.
#' @param MoninObukhov Logical. Enable Monin–Obukhov similarity theory. Default: `TRUE`.
#' @param save_spectral Logical. Save spectral outputs. Default: `TRUE`.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{flag}{Numeric flags (0, 1, or 2) indicating module settings.}
#'   \item{module}{Character vector of module names.}
#' }
#'
#' @examples
#' # Create default setoptions table
#' df <- set_options()
#' head(df)
#'
#' # Modify specific options
#' df <- set_options(verify = FALSE, simulation = 1, soilspectrum = TRUE)
#'
#' # Write to CSV
#' # utils::write.table(df, file = "setoptions.csv", sep = ",",
#' #                    col.names = FALSE, row.names = FALSE, quote = FALSE)
#'
#' @export
set_options <- function(
    verify = TRUE,
    simulation = 0,
    lite = TRUE,
    calc_fluor = TRUE,
    calc_planck = TRUE,
    calc_xanthophyllabs = TRUE,
    soilspectrum = FALSE,
    fluorescence_model = FALSE,
    applTcorr = TRUE,
    saveCSV = TRUE,
    mSCOPE = FALSE,
    calc_directional = FALSE,
    calc_vert_profiles = FALSE,
    soil_heat_method = 2,
    calc_rss_rbs = FALSE,
    MoninObukhov = TRUE,
    save_spectral = TRUE
) {
  # Hardcoded template
  template <- data.frame(
    flag = c(1,1,1,1,0,0,1,1,1,0,0,0,0,2,0,1,1),
    module = c("lite","calc_fluor","calc_planck","calc_xanthophyllabs",
               "soilspectrum","Fluorescence_model","applTcorr","verify",
               "saveCSV","mSCOPE","simulation","calc_directional",
               "calc_vert_profiles","soil_heat_method","calc_rss_rbs",
               "MoninObukhov","save_spectral"),
    stringsAsFactors = FALSE
  )

  # Helper to convert logicals to numeric flags
  logical_to_flag <- function(x) {
    if (is.logical(x)) as.numeric(x) else x
  }

  # Update the flag column according to arguments
  template$flag[template$module == "verify"]             <- logical_to_flag(verify)
  template$flag[template$module == "simulation"]         <- simulation
  template$flag[template$module == "lite"]               <- logical_to_flag(lite)
  template$flag[template$module == "calc_fluor"]         <- logical_to_flag(calc_fluor)
  template$flag[template$module == "calc_planck"]        <- logical_to_flag(calc_planck)
  template$flag[template$module == "calc_xanthophyllabs"]<- logical_to_flag(calc_xanthophyllabs)
  template$flag[template$module == "soilspectrum"]       <- logical_to_flag(soilspectrum)
  template$flag[template$module == "Fluorescence_model"] <- logical_to_flag(fluorescence_model)
  template$flag[template$module == "applTcorr"]          <- logical_to_flag(applTcorr)
  template$flag[template$module == "saveCSV"]            <- logical_to_flag(saveCSV)
  template$flag[template$module == "mSCOPE"]             <- logical_to_flag(mSCOPE)
  template$flag[template$module == "calc_directional"]   <- logical_to_flag(calc_directional)
  template$flag[template$module == "calc_vert_profiles"] <- logical_to_flag(calc_vert_profiles)
  template$flag[template$module == "soil_heat_method"]   <- soil_heat_method
  template$flag[template$module == "calc_rss_rbs"]       <- logical_to_flag(calc_rss_rbs)
  template$flag[template$module == "MoninObukhov"]       <- logical_to_flag(MoninObukhov)
  template$flag[template$module == "save_spectral"]      <- logical_to_flag(save_spectral)

  return(template)
}
