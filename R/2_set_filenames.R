#' Create and/or modify datasheet
#'
#' This function generates the datasheet from a fixed template
#' and lets you either:
#' - Automatically replace values using arguments (like set_filenames)
#' - Interactively edit rows in the second column
#'
#' @param interactive If TRUE, user can edit rows interactively
#' @param rows_to_edit Vector of row indices to edit (only if interactive = TRUE)
#' @inheritParams set_filenames
#' @return A data frame with two columns (col1, col2) representing the datasheet
#' @export
set_filenames <- function(
  simulation_name = "verification_run",
  dataset_dir = "for_verification",
  meteo_ec_csv = "input_data_latin_hypercube_ts",
  t = TRUE,
  Rin = TRUE,
  Rli = FALSE,
  p = FALSE,
  Ta = TRUE,
  u = FALSE,
  ea = FALSE,
  RH = TRUE,
  VPD = FALSE,
  tts = TRUE,
  tto = TRUE,
  psi = TRUE,
  Cab = TRUE,
  Cca = TRUE,
  Cdm = TRUE,
  Cw = TRUE,
  Cs = TRUE,
  Cant = TRUE,
  SMC = FALSE,
  BSMBrightness = FALSE,
  BSMlat = FALSE,
  BSMlon = FALSE,
  LAI = TRUE,
  hc = FALSE,
  LIDFa = TRUE,
  LIDFb = TRUE,
  z = FALSE,
  Ca = FALSE,
  Vcmax25 = TRUE,
  BallBerrySlope = TRUE,
  interactive = FALSE,
  rows_to_edit = NULL
) {
  # --- 1. Hardcoded template ---
  template <- "
Do_not_enter_additional_colums_of_text_in_this_sheet,
,
The_following_three_are_always_required,
Simulation_Name,verification_run
optipar_file,Optipar2021_ProspectPRO_CX.mat
atmos_file,FLEX-S3_std.atm
soil_file,soilnew.txt
,
mSCOPE_csv,
nly,3
,
atmos_names,
,
The_following_are_only_for_the_time_series_option!,
Dataset_dir,for_verification
meteo_ec_csv,input_data_latin_hypercube_ts.csv
t_file,
year_file,
Rin_file,
Rli_file,
p_file,
Ta_file,
ea_file,
u_file,
,
optional,
verification_dir,verificationdata
vegetation_retrieved_csv,
soil_file,
CO2_file,
z_file,
tts_file,
,
\"Variables below are COLUMN NAMES in time_series_files (meteo_ec_csv, vegetation_retrieved_csv)\",
t,t
Rin,Rin
Rli,
p,
Ta,Ta
u,
ea,
RH,RH
VPD,
tts,tts
tto,tto
psi,psi
,
Cab,Cab
Cca,Cca
Cdm,Cdm
Cw,Cw
Cs,Cs
Cant,Cant
,
SMC,
BSMBrightness,
BSMlat,
BSMlon,
,
LAI,LAI
hc,
LIDFa,LIDFa
LIDFb,LIDFb
,
z,
Ca,
,
Vcmax25,Vcmax25
BallBerrySlope,BallBerrySlope
,
,
optiona_leaf_inclination_distribution_file_with_3_headerlines._MUST_be_located_in_./input/leafangles/,
LIDF_file,
"
  
  # --- 2. Convert to data frame ---
  lines <- strsplit(trimws(template, which = "both", whitespace = "\n"), "\n")[[1]]
  df <- do.call(rbind, lapply(lines, function(x) {
    parts <- strsplit(x, ",", fixed = TRUE)[[1]]
    if (length(parts) == 1) parts <- c(parts, "")
    return(parts[1:2])
  }))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  colnames(df) <- c("col1", "col2")
  
  # --- 3. Automated replacements (like set_filenames) ---
  replacements <- list(
    Simulation_Name = simulation_name,
    Dataset_dir     = dataset_dir,
    meteo_ec_csv    = paste0(meteo_ec_csv, ".csv"),
    t               = ifelse(t, "t", ""),
    Rin             = ifelse(Rin, "Rin", ""),
    Rli             = ifelse(Rli, "Rli", ""),
    p               = ifelse(p, "p", ""),
    Ta              = ifelse(Ta, "Ta", ""),
    u               = ifelse(u, "u", ""),
    ea              = ifelse(ea, "ea", ""),
    RH              = ifelse(RH, "RH", ""),
    VPD             = ifelse(VPD, "VPD", ""),
    tts             = ifelse(tts, "tts", ""),
    tto             = ifelse(tto, "tto", ""),
    psi             = ifelse(psi, "psi", ""),
    Cab             = ifelse(Cab, "Cab", ""),
    Cca             = ifelse(Cca, "Cca", ""),
    Cdm             = ifelse(Cdm, "Cdm", ""),
    Cw              = ifelse(Cw, "Cw", ""),
    Cs              = ifelse(Cs, "Cs", ""),
    Cant            = ifelse(Cant, "Cant", ""),
    SMC             = ifelse(SMC, "SMC", ""),
    BSMBrightness   = ifelse(BSMBrightness, "BSMBrightness", ""),
    BSMlat          = ifelse(BSMlat, "BSMlat", ""),
    BSMlon          = ifelse(BSMlon, "BSMlon", ""),
    LAI             = ifelse(LAI, "LAI", ""),
    hc              = ifelse(hc, "hc", ""),
    LIDFa           = ifelse(LIDFa, "LIDFa", ""),
    LIDFb           = ifelse(LIDFb, "LIDFb", ""),
    z               = ifelse(z, "z", ""),
    Ca              = ifelse(Ca, "Ca", ""),
    Vcmax25         = ifelse(Vcmax25, "Vcmax25", ""),
    BallBerrySlope  = ifelse(BallBerrySlope, "BallBerrySlope", "")
  )
  
  for (nm in names(replacements)) {
    row_idx <- which(df$col1 == nm)
    if (length(row_idx) > 0) {
      df$col2[row_idx] <- replacements[[nm]]
    }
  }
  
  # --- 4. Optional interactive editing ---
  if (interactive) {
    if (is.null(rows_to_edit)) rows_to_edit <- seq_len(nrow(df))
    for (i in rows_to_edit) {
      old_val <- df$col2[i]
      cat(sprintf("Row %d | %s , %s\n", i, df$col1[i], old_val))
      new_val <- readline(prompt = "Enter new value (leave blank to keep): ")
      if (nzchar(new_val)) df$col2[i] <- new_val
    }
  }
  
  return(df)
}

# End