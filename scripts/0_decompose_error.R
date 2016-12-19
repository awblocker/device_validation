library(tidyverse)
library(stringr)
library(readr)

# Input parameters
kDataPath <- "data/AshleyLab_device_validation_study_data_matrix_ALL.csv"
kSubjectVars <- c(
  "ID",
  "Sex",
  "Age",
  "Height",
  "Weight",
  "BMI",
  "Skin",
  "Fitzpatrick",
  "Wrist",
  "V02max"
)
kVariablePattern <- "^(\\w+)_([a-z]+)(\\d*)_(\\w+)$"
kVariablePartNames <-
  c("Variable", "Measure", "Activity", "Replicate", "Device")

# Modeling specifications
kDevicesOfInterest <- c("Fitbit", "Apple")
kModels <- list(
  OLS = ~lm(Value ~ ID * Activity, data = .),
  LME = ~lmer(Value ~ (1 + Activity) | ID, data = .)
)
kErrorSummaries <- list(
  OLS_Mean_Abs_Residual = ~mean(abs(resid(OLS))),
  LME_Mean_Abs_Residual = ~mean(abs(resid(LME))),
  OLS_RMSE = ~summary(OLS)[["sigma"]],
  LME_RMSE = ~attr(VarCorr(LME), "sc")
)

# Output parameters
kResultsPath <- "output/0_decompose_error.rds"

parse_variable_names <- function(var_names, pattern, part_names) {
  parts <- str_match(var_names, pattern)
  colnames(parts) <- part_names
  return(as_data_frame(parts))
}

main <- function() {
  wide <- read_tsv(kDataPath)

  # We want a tidy DF with device, activity, and replicate as columns with one
  # row per subject per each of those.
  long <- gather_(wide, "Variable", "Value",
                  setdiff(names(wide), kSubjectVars)) %>%
    mutate(Value = as.numeric(Value),
           ID = as.character(ID))
  conditions <- long %>%
    group_by_(~ID, ~Variable) %>%
    do_(~parse_variable_names(.$Variable,
                              kVariablePattern,
                              kVariablePartNames)) %>%
    ungroup()
  measures <- long %>%
    left_join(conditions) %>%
    select_(~-Variable)

  # Run basic linear models and random effect models.
  models <- measures %>%
    filter_(~Device %in% kDevicesOfInterest) %>%
    group_by_(~Measure, ~Device) %>%
    do_(.dots = kModels) %>%
    ungroup()

  # Compute statistics from models and residuals. Note that the latter are
  # biased towards zero whereas the former are corrected for degrees of freedom.
  # Also note that the LME results require stronger assumptions but, assuming
  # normality, they may provide a more accurate estimate of residual error
  # properties.
  error_stats <- models %>%
    rowwise() %>%
    mutate_(.dots = kErrorSummaries) %>%
    ungroup() %>%
    select_(~-OLS, ~-LME)

  # Save results.
  results <- list(measures = measures,
                  models = models,
                  error_stats = error_stats)
  saveRDS(results, kResultsPath)
}

if (sys.nframe() == 0) {
  main()
}
