noacsr::source_all_functions()

# --- Loading packages ---------------------------------------------------------
library(rofi) # installed using remotes::install_github("martingerdin/rofi")
library(gtsummary)
library(broom)
library(tidyverse)
library(stringr)
library(DiagrammeR)
library(rsvg)
library(flextable)

# --- Organising raw data -----------------------------------------------------

## ---- Import & merge raw data -----------------------------------------------
data <- import_data()

# Merge data and remove duplicate variables/columns
merged.data <- merge_data(data) 
merged.data <- merged.data[, !duplicated(names(merged.data))]

## ---- OFI outcome -----------------------------------------------------------
merged.data$ofi <- create_ofi(merged.data)

merged.data <- merged.data %>%
  mutate(ofi = factor(ofi, levels = c("No","Yes"))) 

n_total_merged_data <- nrow(merged.data)

## --- Adding TBI to data --------------------------------------------------

# Identifying the columns/variables that have the ICD Code (String)
ICD_columns <- grep("^ICD_", names(merged.data), value = TRUE) 

# Adding column to data that indicates whether the patient has TBI or not
merged.data <- merged.data %>%
  mutate(
    TBI = apply(select(., all_of(ICD_columns)), 1, Get_TBI_ICD)
  )

# Define the ICD-10 head injury blocks S00–S09
head_blocks <- sprintf("S0%d", 0:9)                  # "S00" "S01" ... "S09"
indicator_names <- paste0("has_", head_blocks)       # "has_S00" ... "has_S09"

# Create the 10 binary indicators (one column per head injury block)
merged.data <- merged.data %>%
  mutate(
    !!!setNames(
      lapply(head_blocks, function(b) {
        
        # Apply row-wise: for each patient, check if any ICD code starts with block b
        apply(select(., all_of(ICD_columns)), 1, Has_ICD_Block, block = b)
        
      }),
      indicator_names
    )
  )

# #Identifying the columns/variables that have the AIS Code (String)
# AIS_columns <- grep("^AISCode_", names(merged.data), value = TRUE) 
# 
# # Adding column to data that indicates whether the patient has TBI or not 
# # And also what severity of TBI
# merged.data <- merged.data %>%
#   mutate(
#     
#     # Get max AIS severity among 1-codes
#     TBI_sev_num = apply(select(., all_of(AIS_columns)), 1, Get_TBI_Severity),
#     
#     # 2) TBI = had any 1-code
#     TBI = !is.na(TBI_sev_num),
#     
#     # Map to categories
#     TBI_sev_cat = case_when(
#       is.na(TBI_sev_num)   ~ NA_character_,
#       TBI_sev_num %in% 1:2 ~ "Mild",
#       TBI_sev_num == 3     ~ "Moderate",
#       TBI_sev_num %in% 4:6 ~ "Severe",
#       TRUE                 ~ NA_character_
#     )
#   )

### --- Adding S06 subcodes (S06.0–S06.9) -------------------------------------

# Define S06 subcodes
s06_subcodes <- paste0("S06.", 0:9)              # "S06.0" ... "S06.9"
s06_indicator_names <- paste0("has_", s06_subcodes)  # "has_S06.0" ...

# Create binary indicators for each S06 subcode
merged.data <- merged.data %>%
  mutate(
    !!!setNames(
      lapply(s06_subcodes, function(sc) {
        
        # Apply row-wise: for each patient, check if any ICD code starts with subcode sc
        apply(select(., all_of(ICD_columns)), 1, Has_S06_Subcode, subcode = sc)
        
      }),
      s06_indicator_names
    )
  )

# --- Filtering data ----------------------------------------------------------

## ---- Restrict to TBI patients & derive RTS / On-call -----------------------

# Making dataset with only patients with TBI
TBI.only.data <- merged.data %>% 
  filter(TBI)
n_TBI_only_data <- sum(TBI.only.data$TBI)

# Adding On-call hours and RTS as variables 
TBI.only.data <- TBI.only.data %>%
  Add_OnCall(datetime_col = "DateTime_ArrivalAtHospital") %>%
  Add_RTS()

## ---- Variables we bring into the TBI dataset -------------------------------

Variables_wanted <- c(  
  
  # Categorical 
  "Gender",                                                                     # Sex
  "OnCall",                                                                     # On-call times -> hours outside of 08:00 - 17:00, or Saturday and Sunday
  "host_care_level",                                                            # Hospital care level  
  "pre_intubated","ed_intubated",                                               # Intubation status
  "inj_dominant",                                                               # Injury type
  "pt_asa_preinjury",                                                           # ASA class
  "host_vent_days_NotDone",                                                     # Mechanical ventilation
  # "TBI_sev_cat",                                                                # Severiy of TBI
  
  paste0("has_S0", 0:9),
  paste0("has_S06.", 0:9),
  
  # Continuous 
  "pt_age_yrs",                                                                 # Age
  "ISS", "NISS",                                                                # ISS, NISS
  "ed_sbp_value",                                                               # SBP 
  "ed_rr_value",                                                                # RR
  "ed_gcs_sum",                                                                 # GCS
  "dt_ed_first_ct",                                                             # Time to first CT
  "RTS",                                                                        # RTS
  "iva_dagar_n", "hosp_los_days",                                               # Length of stay 
  
  # Outcome 
  "ofi"
)

# Filtering my dataset to only the variables i want, including RTS and On call. 
# How they are derived can be seen in Variable_filter
TBI.only.filtered <- TBI.only.data %>% select(all_of(Variables_wanted))

##---- Age restriction & missing OFI -----------------------------------------

# Excluding individuals that are under 18
n_under18 <- sum(TBI.only.filtered$pt_age_yrs < 18) 

TBI.only.filtered <- TBI.only.filtered %>% 
  filter(pt_age_yrs >= 18)

# Removing patients with missing Ofi
n_no_ofi <- sum(is.na(TBI.only.filtered$ofi))

Analysis.sample <- Remove_missing_ofi(TBI.only.filtered)

## ---- Organise variables & define predictors --------------------------------

Variables_ordered <- c( 
  # Demographics & baseline
  "Gender", "pt_age_yrs", "pt_asa_preinjury",
  # Type
  paste0("has_S0", 0:9),
  "inj_dominant",
  # ED physiology
  "ed_gcs_cat", "ed_sbp_cat", "ed_rr_cat",
  # Injury severity (summary scores)
  "ISS", "NISS", "RTS", 
  
  # "TBI_sev_cat",
  
  # Interventions & timeliness
  "Intubation", "dt_ed_first_ct", "host_vent_days_NotDone", 
  # System / care pathway
  "OnCall", "host_care_level",
  # Utilization / outcomes
  "iva_dagar_n", "hosp_los_days"
)

## --- Choosing predictors/variables and dropping NA values --------------------

# Exclude variables due to collinearity (NISS, TBI_sev_cat, ED categories). "TBI_sev_cat",
# or because they overlap conceptually with other included variables (iva_dagar_n)

exclude_vars <- c("NISS", "ed_gcs_cat", "ed_sbp_cat", "ed_rr_cat", 
                  "ed_sbp_value", "ed_rr_value", "ed_gcs_sum", "iva_dagar_n")

# Define S0x variables (to keep in Table 1 but exclude from regressions)
S0_vars <- paste0("has_S0", 0:9)


Complete.analysis.sample <- Analysis.sample %>%
  Variable_Organiser() %>%
  select(-all_of(exclude_vars))

# Predictors used in regression models
# Predictors for regression exclude BOTH exclude_vars and S0_vars
Predictors <- Variables_ordered %>%
  intersect(names(Complete.analysis.sample)) %>%
  setdiff(c(exclude_vars, S0_vars))

n_eligible <- nrow(Complete.analysis.sample)

# Complete-case analysis for chosen predictors
Complete.analysis.sample <- Complete.analysis.sample %>%  
  filter(if_all(all_of(Predictors), ~ !is.na(.)))

n_final <- nrow(Complete.analysis.sample)
n_missing_cc <- n_eligible - n_final
n_ofi <- sum(Complete.analysis.sample$ofi == "Yes")

# --- Descriptive table -------------------------------------------------------

## --- Labels used in the tables ----------------------------------------------

# Labels that are used in BOTH Table 1 and regressions
Labels_common <- list(
  Gender ~ "Gender",
  pt_age_yrs ~ "Age (years)",
  pt_asa_preinjury ~ "ASA class (preinjury)",
  inj_dominant ~ "Injury type",
  ISS ~ "Injury Severity Score (ISS)",
  RTS ~ "Revised Trauma Score (RTS)",
  Intubation ~ "Intubation",
  dt_ed_first_ct ~ "Time to first CT (min)",
  host_vent_days_NotDone ~ "Mechanical ventilation",
  OnCall ~ "On-call times",
  host_care_level ~ "Hospital care level",
  hosp_los_days ~ "Hospital length of stay (days)"
)

# Labels ONLY for Table 1 (the S0x blocks)
Labels_S0 <- list(
  has_S00 ~ "S00 - Superficial injury of head",
  has_S01 ~ "S01 - Open wound of head",
  has_S02 ~ "S02 - Fracture of skull and facial bones",
  has_S03 ~ "S03 - Dislocation/sprain/strain (head joints/ligaments)",
  has_S04 ~ "S04 - Injury of cranial nerves",
  has_S05 ~ "S05 - Injury of eye and orbit",
  has_S06 ~ "S06 - Intracranial injury",
  has_S07 ~ "S07 - Crushing injury of head",
  has_S08 ~ "S08 - Traumatic amputation of part of head",
  has_S09 ~ "S09 - Other and unspecified injuries of head"
)

Labels_S06sub <- list(
  has_S06.0 ~ "S06.0 - Concussion",
  has_S06.1 ~ "S06.1 - Traumatic cerebral oedema",
  has_S06.2 ~ "S06.2 - Diffuse brain injury",
  has_S06.3 ~ "S06.3 - Focal brain injury",
  has_S06.4 ~ "S06.4 - Epidural haemorrhage",
  has_S06.5 ~ "S06.5 - Traumatic subdural haemorrhage",
  has_S06.6 ~ "S06.6 - Traumatic subarachnoid haemorrhage",
  has_S06.7 ~ "S06.7 - Intracranial injury with prolonged coma",
  has_S06.8 ~ "S06.8 - Other intracranial injuries",
  has_S06.9 ~ "S06.9 - Intracranial injury, unspecified"
)

# Final label lists
Labels_table1 <- c(Labels_common, Labels_S0)  # used in Table 1
Labels_reg    <- Labels_common               # used in SR + MV (NO S0 labels)

## --- Producing descriptive table (Table1) ------------------------------------

# Table 1 includes S0_vars even though regressions exclude them
Variables_table1 <- c("ofi", Predictors, S0_vars)
Variables_table1 <- intersect(Variables_table1, names(Complete.analysis.sample))

Descriptive.table1 <- Complete.analysis.sample %>%
  select(all_of(Variables_table1)) %>%
  tbl_summary(
    by = ofi,
    label = Labels_table1,
    missing = "ifany",
    missing_text = "No data",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25}–{p75})"
    ),
    digits = list(RTS~1) #Rounding RTS to 1 decimal 
  )%>%
  add_p() %>%
  add_overall() %>%
  bold_labels() %>%  
  modify_caption("**Table 1. Sample characteristics and processes**")

Descriptive.table1 # First baseline descriptive table

## --- Producing descriptive S06.x table (Table2) -----------------------------

S06_vars <- paste0("has_S06.", 0:9)

Descriptive_S06 <- Complete.analysis.sample %>%
  filter(has_S06 == TRUE) %>%
  select(ofi, all_of(S06_vars)) %>%
  tbl_summary(
    by = ofi,
    label = Labels_S06sub,
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)",
    percent = "column"
  ) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 2. Distribution of S06 subcodes among patients with intracranial injury (S06)**")
  
Descriptive_S06

# --- Simple regression analysis ----------------------------------------------

## --- Model that produces unadjusted logistic regression results -------------

#For every variable (Predictors) in the Complete.analysis.sample data, we fit a separate
#logistic regression model with OFI (Yes/No) as the dependent variable.
#Each model is stored in the list `SR.model`

SR.model <- map(Predictors, ~ glm(reformulate(.x, "ofi"),      
                                  data = Complete.analysis.sample,
                                  family = binomial)
)   

# Code below loops through both SR.model and predictors in parallel.
SR.results <- map2_dfr(
  SR.model,                #List of fitted glm() models (one per predictor)
  Predictors,              #The matching predictor names (same order as SR.model)
  ~ tidy(.x,        
                conf.int = TRUE,            #Add 95% CIs
                exponentiate = TRUE) %>%    #Turn log-odds into ORs 
    filter(term != "(Intercept)") %>%       #Removing Intercept as the baseline OR is not relevant 
    mutate(variable = .y)                   #Add the predictor name (from .y) as a column
)

## --- Producing unadjusted logistic regression table -------------------------

SR.Table1 <- tbl_uvregression(
  data = Complete.analysis.sample,
  y = ofi,
  method = glm,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  include = all_of(Predictors),
  hide_n = TRUE,
  conf.int = TRUE,
  conf.level = 0.95,
  pvalue_fun = label_style_pvalue(digits = 3),
  label = Labels_reg, 
  missing = "ifany",
  missing_text = "No data"
) %>%
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        OR_CI = dplyr::if_else(
          !is.na(estimate) & !is.na(conf.low) & !is.na(conf.high),
          paste0(
            sprintf("%.2f", estimate), " (",
            sprintf("%.2f", conf.low), ", ",
            sprintf("%.2f", conf.high), ")"
          ),
          NA_character_  # blank for reference rows
        )
      ) %>%
      relocate(OR_CI, .after = label)
  ) %>%
  modify_column_hide(columns = c(estimate, conf.low, conf.high)) %>%
  modify_header(
    label   = "**Characteristics/Predictors**",
    OR_CI   = "**Unadjusted OR (95% CI)**",
    p.value = "**p-value**"
  ) %>%
  modify_caption(
    "**Table 2. Unadjusted logistic regression analyses of associations between patient level factors 
and opportunities for improvement in TBI patients**")

# SR.Table1 

# --- Multivariable logistic regression ---------------------------------------

## --- Building the MV model --------------------------------------------------

# Producing code that joins all variable names in the predictor vector into a single string separated by +
# Adding the ofi at the start and then converting it into a formula to fit in the regression model glm(formula, data, family)
MV.formula <- as.formula(paste("ofi ~", paste(Predictors, collapse = " + ")))

# Building the multivariable model with MV.formula containing ofi and all predictors
MV.model <- glm(
  MV.formula, 
  data = Complete.analysis.sample,
  family = binomial)

# Tidying results as OR with 95% CI
MV.results <- tidy(
  MV.model,
  exponentiate = TRUE,
  conf.int = TRUE,
) %>%
  filter(term != "(Intercept)")

## --- Producing multivariable table ------------------------------------------

# Producing table that displays the adjusted ORs for the predictors 
MV.Table1 <- tbl_regression(
  x = MV.model,
  exponentiate = TRUE,
  conf.level = 0.95,
  conf.int = TRUE,
  include = all_of(Predictors),
  pvalue_fun = label_style_pvalue(digits = 3),
  label = Labels_reg,
  missing = "ifany",
  missing_text = "No data"
) %>%
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        OR_CI = if_else(
          !is.na(estimate) & !is.na(conf.low) & !is.na(conf.high),
          paste0(
            sprintf("%.2f", estimate), " (",
            sprintf("%.2f", conf.low), ", ",
            sprintf("%.2f", conf.high), ")"
          ),
          NA_character_  # blank for reference rows
        )
      ) %>%
      relocate(OR_CI, .after = label)
  ) %>%
  modify_column_hide(columns = c(estimate, conf.low, conf.high)) %>%
  modify_header(
    label   = "**Characteristics/Predictors**",
    OR_CI   = "***Adjusted*** **OR (95% CI)**",
    p.value = "**p-value**"
  ) %>% 
  modify_caption(
    "**Table 3. Adjusted logistic regression analyses of associations between patient level factors 
and opportunities for improvement in TBI patients**"
  )
# MV.Table1 # Table describing adjusted associations

# ---- Combined unadjusted & adjusted table (Table3) --------------------------

Merged.Table <- tbl_merge(
  tbls = list(SR.Table1, MV.Table1),
  tab_spanner = c("**Unadjusted**", "**Adjusted**")
) %>%
  modify_caption(
    "**Table 3. Unadjusted and adjusted logistic regression analyses of associations between patient-level factors and opportunities for improvement in TBI patients**"
  )
Merged.Table
