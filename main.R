noacsr::source_all_functions()

# Load packages
library(rofi) # installed using remotes::install_github("martingerdin/rofi")

# --- Organising raw data ------------------------------------------

# Import data
data <- import_data(test = TRUE)

# Merge data and remove duplicate variables/columns
merged.data <- merge_data(data, test = TRUE)
merged.data <- merged.data[, !duplicated(names(merged.data))]

# Add opportunities for improvement outcome and making it a factor
merged.data$ofi <- create_ofi(merged.data)
merged.data <- merged.data %>%
  dplyr::mutate(ofi = factor(ofi, levels = c("No","Yes"))) 

n_total_merged_data <- nrow(merged.data)


# --- Adding TBI to data --------------------------------------------------

AIS_columns <- grep("^AISCode_", names(merged.data), value = TRUE) #Identifying the columns/variables that have the AIS Code (String)

# Adding column to data that indicates whether the patient has TBI or not 
# And also what severity of TBI

merged.data <- merged.data %>%
  mutate(
    
    # Get max AIS severity among 140-codes
    TBI_sev_num = apply(select(., all_of(AIS_columns)), 1, Get_TBI_Severity),
    
    # 2) TBI = had any 140-code
    TBI = !is.na(TBI_sev_num),
    
    # Map to categories
    TBI_sev_cat = case_when(
      is.na(TBI_sev_num)   ~ NA_character_,
      TBI_sev_num %in% 1:2 ~ "Mild",
      TBI_sev_num == 3     ~ "Moderate",
      TBI_sev_num %in% 4:6 ~ "Severe",
      TRUE                 ~ NA_character_
    )
  )

# --- Filtering data --------------------------------------------------

# Making dataset with only patients that have TBI
TBI.only.data <- merged.data %>% filter(TBI)
n_TBI_only_data <- sum(TBI.only.data$TBI)

# Cant find DOA as a variable, but filter the data so that they are excluded 
# Also filter patients <18 years 

TBI.only.data <- TBI.only.data %>%
  Add_OnCall(datetime_col = "DateTime_ArrivalAtHospital") %>%
  Add_RTS()

Variables_wanted <- c(  
  
  # Categorical 
  "Gender",                                                                     # Sex
  "OnCall",                                                                     # On-call times -> hours outside of 08:00 - 17:00, or Saturday and Sunday
  "host_care_level",                                                            # Hospital care level  
  "pre_intubated","ed_intubated",                                               # Intubation status
  "inj_dominant",                                                               # Injury type
  "pt_asa_preinjury",                                                           # ASA class
  "host_vent_days_NotDone",                                                     # Mechanical ventilation
  "iva_dagar_n", "hosp_los_days",                                               # Length of stay 
  "TBI_sev_cat",                                                                # Severiy of TBI
  
  # Continuous 
  
  "pt_age_yrs",                                                                 # Age
  "ISS", "NISS",                                                                # ISS, NISS
  "ed_sbp_value",                                                               # SBP 
  "ed_rr_value",                                                                # RR
  "ed_gcs_sum",                                                                 # GCS
  "dt_ed_first_ct",                                                             # Time to first CT
  "RTS",                                                                        # RTS
  
  # Outcome 
  "ofi"
)

# Filtering my dataset to only the variables i want, including RTS and On call. How they are derived can be seen in Variable_filter
TBI.only.filtered <- TBI.only.data %>% select(all_of(Variables_wanted))

# Excluding individuals that are under 18
n_under18 <- sum(TBI.only.filtered$pt_age_yrs < 18) 

TBI.only.filtered <- TBI.only.filtered %>% 
  filter(pt_age_yrs >= 18)

# Removing patients with missing Ofi
n_no_ofi <- sum(is.na(TBI.only.filtered$ofi))

Analysis.sample <- Remove_missing_ofi(TBI.only.filtered)

# --- Descriptive table -------------------------------------------

## --- Lists of variables and labels ----------------------------------------

Variables_ordered <- c(
  # Demographics & baseline
  "Gender", "pt_age_yrs", "pt_asa_preinjury",
  # Type
  "inj_dominant",
  # ED physiology
  "ed_gcs_cat", "ed_sbp_cat", "ed_rr_cat",
  # Injury severity (summary scores)
  "ISS", "NISS", "RTS", "TBI_sev_cat",
  # Interventions & timeliness
  "Intubation", "dt_ed_first_ct", "host_vent_days_NotDone", 
  # System / care pathway
  "OnCall", "host_care_level",
  # Utilization / outcomes
  "iva_dagar_n", "hosp_los_days"
)

# Reusable labels list for descriptive tables
Labels_table1 <- list(
  Gender ~ "Gender",
  pt_age_yrs ~ "Age (years)",
  pt_asa_preinjury ~ "ASA class (preinjury)",
  inj_dominant ~ "Injury type",
  ed_gcs_cat ~ "GCS in ED",
  ed_sbp_cat ~ "SBP in ED",
  ed_rr_cat ~ "RR in ED",
  ISS ~ "Injury Severity Score (ISS)",
  NISS ~ "New Injury Severity Score (NISS)",
  RTS ~ "Revised Trauma Score (RTS)",
  TBI_sev_cat ~ "TBI severity",
  Intubation ~ "Intubation",
  dt_ed_first_ct ~ "Time to first CT (min)",
  host_vent_days_NotDone ~ "Mechanical ventilation",
  OnCall ~ "On-call times",
  host_care_level ~ "Hospital care level",
  iva_dagar_n ~ "ICU length of stay (days)",
  hosp_los_days ~ "Hospital length of stay (days)"
)

## --- Organising variables -------------------------------------------------

Descriptive.table1.data <- Analysis.sample  %>% Variable_Organiser()

## --- Producing descriptive table -----------------------------------------

Variables_table1 <- c("ofi", Variables_ordered)

Descriptive.table1 <- Descriptive.table1.data %>%
  select(all_of(c("ofi", Variables_table1))) %>%
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
  add_overall() %>%
  bold_labels() %>%  
  modify_caption("**Table 1. Sample characteristics and processes**")

Descriptive.table1 # First baseline descriptive table


# --- Simple regression analysis -----------------------------------

## --- Choosing predictors/variables and dropping NA values --------------------

exclude_vars <- c("NISS", "ed_gcs_cat", "ed_sbp_cat", "ed_rr_cat","TBI_sev_cat", "iva_dagar_n")

Complete.analysis.sample <- Analysis.sample %>%
  Variable_Organiser() %>%
  select(-all_of(exclude_vars))

Predictors <- Variables_ordered %>%
  intersect(names(Complete.analysis.sample)) %>%
  setdiff(exclude_vars)

n_eligible <- nrow(Complete.analysis.sample)

Complete.analysis.sample <- Complete.analysis.sample %>%  
  filter(if_all(all_of(Predictors), ~ !is.na(.)))

n_final <- nrow(Complete.analysis.sample)

n_missing_cc <- n_eligible - n_final

n_ofi <- sum(Complete.analysis.sample$ofi == "Yes")

## --- Model that produces simple regression results -------------

#For every variable (Predictors) in the Complete.analysis.sample data, we fit a separate
#logistic regression model with OFI (Yes/No) as the dependent variable.
#Each model is stored in the list `SR.model`.

SR.model <- map(Predictors, ~ glm(reformulate(.x, "ofi"),      
                                  data = Complete.analysis.sample,
                                  family = binomial)
)   

# Code below loops through both SR.model and predictors in parallel.

SR.results <- map2_dfr(
  SR.model,                #List of fitted glm() models (one per predictor)
  Predictors,              #The matching predictor names (same order as SR.model)
  ~ broom::tidy(.x,        
                conf.int = TRUE,            #Add 95% CIs
                exponentiate = TRUE) %>%    #Turn log-odds into ORs 
    filter(term != "(Intercept)") %>%       #Removing Intercept as the baseline OR is not relevant 
    mutate(variable = .y)                   #Add the predictor name (from .y) as a column
)

## --- Producing simple regression table --------------------------------------

### --- Labels used in the regression tables ----------------------------------

Labels_regression_tables <- list(
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
  # iva_dagar_n ~ "ICU length of stay (days)",
  hosp_los_days ~ "Hospital length of stay (days)"
)

### --- Constructing table ---------------------------------------------------

SR.Table1 <- tbl_uvregression(
  data = Complete.analysis.sample,
  y = ofi,
  method = glm,
  method.args = list(family=binomial),
  exponentiate = TRUE,
  include = all_of(Predictors),
  hide_n = TRUE,
  conf.level = 0.95,
  conf.int = TRUE,
  pvalue_fun = label_style_pvalue(digits = 3),
  label = Labels_regression_tables, 
  missing = "ifany",
  missing_text = "No data"
) %>%
  bold_labels() %>%
  modify_header(
    label = "**Characteristics/Predictors**",
    estimate = "***Unadjusted*** **OR**",
    conf.low = "**95% CI**",            
    p.value = "**p-value**" 
  ) %>% 
  modify_caption("**Table 2. Unadjusted logistic regression analyses of associations between patient level factors 
  and opportunities for improvement in TBI patients**")

SR.Table1 # Table describing unadjusted associations 

# --- Multivariable regression analysis ---------------------------------------

## --- Building the MV model --------------------------------------------------

# Producing code that joins all variable names in the predictor vector into a single string separated by +
# Adding the ofi at the start and then converting it into a formula to fit in the regression model glm(formula, data, family)

MV.formula <- as.formula(paste("ofi ~", paste(Predictors, collapse = " + ")))

# Building the multivariable model with MV.formula containing ofi and all predictors

MV.model <- glm(MV.formula, 
                data = Complete.analysis.sample,
                family = binomial)

# Tidying results as OR with 95% CI

MV.results <- broom::tidy(
  MV.model,
  exponentiate = TRUE,
  conf.int = TRUE,
) %>%
  filter(term != "(Intercept)")

## --- Producing multivariable table ------------------------------------------

# Producing table that displays the adjusted ORs for the predictors using tbl_regression() 
# Adding 95% CI and P value with 3 decimals to table
# Also changing names with modify_header and label 

MV.Table1 <- tbl_regression(
  x = MV.model,
  exponentiate = TRUE,
  conf.level = 0.95,
  conf.int = TRUE,
  include = all_of(Predictors),
  pvalue_fun = label_style_pvalue(digits = 3),
  label = Labels_regression_tables, # For real data use Labels_table1
  missing = "ifany",
  missing_text = "No data"
) %>%
  bold_labels() %>%
  modify_header(
    label = "**Characteristics/Predictors**",
    estimate = "***Adjusted*** **OR**",
    conf.low = "**95% CI**",            
    p.value = "**p-value**"
  ) %>% 
  modify_caption("**Table 3. Adjusted logistic regression analyses of associations between patient level factors 
  and opportunities for improvement in TBI patients**")

MV.Table1 # Table describing adjusted associations