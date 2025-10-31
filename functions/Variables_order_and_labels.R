
# Making vector with the variables i want but ordered in the way presented in the tables 

Variables_ordered <- c(
  # Demographics & baseline
  "Gender", "pt_age_yrs", "pt_asa_preinjury",
  # Mechanism
  "inj_mechanism",
  # Prehospital physiology
  "pre_gcs_cat", "pre_sbp_cat", "pre_rr_cat",
  # ED physiology
  "ed_gcs_cat", "ed_sbp_cat", "ed_rr_cat",
  # Injury severity (summary scores)
  "ISS", "NISS", "RTS", "TBI_sev_cat",
  # Interventions & timeliness
  "Intubation", "dt_ed_first_ct", "host_vent_days_NotDone", "hosp_vent_days",
  # System / care pathway
  "OnCall", "host_care_level",
  # Utilization / outcomes
  "iva_dagar_n", "hosp_los_days"
  
  #   All intubated so not needed and if i run the code it does not work
)

# Reusable labels list for tables
Labels_table1 <- list(
  Gender ~ "Gender",
  pt_age_yrs ~ "Age (years)",
  pt_asa_preinjury ~ "ASA class (preinjury)",
  inj_mechanism ~ "Mechanism of injury",
  pre_gcs_cat ~ "GCS prehospital",
  pre_sbp_cat ~ "SBP prehospital",
  pre_rr_cat ~ "RR prehospital",
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
  hosp_vent_days ~ "Hospital ventilation duration (days)",
  OnCall ~ "On-call times",
  host_care_level ~ "Hospital care level",
  iva_dagar_n ~ "ICU length of stay (days)",
  hosp_los_days ~ "Hospital length of stay (days)"
)

# Temporarily making list for the regression tables as they do not include iva_dagar_n
# For the real data use Labels_table1

Labels_regression_tables <- list(
  Gender ~ "Gender",
  pt_age_yrs ~ "Age (years)",
  pt_asa_preinjury ~ "ASA class (preinjury)",
  inj_mechanism ~ "Mechanism of injury",
  pre_gcs_cat ~ "GCS prehospital",
  pre_sbp_cat ~ "SBP prehospital",
  pre_rr_cat ~ "RR prehospital",
  ed_gcs_cat ~ "GCS in ED",
  ed_sbp_cat ~ "SBP in ED",
  ed_rr_cat ~ "RR in ED",
  ISS ~ "Injury Severity Score (ISS)",
  NISS ~ "New Injury Severity Score (NISS)",
  RTS ~ "Revised Trauma Score (RTS)",
  TBI_sev_cat ~ "TBI severity",
  Intubation ~ "Intubation",
  dt_ed_first_ct ~ "Time to first CT (min)",
  hosp_vent_days ~ "Hospital ventilation duration (days)",
  OnCall ~ "On-call times",
  host_care_level ~ "Hospital care level",
  iva_dagar_n ~ "ICU length of stay (days)",
  hosp_los_days ~ "Hospital length of stay (days)"
)

#   host_vent_days_NotDone ~ "Mechanical ventilation",

