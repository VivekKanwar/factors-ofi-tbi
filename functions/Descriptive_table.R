
 Descriptive.table1.data <- Analysis.sample  %>% Variable_Organiser()


Variables_table1 <- c("Intubation", "ed_gcs_cat",  "pre_gcs_cat", #Adding the new label/variables. Can continue to add new variables/labels  
                      setdiff(Variables_wanted, c("pre_intubated", "ed_intubated"))) #Removing pre_intub and ed_intub from my variables in the table

Descriptive.table1 <- Descriptive.table1.data %>%
  select(all_of(c("ofi", Variables_table1))) %>%
           tbl_summary(
             by = ofi,
             label = list(
               
               #Categorical
               Intubation ~ "Intubation",
               OnCall ~ "On call times",
               host_care_level ~ "Hospital care level",
               inj_mechanism  ~ "Mechanism of injury",
               pt_asa_preinjury ~ "ASA class (preinjury)",
               hosp_vent_days ~ "Hospital ventilation duration (days)",
               host_vent_days_NotDone ~ "Mechanical ventilation not performed",
               iva_dagar_n ~ "ICU length of stay (days)",
               hosp_los_days ~ "Hospital length of stay (days)",
               ed_gcs_cat ~ "GCS in ED",
               pre_gcs_cat ~ "GCS prehospital",
               
               #Continuous
               pt_age_yrs ~ "Age (years)",
               ISS ~ "Injury Severity Score (ISS)",
               NISS ~ "New Injury Severity Score (NISS)",
               pre_sbp_value ~ "Prehospital systolic BP (mmHg)",
               ed_sbp_value ~ "ED systolic BP (mmHg)",
               pre_rr_value ~ "Prehospital respiratory rate (/min)",
               ed_rr_value ~ "ED respiratory rate (/min)",
               dt_ed_first_ct ~ "Time to first CT (min)",
               RTS ~ "Revised Trauma Score (RTS)"
               
             ),
             missing = "ifany",
             missing_text = "No data",
             statistic = list(
               all_categorical() ~ "{n} ({p}%)",
               all_continuous() ~ "{median} ({p25}–{p75})"
             ),
             digits = list(RTS~1,pre_gcs_sum~0) #Rounding RTS to 1 decimal 
           )%>%
           add_overall() %>%
           bold_labels() %>%  
           modify_caption("**Table 1. Sample characteristics and processes**")
