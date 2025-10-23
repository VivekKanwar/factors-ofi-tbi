
Descriptive.table1.data <- Analysis.sample  %>% mutate(                         #continue adding re-labelings and categorise certain continuous variables
  Gender = factor(Gender, levels = c("M", "K"), labels = c("Male", "Female")),
  
  host_care_level = factor(host_care_level,
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("Emergency Deptartment", "General Ward", "Operating Theatre", "High Dependency Unit", "Critical Care Unit")
                          ),
  
  Intubation = case_when( #Hur ska jag tänka här angående till exemepel om pre intub är missing men ed inte är det, eller tvärtom. blir det missing?
    pre_intubated == 1 & ed_intubated != 1 ~ "Pre-hospital",                    #only prehospital intubated
    pre_intubated != 1 & ed_intubated == 1 ~ "Emergency department",            #only ED intubated
    pre_intubated == 2 & ed_intubated == 2 ~ "None",                            #neither
    pre_intubated == 1 & ed_intubated == 1 ~ "Pre-hospital",                    #both yes, count as prehospital
    TRUE ~ NA_character_                                                        #missing
  ),
  
  Intubation = factor(Intubation,
                      levels = c("None", "Pre-hospital", "Emergency department")
                      ),
  
  OnCall = factor(OnCall,levels = c(FALSE, TRUE), labels = c("Weekday", "On-call")),
  
  ed_gcs_cat = cut( #Categorising GCS in ED for the table
    ed_gcs_sum,
    breaks = c(3, 4, 6, 9, 13, 16),  # 3, 4–5, 6–8, 9–12, 13–15
    right  = FALSE,
    include.lowest = TRUE,
    labels = c("3", "4–5", "6–8", "9–12", "13–15")
    ), 
  ed_gcs_cat = factor(ed_gcs_cat, levels = c("13–15","9–12","6–8","4–5","3")),
  
  pre_gcs_cat = cut(
    pre_gcs_sum, breaks = c(3, 4, 6, 9, 13, 16),
    right = FALSE, 
    include.lowest = TRUE,
    labels = c("3", "4–5", "6–8", "9–12", "13–15")
    ),
  pre_gcs_cat = factor(pre_gcs_cat, levels = c("13–15","9–12","6–8","4–5","3")),
  
  ofi = factor(ofi,levels = c("Yes", "No"), 
               labels = c("Opportunity for improvement", "No opportunity for improvement")
              ),
  inj_mechanism = factor(inj_mechanism,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 999),
                         labels = c(
                           "Motor vehicle accident (not motorcycle)",
                           "Motorcycle accident",
                           "Bicycle accident",
                           "Injured pedestrian",
                           "Other vehicle accident (ship, aircraft, train, tram)",
                           "Gunshot injury",
                           "Stab or sharp object injury",
                           "Struck or hit by blunt object",
                           "Same-level fall (low-energy)",
                           "Fall from height (high-energy)",
                           "Explosion injury",
                           "Other cause of injury (asphyxiation, burns)",
                           "Not known"
                           )
                         )
)


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
               pre_gcs_sum ~ "Prehospital GCS",
               ed_gcs_sum ~ "ED GCS",
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
