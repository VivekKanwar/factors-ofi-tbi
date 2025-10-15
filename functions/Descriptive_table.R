
Descriptive.table1.data <- Final.sample  %>% mutate(                            #continue adding re-labelings and categorise certain continuous variables
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
  
  OnCall = factor(OnCall,levels = c(FALSE, TRUE), labels = c("Weekday", "On-call"))
)

Variables_table1 <- c("Intubation", setdiff(Variables_wanted, c("pre_intubated", "ed_intubated"))) #Adding the new Intubation label/variable. Can continue to add new variables/labels with setdiff 

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
               
               #Continuous
               pt_age_yrs ~ "Age (years)",
               ISS ~ "Injury Severity Score (ISS)",
               NISS ~ "New Injury Severity Score (NISS)",
               pre_sbp_value ~ "Prehospital systolic BP (mmHg)",
               ed_sbp_value ~ "ED systolic BP (mmHg)",
               pre_rr_value ~ "Prehospital respiratory rate (/min)",
               ed_rr_value ~ "ED respiratory rate (/min)",
               pre_gcs_sum ~ "Prehospital GCS total",
               ed_gcs_sum ~ "ED GCS total",
               dt_ed_first_ct ~ "Time to first CT (min)",
               RTS ~ "Revised Trauma Score (RTS)"
               
             ),
             missing = "ifany",
             statistic = list(
               all_categorical() ~ "{n} ({p}%)",
               all_continuous() ~ "{median} ({p25}–{p75})"
             )
           )%>%
           add_overall() %>%
           bold_labels() %>%  
           modify_caption("**Table 1.**")
