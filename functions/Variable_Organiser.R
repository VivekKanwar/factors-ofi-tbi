Variable_Organiser <- function(DataFrame) {
  
  DataFrame %>% mutate(
    
    # Gender
    Gender = factor(Gender, levels = c("M", "K"), labels = c("Malesssss", "Female")),
    
    # Ward level
    host_care_level = factor(host_care_level,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("Emergency Department", 
                                    "General Ward", 
                                    "Operating Theatre", 
                                    "High Dependency Unit", 
                                    "Critical Care Unit")
                         ),
    
    # Intubation
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
    
    # On Call
    OnCall = factor(OnCall,levels = c(FALSE, TRUE), labels = c("Weekday", "On-call")),
    
    # GCS 
    ed_gcs_cat = case_when(
      is.na(ed_gcs_sum)          ~ NA_character_,
      ed_gcs_sum == 3            ~ "3",
      ed_gcs_sum >= 4 & ed_gcs_sum <= 5  ~ "4–5",
      ed_gcs_sum >= 6 & ed_gcs_sum <= 8  ~ "6–8",
      ed_gcs_sum >= 9 & ed_gcs_sum <= 12 ~ "9–12",
      ed_gcs_sum >= 13 & ed_gcs_sum <= 15 ~ "13–15"
      ),
    
    ed_gcs_cat = factor(ed_gcs_cat,
                    levels = c("13–15", "9–12", "6–8", "4–5", "3")), #This wont present missing on the table, should not matter as we are doing complete case
    
    pre_gcs_cat = case_when(
      is.na(pre_gcs_sum)          ~ NA_character_,
      pre_gcs_sum == 3            ~ "3",
      pre_gcs_sum >= 4 & pre_gcs_sum <= 5  ~ "4–5",
      pre_gcs_sum >= 6 & pre_gcs_sum <= 8  ~ "6–8",
      pre_gcs_sum >= 9 & pre_gcs_sum <= 12 ~ "9–12",
      pre_gcs_sum >= 13 & pre_gcs_sum <= 15 ~ "13–15"
      ),
    pre_gcs_cat = factor(pre_gcs_cat, 
                     levels = c("13–15", "9–12","6–8", "4–5","3")),
    
    # RR
    pre_rr_cat= case_when(
      is.na(pre_rr_value)                  ~ NA_character_,
      pre_rr_value == 0                    ~ "0",
      pre_rr_value >= 1  & pre_rr_value <= 5  ~ "1–5",
      pre_rr_value >= 6  & pre_rr_value <= 9  ~ "6–9",
      pre_rr_value >= 10 & pre_rr_value <= 29 ~ "10–29",
      pre_rr_value >= 30                   ~ ">29"
    ),
    pre_rr_cat = factor(pre_rr_cat,
                        levels = c("10–29", ">29", "6–9", "1–5", "0")), # 10-29 baseline
    
    ed_rr_cat = case_when(
      is.na(ed_rr_value)                  ~ NA_character_,
      ed_rr_value == 0                    ~ "0",
      ed_rr_value >= 1  & ed_rr_value <= 5  ~ "1–5",
      ed_rr_value >= 6  & ed_rr_value <= 9  ~ "6–9",
      ed_rr_value >= 10 & ed_rr_value <= 29 ~ "10–29",
      ed_rr_value >= 30                   ~ ">29"
    ),
    ed_rr_cat = factor(ed_rr_cat,
                       levels = c("10–29", ">29", "6–9", "1–5", "0")),
    
    # SBP, 0 and 1-49 are merged as there were to few patients in 0 category for the regression model
    
    ed_sbp_cat = case_when(
      is.na(ed_sbp_value)           ~ NA_character_,
      ed_sbp_value >= 0  & ed_sbp_value <= 49  ~ "0–49",
      ed_sbp_value >= 50 & ed_sbp_value <= 75  ~ "50–75",
      ed_sbp_value >= 76 & ed_sbp_value <= 89  ~ "76–89",
      ed_sbp_value >= 90                     ~ ">89"
    ),
    ed_sbp_cat = factor(ed_sbp_cat,
                        levels = c(">89", "76–89", "50–75", "0–49")),
    
    pre_sbp_cat = case_when(
      is.na(pre_sbp_value)                 ~ NA_character_,
      pre_sbp_value >= 0  & pre_sbp_value <= 49  ~ "0–49",
      pre_sbp_value >= 50 & pre_sbp_value <= 75  ~ "50–75",
      pre_sbp_value >= 76 & pre_sbp_value <= 89  ~ "76–89",
      pre_sbp_value >= 90                       ~ ">89"
    ),
    pre_sbp_cat = factor(pre_sbp_cat,
                         levels = c(">89", "76–89", "50–75", "0–49")),
    
    
    
    # Injury mechanism 
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

}
