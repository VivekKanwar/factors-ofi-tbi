Variable_Organiser <- function(DataFrame) {
  
  DataFrame %>% mutate(
    
    # Gender
    Gender = factor(Gender, levels = c("M", "K"), labels = c("Male", "Female")),
    
    # CT to 10 min increments
    dt_ed_first_ct = dt_ed_first_ct / 10,
    
    # Ward level
    host_care_level = factor(host_care_level,
                         levels = c(1:5),
                         labels = c("Emergency Department", 
                                    "General Ward", 
                                    "Operating Theatre", 
                                    "High Dependency Unit", 
                                    "Critical Care Unit")
                         ),
    
    # Mechanical ventilation
    
    host_vent_days_NotDone = factor(host_vent_days_NotDone,
                                    levels = c(0,1),
                                    labels = c("Performed",
                                               "Not performed")
                                    ),
    
    # ASA Class
    
    pt_asa_preinjury = case_when(
      pt_asa_preinjury == 999 ~ NA_real_,   # treat 999 as NA
      TRUE                    ~ as.numeric(pt_asa_preinjury)
    ),
    
    pt_asa_preinjury = factor(
      pt_asa_preinjury,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("I", "II", "III", "IV", "V", "VI")
    ),
    
    
    # Intubation
    Intubation = case_when(
      pre_intubated == 1                       ~ "Pre-hospital",                # takes priority (even if ED==1)
      ed_intubated  == 1                       ~ "Emergency department",        # ED-only yes
      pre_intubated == 2 & ed_intubated == 2   ~ "None",                        # both no
      pre_intubated == 2 & is.na(ed_intubated) ~ "None",                        # pre=no, ED unknown
      ed_intubated  == 2 & is.na(pre_intubated)~ "None",                        # ED=no, pre unknown
      TRUE ~ NA_character_                                                      # both unknown or conflicting weirdness
    ),
    
    Intubation = factor(Intubation,
                        levels = c("Emergency department", "Pre-hospital", "None")
    ), 
    
    # On Call
    OnCall = factor(OnCall,levels = c(FALSE, TRUE), labels = c("Weekday", "On-call")),
    
    # GCS 
    ed_gcs_cat = case_when(
      is.na(ed_gcs_sum) | ed_gcs_sum %in% c(99, 999) ~ NA_character_,
      ed_gcs_sum == 3                ~ "3",
      ed_gcs_sum >= 4 & ed_gcs_sum <= 5   ~ "4–5",
      ed_gcs_sum >= 6 & ed_gcs_sum <= 8   ~ "6–8",
      ed_gcs_sum >= 9 & ed_gcs_sum <= 12  ~ "9–12",
      ed_gcs_sum >= 13 & ed_gcs_sum <= 15 ~ "13–15"
    ),
    ed_gcs_cat = factor(
      ed_gcs_cat,
      levels = c("13–15", "9–12", "6–8", "4–5", "3")
    ),
    
    # RR
    
    ed_rr_cat = case_when(
      is.na(ed_rr_value) | ed_rr_value %in% c(99, 999) ~ NA_character_,
      ed_rr_value == 0                ~ "0",
      ed_rr_value >= 1  & ed_rr_value <= 5   ~ "1–5",
      ed_rr_value >= 6  & ed_rr_value <= 9   ~ "6–9",
      ed_rr_value >= 10 & ed_rr_value <= 29  ~ "10–29",
      ed_rr_value >= 30               ~ ">29"
    ),
    ed_rr_cat = factor(
      ed_rr_cat,
      levels = c("10–29", ">29", "6–9", "1–5", "0")  # baseline: 10–29
    ),
    
    # SBP, 0 and 1-49 are merged as there were to few patients in 0 category for the regression model
    
    ed_sbp_cat = case_when(
      is.na(ed_sbp_value) | ed_sbp_value == 999 ~ NA_character_,  # 999 becomes NA
      ed_sbp_value >= 0  & ed_sbp_value <= 49  ~ "0–49",
      ed_sbp_value >= 50 & ed_sbp_value <= 75  ~ "50–75",
      ed_sbp_value >= 76 & ed_sbp_value <= 89  ~ "76–89",
      ed_sbp_value >= 90                       ~ ">89"
    ),
    ed_sbp_cat = factor(
      ed_sbp_cat,
      levels = c(">89", "76–89", "50–75", "0–49")  
    ),
    
    # Injury type 
    inj_dominant = case_when(
      inj_dominant == 999 ~ NA_real_,     # convert 999 to missing
      TRUE                 ~ as.numeric(inj_dominant)
    ),
    
    inj_dominant = factor(
      inj_dominant,
      levels = c(1,2),
      labels = c("Blunt","Penetrating")
      )
    )
}
