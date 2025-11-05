
Remove_missing_ofi <- function(x)  {
  dplyr::filter(x, !is.na(ofi)) #Filtering patients so that only those with an outcome are left 
}

#Adding column that says if it is on call times or not, derived from arrival time and date to hospital 

TBI.only.data <- TBI.only.data %>% #probs need to change the name of this and make a new dataset so when i run the whole main code it works
  mutate(
    ArrivalTime = ymd_hms(DateTime_ArrivalAtHospital, tz = "Europe/Stockholm"),
    
    #Extract hour (00–23) and weekday (Mon=1 ... Sun=7)
    ArrivalHour = hour(ArrivalTime),
    ArrivalDay = wday(ArrivalTime, week_start = 1),
    
    #Define On-call times: outside 08:00–17:00 OR Saturday/Sunday
    OnCall = case_when(
      is.na(ArrivalTime) ~ NA,                   #Keep missing timestamps
      ArrivalDay %in% c(6, 7) ~ TRUE,            #Saturday (6) or Sunday (7)
      ArrivalHour < 8 | ArrivalHour >= 17 ~ TRUE, #outside 08–17
      TRUE ~ FALSE                               # else, within working hours
    )
  )

# Adding column with RTS, SBP and RR are already present in the dataset 
# But to be consistent all three were coded for
# Need to consider 999 values and how ti interpret them 

TBI.only.data <- TBI.only.data %>%
  mutate(
    # Assigning values from 0-4 in order to calculate RTS
    ed_gcs_clean = case_when(
      is.na(ed_gcs_sum) | ed_gcs_sum %in% c(99, 999) ~ NA_real_,
        TRUE                                           ~ as.numeric(ed_gcs_sum)
      ),
      ed_sbp_clean = case_when(
        is.na(ed_sbp_value) | ed_sbp_value == 999      ~ NA_real_,
        TRUE                                           ~ as.numeric(ed_sbp_value)
      ),
      ed_rr_clean = case_when(
        is.na(ed_rr_value) | ed_rr_value %in% c(99, 999) ~ NA_real_,
        TRUE                                             ~ as.numeric(ed_rr_value)
      ),
      
      # RTS components (0–4)
      RTS_Gcs = case_when(
        is.na(ed_gcs_clean) ~ NA_real_,
        ed_gcs_clean >= 13  ~ 4,
        ed_gcs_clean >= 9   ~ 3,
        ed_gcs_clean >= 6   ~ 2,
        ed_gcs_clean >= 4   ~ 1,
        ed_gcs_clean == 3   ~ 0
      ),
      RTS_Sbp = case_when(
        is.na(ed_sbp_clean) ~ NA_real_,
        ed_sbp_clean > 89   ~ 4,
        ed_sbp_clean >= 76  ~ 3,
        ed_sbp_clean >= 50  ~ 2,
        ed_sbp_clean >= 1   ~ 1,
        ed_sbp_clean == 0   ~ 0
      ),
      RTS_Rr = case_when(
        is.na(ed_rr_clean)                    ~ NA_real_,
        ed_rr_clean >= 10 & ed_rr_clean <= 29 ~ 4,
        ed_rr_clean > 29                      ~ 3,
        ed_rr_clean >= 6  & ed_rr_clean <= 9  ~ 2,
        ed_rr_clean >= 1  & ed_rr_clean <= 5  ~ 1,
        ed_rr_clean == 0                      ~ 0
      ),
      
      # weighted RTS using published coeffecients
      RTS = 0.9368 * RTS_Gcs + 0.7326 * RTS_Sbp + 0.2908 * RTS_Rr
    ) %>%
    select(-RTS_Gcs, -RTS_Sbp, -RTS_Rr)  # Drop intermediates


Variables_wanted <- c( # Remove variables or add variables along the way 

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

# "hosp_vent_days"
