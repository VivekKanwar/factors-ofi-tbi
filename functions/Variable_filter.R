
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
    # Assigning values from 0-4 to GCS in order to calculate RTS
    RTS_Gcs = case_when(
      is.na(ed_gcs_sum) ~ NA_real_,
      ed_gcs_sum >= 13 ~ 4,
      ed_gcs_sum >= 9  ~ 3,
      ed_gcs_sum >= 6  ~ 2,
      ed_gcs_sum >= 4  ~ 1,
      ed_gcs_sum == 3  ~ 0
    ),
    # Assigning values from 0-4 to SBP in order to calculate RTS
    RTS_Sbp = case_when(
      is.na(ed_sbp_value) ~ NA_real_,
      ed_sbp_value > 89  ~ 4,
      ed_sbp_value >= 76 ~ 3,
      ed_sbp_value >= 50 ~ 2,
      ed_sbp_value >= 1  ~ 1,
      ed_sbp_value == 0  ~ 0
    ),
    # Assigning values from 0-4 to RR in order to calculate RTS
    RTS_Rr = case_when(
      is.na(ed_rr_value) ~ NA_real_,
      ed_rr_value >= 10 & ed_rr_value <= 29 ~ 4,
      ed_rr_value > 29                      ~ 3,
      ed_rr_value >= 6 & ed_rr_value <= 9   ~ 2,
      ed_rr_value >= 1 & ed_rr_value <= 5   ~ 1,
      ed_rr_value == 0                      ~ 0
    ),
    RTS = 0.9368 * RTS_Gcs + 0.7326 * RTS_Sbp + 0.2908 * RTS_Rr # Calculating RTS according the published coefficients 
  ) %>%
  select(-RTS_Gcs, -RTS_Sbp, -RTS_Rr) # Choosing not to include these values in the dataset


Variables_wanted <- c( # Remove variables or add variables along the way 

# Categorical 
  "Gender",                                                                     # Sex
  "OnCall",                                                                     # On-call times -> hours outside of 08:00 - 17:00, or Saturday and Sunday
  "host_care_level",                                                            # Hospital care level  
  "pre_intubated","ed_intubated",                                               # Intubation status
  "inj_mechanism",                                                              # Mechanism of injury
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
