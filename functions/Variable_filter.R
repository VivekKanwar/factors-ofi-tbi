
# ---Function: Remove_missing_ofi()--------------------------

Remove_missing_ofi <- function(df)  {
  filter(df, !is.na(ofi))
}


# ---Function: Add_OnCall()--------------------------

Add_OnCall <- function(df,
                       datetime_col = "DateTime_ArrivalAtHospital",
                       tz = "Europe/Stockholm") {
  df <- df %>%
    mutate(
      
      # Extract hour (00–23) and weekday (Mon=1 ... Sun=7)
      ArrivalTime = ymd_hms(.data[[datetime_col]], tz = tz),
      ArrivalHour = hour(ArrivalTime),
      ArrivalDay  = wday(ArrivalTime, week_start = 1),
      
      # Define On-call times: outside 08:00–17:00 OR Saturday/Sunday
      OnCall = dplyr::case_when(
        is.na(ArrivalTime)              ~ NA,            # Missing
        ArrivalDay %in% c(6, 7)         ~ TRUE,          # Sat/Sun
        ArrivalHour < 8 | ArrivalHour >= 17 ~ TRUE,      # Outside hours
        TRUE                            ~ FALSE          # Else, Working hours
      )
    )
  df
}


# --- Function: Add_RTS()  --------------------------

Add_RTS <- function(df,
                    gcs_col = "ed_gcs_sum",
                    sbp_col = "ed_sbp_value",
                    rr_col  = "ed_rr_value") {
  stopifnot(all(c(gcs_col, sbp_col, rr_col) %in% names(df)))
  
  df <- df %>%
    dplyr::mutate(
      
      # Clean variables with no unknown or missing data (99/99)
      ed_gcs_clean = case_when(
        is.na(.data[[gcs_col]]) | .data[[gcs_col]] %in% c(99, 999) ~ NA_real_,
        TRUE ~ as.numeric(.data[[gcs_col]])
      ),
      ed_sbp_clean = case_when(
        is.na(.data[[sbp_col]]) | .data[[sbp_col]] == 999 ~ NA_real_,
        TRUE ~ as.numeric(.data[[sbp_col]])
      ),
      ed_rr_clean = case_when(
        is.na(.data[[rr_col]]) | .data[[rr_col]] %in% c(99, 999) ~ NA_real_,
        TRUE ~ as.numeric(.data[[rr_col]])
      ),
      
      # RTS components
      # Assigning values from 0-4 in order to calculate RTS
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
        is.na(ed_rr_clean)                      ~ NA_real_,
        ed_rr_clean >= 10 & ed_rr_clean <= 29   ~ 4,
        ed_rr_clean > 29                        ~ 3,
        ed_rr_clean >= 6  & ed_rr_clean <= 9    ~ 2,
        ed_rr_clean >= 1  & ed_rr_clean <= 5    ~ 1,
        ed_rr_clean == 0                        ~ 0
      ),
      
      # Weighted RTS (Trauma Score formula), using published coeffecients
      RTS = 0.9368 * RTS_Gcs + 0.7326 * RTS_Sbp + 0.2908 * RTS_Rr
    ) %>%
    select(-RTS_Gcs, -RTS_Sbp, -RTS_Rr, -ed_rr_clean, -ed_gcs_clean, -ed_sbp_clean) # Dropping intermediates
  
  df
}