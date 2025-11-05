# Function to extract the maximum AIS severity from intracranial (1xxxxx.x) codes
Get_TBI_Severity <- function(AIS_code) {
  AIS_code <- str_trim(as.character(AIS_code))  
  
  # Remove empty or missing entries
  AIS_code <- AIS_code[!is.na(AIS_code) & AIS_code != ""]
  if (length(AIS_code) == 0) return(NA_real_)
  
  # Keep only 1xxxxx.x codes 
  AIS_code <- AIS_code[str_detect(AIS_code, "^1")]
  if (length(AIS_code) == 0) return(NA_real_)
  
  # Extract the last digit (severity level 0–6) from each AIS code
  AIS.sev <- suppressWarnings(as.numeric(str_extract(AIS_code, "([0-6])$")))
  if (all(is.na(AIS.sev))) return(NA_real_)
  
  # Return the most severe/max AIS value among the patient's 140-codes.
  max(AIS.sev, na.rm = TRUE)
}
