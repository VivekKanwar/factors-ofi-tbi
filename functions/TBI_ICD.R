
Get_TBI_ICD <- function(ICD_code) {
  
  # Ensure the input is treated as character data
  ICD_code <- str_trim(as.character(ICD_code))
  
  # Remove missing values (NA) and empty strings ("")
  ICD_code <- ICD_code[!is.na(ICD_code) & ICD_code != ""]
  
  # If no ICD codes remain after cleaning, the patient cannot meet the TBI definition
  if (length(ICD_code) == 0) return(FALSE)
  
  # Check whether ANY of the patient's ICD codes start with "S0" (ICD-10 head injury codes S00–S09)
  any(str_detect(ICD_code, "^S0"))
}

<<<<<<< HEAD
# Checks if a patient has any ICD code starting with a given block
=======
# --- Helper function: checks if a patient has ANY ICD code starting with a given block (e.g., "S06") ---
>>>>>>> 7e250c5b89c11adb5007330e5be1eff8249cdeaf
Has_ICD_Block <- function(ICD_code, block) {

  # Convert to character and trim whitespace
  ICD_code <- str_trim(as.character(ICD_code))

  # Remove missing/empty entries
  ICD_code <- ICD_code[!is.na(ICD_code) & ICD_code != ""]
  if (length(ICD_code) == 0) return(FALSE)

  # TRUE if any code starts with the specified block (e.g., "S06" matches "S06", "S06.1", "S06.5")
  any(str_detect(ICD_code, paste0("^", block)))
}

<<<<<<< HEAD
# Checks if any ICD code starts with a specific prefix, e.g. "S06.5"
=======
# Function: checks if any ICD code starts with a specific prefix, e.g. "S06.5"
>>>>>>> 7e250c5b89c11adb5007330e5be1eff8249cdeaf
Has_S06_Subcode <- function(ICD_code, subcode) {
  ICD_code <- str_trim(as.character(ICD_code))
  ICD_code <- ICD_code[!is.na(ICD_code) & ICD_code != ""]
  if (length(ICD_code) == 0) return(FALSE)
  
  any(str_detect(ICD_code, paste0("^", subcode)))
}



