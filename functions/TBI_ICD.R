
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

