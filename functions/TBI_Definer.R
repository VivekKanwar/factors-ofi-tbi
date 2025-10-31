
AIS_columns <- grep("^AISCode_", names(merged.data), value = TRUE) #Identifying the columns/variables that have the AIS Code (String)

# Function to extract the maximum AIS severity from intracranial (140xxx.x) codes
Get_TBI_Severity <- function(AIS_code) {
  AIS_code <- str_trim(as.character(AIS_code))  
  
  # Remove empty or missing entries
  AIS_code <- AIS_code[!is.na(AIS_code) & AIS_code != ""]
  if (length(AIS_code) == 0) return(NA_real_)
  
  # Keep only 140xxx.x codes 
  AIS_code <- AIS_code[str_detect(AIS_code, "^140")]
  if (length(AIS_code) == 0) return(NA_real_)
  
  # Extract the last digit (severity level 0–6) from each AIS code
  AIS.sev <- suppressWarnings(as.numeric(str_extract(AIS_code, "([0-6])$")))
  if (all(is.na(AIS.sev))) return(NA_real_)
  
  # Return the most severe/max AIS value among the patient's 140-codes.
  max(AIS.sev, na.rm = TRUE)
}
  

Is_TBI_GCS <- function (ed_gcs, pre_gcs) { #Function that sees if the patients have had a gcs 8 or under pre hosp or in the ed
  ed_is_TBI_GCS  <- suppressWarnings(as.numeric(ed_gcs))  <= 8 #Converts the value in ed_gcs to a number and hides any warnings (which become NA)
  pre_is_TBI_GCS <- suppressWarnings(as.numeric(pre_gcs)) <= 8 # ""
  (!is.na(ed_is_TBI_GCS)  & ed_is_TBI_GCS ) | ( !is.na(pre_is_TBI_GCS) & pre_is_TBI_GCS )#If either the ED or pre GCS is 8 or under return true
  
}

# https://journals.lww.com/euro-emergencymed/fulltext/2023/08000/effect_of_age_on_the_association_between_the.10.aspx
# https://thejns.org/view/journals/j-neurosurg/142/6/article-p1625.xml
# https://link.springer.com/article/10.1007/s00068-025-02909-4#:~:text=Two%20AIS%20variables%20were%20constructed,head%20injuries%20of%20a%20patient

#Find paper that defines TBI with AIS codes and use that as definer 

