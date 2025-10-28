
AIS_columns <- grep("^AISCode_", names(merged.data), value = TRUE) #Identifying the columns/variables that have the AIS Code (String)

Is_TBI_AIS <- function(AIS_code) {
  AIS_code <- str_trim(as.character(AIS_code)) #Removing spaces and ensuring that the string is only characters
  Ais_empty <- is.na(AIS_code) | AIS_code == "" 
  Body_region <- str_detect(AIS_code, "^140")   # First digits = 140 = Head injury with intracranial injuries
  result_ais <- Body_region
  result_ais [Ais_empty] <- FALSE
  result_ais
  
  # Severity_scale <- as.numeric(str_extract(AIS_code, "([0-6])$"))  # Last digit = severity (1–6)
  # result_ais <- Body_region & !is.na(Severity_scale) & Severity_scale >= 3 #So if the body region is head (1) and the severity is 3 or more then the AIS is according to our def of TBI
#   result_ais [Ais_empty] <- FALSE #If the AIS code is missing or empty return FALSE and skip going through this string
# result_ais
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

