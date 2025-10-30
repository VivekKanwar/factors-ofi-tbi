## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.

noacsr::source_all_functions()

# Load packages
library(rofi) # installed using remotes::install_github("martingerdin/rofi")

# Import data
data <- import_data(test = TRUE)

# Merge data
merged.data <- merge_data(data, test = TRUE)

# Add opportunities for improvement outcome
merged.data$ofi <- create_ofi(merged.data)

# Making OFI a factor and not characters 
merged.data <- merged.data %>%
  dplyr::mutate(ofi = factor(ofi, levels = c("No","Yes"))) 

# Calling function that defines TBI based on parametrers and values in the data 
source("functions/TBI_Definer.R")


# Adding column to data that indicates whether the patient has TBI or not 
# And also what severity of TBI

merged.data <- merged.data %>%
  mutate(
    
    # Get max AIS severity among 140-codes
    TBI_sev_num = apply(select(., all_of(AIS_columns)), 1, Get_TBI_Severity),
    
    # 2) TBI = had any 140-code
    TBI = !is.na(TBI_sev_num),
    
    # Map to categories
    TBI_sev_cat = case_when(
      is.na(TBI_sev_num)   ~ NA_character_,
      TBI_sev_num %in% 1:2 ~ "Mild",
      TBI_sev_num == 3     ~ "Moderate",
      TBI_sev_num %in% 4:6 ~ "Severe",
      TRUE                 ~ NA_character_
    )
  )

# merged.data <- merged.data %>%
#   mutate(
#     TBI = (if_any(all_of(AIS_columns), ~ Is_TBI_AIS(.))) 
#     

# & Is_TBI_GCS(ed_gcs_sum, pre_gcs_sum)
# )


# Making dataset with only patients that have TBI
TBI.only.data <- merged.data %>% filter(TBI)

# Cant find DOA as a variable, but filter the data so that they are excluded 

# Calling for code that filters my variables 
source("functions/Variable_filter.R") 

# Filtering my dataset to only the variables i want, including RTS and On call. How they are derived can be seen in Variable_filter
TBI.only.filtered <- TBI.only.data %>% select(all_of(Variables_wanted))

# Removing patients with missing Ofi, not really final sample because for my regression i will be removing all patients with missing 
Analysis.sample <- Remove_missing_ofi(TBI.only.filtered) 

# Calling for functions that produces the initial descriptive table 
source("functions/Variable_Organiser.R")
source("functions/Descriptive_table.R")

Descriptive.table1 # First baseline descriptive table

# Calling for function that cleans the data before analysing, then performing simple regression and producing corresponding tables/figures
source("functions/Analysis_simple.R")

SR.Table1 # Table describing unadjusted associations 

source("functions/Analysis_multi.R")

MV.Table1 # Table describing adjusted associations
