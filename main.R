## Welcome!

## This is your project's main script file and together with
## manuscript.qmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

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

# Adding column to data that indicates whether the patient has OFI or not 
merged.data <- merged.data %>%
  mutate(
    TBI = (if_any(all_of(AIS_columns), ~ Is_TBI_AIS(.))) & Is_TBI_GCS(ed_gcs_sum, pre_gcs_sum)
  )

# Making dataset with only patients that have TBI
TBI.only.data <- merged.data %>% filter(TBI)

# Cant find DOA as a variable, but filter the data so that they are excluded 

# Calling for coude that filters my variables 
source("functions/Variable_filter.R") 

# Filtering my dataset to only the variables i want, including RTS and On call. How they are derived can be seen in Variable_filter
TBI.only.filtered <- TBI.only.data %>% select(all_of(Variables_wanted))

# Removing patients with missing Ofi, not really final sample because for my regression i will be removing all patients with missing 
Analysis.sample <- Remove_missing_ofi(TBI.only.filtered) 

# Calling for function that produces the initial descriptive table 
source("functions/Descriptive_table.R")

Descriptive.table1 # First baseline descriptive table

# Calling for function that cleans the data before analysing, then performing simple regression and producing corresponding tables/figures
source("functions/Analysis_simple.R")

SR.Table1 # Table describing unadjusted associations 


