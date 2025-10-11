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

#Making OFI a factor and not characters 
merged.data <- merged.data %>%
  dplyr::mutate(ofi = factor(ofi, levels = c("Yes","No"))) 

#Filter data to variables that should be observed, remove patients with missing data 
#Make first descriptive table 

merged.data <- merged.data %>%
  mutate(
    TBI = (if_any(all_of(AIS_columns), ~ Is_TBI_AIS(.))) & Is_TBI_GCS(ed_gcs_sum, pre_gcs_sum)
  )




