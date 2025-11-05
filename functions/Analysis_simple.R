# Removing all patients that have NA in any of columns so i have complete data
# Using Variable_Organiser function to categorise and organise variables 

exclude_vars <- c("NISS", "ed_gcs_cat", "ed_sbp_cat", "ed_rr_cat")

Complete.analysis.sample <- Analysis.sample %>%
  Variable_Organiser() %>%
  select(-all_of(exclude_vars))

Predictors <- Variables_ordered %>%
  intersect(names(Complete.analysis.sample)) %>%
  setdiff(exclude_vars)

Complete.analysis.sample <- Complete.analysis.sample %>%  
  filter(if_all(all_of(Predictors), ~ !is.na(.)))        # drop rows missing any predictor
  

#For every variable (Predictors) in the Complete.analysis.sample data, we fit a separate
#logistic regression model with OFI (Yes/No) as the dependent variable.
#Each model is stored in the list `SR.model`.

SR.model <- map(Predictors, ~ glm(reformulate(.x, "ofi"),      
                                     data = Complete.analysis.sample,
                                     family = binomial)
                )   

# Code below loops through both SR.model and predictors in parallel.

SR.results <- map2_dfr(
  SR.model,                #List of fitted glm() models (one per predictor)
  Predictors,              #The matching predictor names (same order as SR.model)
  ~ broom::tidy(.x,        
                conf.int = TRUE,            #Add 95% CIs
                exponentiate = TRUE) %>%    #Turn log-odds into ORs 
    filter(term != "(Intercept)") %>%       #Removing Intercept as the baseline OR is not relevant 
    mutate(variable = .y)                   #Add the predictor name (from .y) as a column
)

# Producing table presenting the ORs ascertained from the above models using tbl_uvreggression() from gtsummary package
# Hiding no. of patients with hide_n, adding 95% CI and P value with 3 decimals to table
# Also changing names of columns with modify_header and list

SR.Table1 <- tbl_uvregression(
  data = Complete.analysis.sample,
  y = ofi,
  method = glm,
  method.args = list(family=binomial),
  exponentiate = TRUE,
  include = all_of(Predictors),
  hide_n = TRUE,
  conf.level = 0.95,
  conf.int = TRUE,
  pvalue_fun = label_style_pvalue(digits = 3),
  label = Labels_regression_tables, 
  missing = "ifany",
  missing_text = "No data"
) %>%
  bold_labels() %>%
  modify_header(
    label = "**Characteristics/Predictors**",
    estimate = "***Unadjusted*** **OR**",
    conf.low = "**95% CI**",            
    p.value = "**p-value**" 
  ) %>% 
  modify_caption("**Table 2. Unadjusted logistic regression analyses of associations between patient level factors 
  and opportunities for improvement in TBI patients**")
