# Removing all patients that have missing data, EXCEPT iva dagar as that would leave my sample to be 2 ppl
# and thus the analysis and tables/graphs will be bad. For the real data use drop.na() instead

Complete.analysis.sample <- Analysis.sample %>%  
  filter(if_all(-iva_dagar_n, ~ !is.na(.))) %>% 
  mutate(                         
  Gender = factor(Gender, levels = c("M", "K"), labels = c("Male", "Female")),
  
  host_care_level = factor(host_care_level,
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("Emergency Department", 
                                      "General Ward", 
                                      "Operating Theatre", 
                                      "High Dependency Unit", 
                                      "Critical Care Unit")
                           )
  
  
  
  
  )

Predictors <- Complete.analysis.sample %>% 
  select(-ofi,-iva_dagar_n) %>% #For now remove Iva dagar as well because the results wont be ascertained due to lack of data on that 
  names()

#For every variable (Predictors) in the Complete.analysis.sample data, we fit a separate
#logistic regression model with OFI (Yes/No) as the dependent variable.
#Each model is stored in the list `SR.model`.

SR.model <- map(Predictors, ~ glm(reformulate(.x, "ofi"),      
                                     data = Complete.analysis.sample,
                                     family = binomial)
                )                                                              
# Code below loops through both SR.model and predictors in parallel.
# For each pair (model, variable name/predictor):
#   Extracts regression results (OR, 95% CI, p-value) using broom::tidy()
#   Converts log-odds to odds ratios (exponentiate = TRUE)
#   Filters out the intercept term
#   Adds a column called 'variable' with the name of the predictor (.y)
#   Combines all small result tables into one large data frame (`SR.results`)
#   using row-binding (_dfr = "data frame row-bind").

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
# Also changing names of columns with modify_header

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
  pvalue_fun = label_style_pvalue(digits = 3)
) %>%
  bold_labels() %>%
  modify_header(
    label = "**Characteristics/Predictors**",
    estimate = "***Unadjusted*** **OR**",
    conf.low = "**95% CI**",            
    p.value = "**p-value**" 
  )
