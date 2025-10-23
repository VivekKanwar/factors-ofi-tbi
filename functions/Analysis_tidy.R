Complete.analysis.sample <- Analysis.sample %>%  
  filter(if_all(-iva_dagar_n, ~ !is.na(.))) #Removing all patients that have missing data, EXCEPT iva dagar as that would leave my sample to be 2 ppl
                                            #and thus the analysis and tables/graphs will be bad. For the real data use drop.na() instead
Predictors <- Complete.analysis.sample %>% 
  select(-ofi,-iva_dagar_n) %>% #For now remove Iva dagar as well because the results wont be ascertained due to lack of data on that 
  names()

#For every variable (Predictors) in the cleaned dataset, we fit a separate
#logistic regression model with OFI (Yes/No) as the dependent variable.
#Each model is stored in the list `SR.model`.

SR.model <- map(Predictors, ~ glm(reformulate(.x, "ofi"),      
                                     data = Complete.analysis.sample,
                                     family = binomial)
                )                                                              
#Code below loops through both SR.model (the list of fitted models) and
#Predictors (the corresponding variable names) in parallel.
#For each pair (model, variable name):
#   Extracts regression results (OR, 95% CI, p-value) using broom::tidy()
#   Converts log-odds to odds ratios (exponentiate = TRUE)
#   Filters out the intercept term
#   Adds a column called 'variable' with the name of the predictor (.y)
#   Combines all small result tables into one large data frame (`SR.results`)
#   using row-binding (_dfr = "data frame row-bind").

SR.results <- map2_dfr(
  SR.model,                #List of fitted glm() models (one per predictor)
  Predictors,              #The matching predictor names (same order as SR.model)
  ~ broom::tidy(.x,        #Convert model .x into a tidy data frame
                conf.int = TRUE,            #Add 95% CIs
                exponentiate = TRUE) %>%   #Turn log-odds into ORs (exp(coef))
    filter(term != "(Intercept)") %>%       #Removing Intercept as the baseline OR is not relevant 
    mutate(variable = .y)                   #Add the predictor name (from .y) as a column
)

#Printing ORs of the simple regression models for each predictor 

#SR.results %>%
  #print(n = Inf)        #Print ALL rows