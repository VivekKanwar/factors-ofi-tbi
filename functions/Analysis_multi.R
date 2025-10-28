
# Should we do a model adjusting for interactions as well, not only confounders? And we can use interaction model on 
# the predictors which are statistically significant in the Multivaribale analysis?

# Producing code that joins all variable names in the predictor vector into a single string separated by +
# Adding the ofi at the start and then converting it into a formula to fit in the regression model glm(formula, data, family)

MV.formula <- as.formula(paste("ofi ~", paste(Predictors, collapse = " + ")))

# Building the multivariable model with MV.formula containing ofi and all predictors

MV.model <- glm(MV.formula, 
                data = Complete.analysis.sample,
                family = binomial)

# Tidying results as OR with 95% CI

MV.results <- broom::tidy(
  MV.model,
  exponentiate = TRUE,
  conf.int = TRUE,
) %>%
  filter(term != "(Intercept)")

# Producing table that displays the adjusted ORs for the predictors using tbl_regression() 
# Adding 95% CI and P value with 3 decimals to table
# Also changing names with modify_header and label 

MV.Table1 <- tbl_regression(
  x = MV.model,
  exponentiate = TRUE,
  conf.level = 0.95,
  conf.int = TRUE,
  include = all_of(Predictors),
  pvalue_fun = label_style_pvalue(digits = 3),
  label = Labels_regression_tables, # For real data use Labels_table1
  missing = "ifany",
  missing_text = "No data"
) %>%
  bold_labels() %>%
  modify_header(
    label = "**Characteristics/Predictors**",
    estimate = "***Adjusted*** **OR**",
    conf.low = "**95% CI**",            
    p.value = "**p-value**"
  ) %>% 
  modify_caption("**Table 3. Adjusted logistic regression analyses of associations between patient level factors 
  and opportunities for improvement in TBI patients**")

#add_global_p()  # overall p value for each category if wanted
