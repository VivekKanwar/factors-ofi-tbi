#install.packages("broom")

#Data.YN.ofi <- Data.YN.ofi %>% dplyr::mutate(
#    Gender = dplyr::recode(Gender, "K" = "Female", "M" = "Male"), #Changing the variable name for tables and graphs later
#    Gender = factor(Gender, levels = c("Female","Male"))  # Female = reference so the OR will be males vs females
#    )
#m_gender <- glm(ofi ~ Gender, data = Data.YN.ofi, family = binomial()) #Performing the simple logistical regression comparing OFI in males Vs females

#broom::tidy(m_gender, conf.int = TRUE, exponentiate = TRUE) #Converting log-odds to OR and adding CI
