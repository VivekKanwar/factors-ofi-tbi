Descriptive.table1.data <- Analysis.sample  %>% Variable_Organiser()

source("functions/Variables_order_and_labels.R")

Variables_table1 <- c("ofi", Variables_ordered)

Descriptive.table1 <- Descriptive.table1.data %>%
  select(all_of(c("ofi", Variables_table1))) %>%
           tbl_summary(
             by = ofi,
             label = Labels_table1,
             missing = "ifany",
             missing_text = "No data",
             statistic = list(
               all_categorical() ~ "{n} ({p}%)",
               all_continuous() ~ "{median} ({p25}–{p75})"
             ),
             digits = list(RTS~1) #Rounding RTS to 1 decimal 
           )%>%
           add_overall() %>%
           bold_labels() %>%  
           modify_caption("**Table 1. Sample characteristics and processes**")
