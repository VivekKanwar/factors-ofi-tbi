library(dplyr)


filter_ofi <- function(x) 
{
  dplyr::filter(x, !is.na(ofi) & ofi != "No") #Filtering patients so that only those where ofi is +ve are left
  }

ofi.data <- filter_ofi(merged.data) #testing if function works 

