library(dplyr)


filter_ofi <- function(x) 
{
  dplyr::filter(x, !is.na(ofi) & ofi != "No") 
  }

ofi.data <- filter_ofi(merged.data)
