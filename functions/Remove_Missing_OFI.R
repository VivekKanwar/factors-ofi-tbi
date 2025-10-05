library(dplyr)

Remove_missing_ofi <- function(x)  {
  dplyr::filter(x, !is.na(ofi)) #Filtering patients so that only those with an outcome are left 
}

Data.YN.ofi <- Remove_missing_ofi(merged.data) #Removing patients that do not have data on OFI 