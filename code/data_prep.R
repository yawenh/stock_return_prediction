## Data Preparation Part: 
##
## In this part, we get the backward return data we need to use for the prediction.
##
## Author: Yawen Hu
## Updated: November 17, 2020 

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)
# directories: ----------------------------------------------------------------
setwd("~/Desktop/urop/data")

# data: -----------------------------------------------------------------------
price = read_delim("price_data.csv", delim = ",")
bret_refer = read_delim("bret_ori.csv", delim = ",")

# function: --------------------------------------------------------------------
backward_return = function(a, h, data = price) {
  # Inputs:
  #     a - The asset number.
  #     h - represent h-min backward return.
  #     data - default to be `price` dataset.
  # Outputs: 
  #     the vector of h-min backward return of asset a.
  asset = paste0("Asset_", a)
  data =  data %>% dplyr::select(-X1)
  start_price = as.numeric(data[1, a])
  data = data %>%
    mutate(
      lag = DataCombine::shift(.data[[asset]], shiftBy = -h),
      lag = ifelse(is.na(lag), start_price, lag)
    ) %>%
    transmute( br = (.data[[asset]] - lag) / .data[[asset]] ) 
  return (as.vector(data$br))
}

forward_return = function(h, data = price) {
  # Inputs:
  #     h - represent h-min forward return
  #     data - default to be `price` dataset
  # Outputs: 
  #     the vector of h-min forward return of asset 1.
  last_price = as.numeric(data[524160, 2])
  data = data %>%
    mutate(
      lead = DataCombine::shift(Asset_1, shiftBy = h),
      lead = ifelse(is.na(lead), last_price, lead)
    ) %>%
    transmute( fr = (lead - Asset_1) / Asset_1 ) 
  return (as.vector(data$fr))
}

bret_dataframe = function(df, i, j) {
  varname = paste0("Asset_", i, "_BRet_", j)
  df[[varname]] = with(df, backward_return(i, j))
  return (df)
}

# bret: h - min backward return with Asset 1, 2, 3:
bret = price
for(j in c(3,10,30,60,120,180,240,360,480,600,720,960,1200,1440)){
  for(i in 1:3){
    bret = bret_dataframe(bret, i, j)
  }
}

fret = bret %>% 
  select(-c(1:4)) %>%
  mutate( Asset_1_FRet_10 = forward_return(10) )

save(fret, price, backward_return, bret_dataframe, file = "price_data.RData")


# 79: -------------------------------------------------------------------------
