library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)


balances <- fread('all_balances.csv')

#make customer id a string variable
balances$CUSTOMER_UNIQUE_ID <- sprintf("%f", balances$CUSTOMER_UNIQUE_ID)

allBalances <- balances %>%
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarize(VALUE_DATE=max(VAL_DT), HIGHEST_BALANCE= max(BAL), MEAN_BALANCE= mean(BAL), BALANCE_HISTORY= sum(BAL))

