library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)

invest <- fread('all_investments.csv')
count(invest, PRODUCT)
count(invest, invest$PRODUCT_NAME)
count(invest, invest$ROLLOVER_ALLOWED)
count(invest, invest$ROLLOVER_COUNT)
count(invest, invest$CONTRACT_STATUS)
colnames(invest)

#setting customer id as a string
invest$CUSTOMER_UNIQUE_ID <- sprintf("%f", invest$CUSTOMER_UNIQUE_ID)

# Dropping columns
newInvest = subset(invest, select = -c(CONTRACT_REF_NO, PAYMENT_METHOD, CURRENCY, AMOUNT, ROLLOVER_ALLOWED, ROLLOVER_COUNT, CONTRACT_STATUS, MASKED_ACCOUNT))

# GROUP_BY TO FIND THE INVESTMENT STRENGTH OF THE CUSTOMER
investHistory <- newInvest %>%
  group_by(CUSTOMER_UNIQUE_ID) %>% 
  summarize(BOOKING_DATE=max(BOOKING_DATE), MATURITY_DATE=max(MATURITY_DATE), COMBINED_TENOR= sum(TENOR), MAIN_COMP_RATE=max(MAIN_COMP_RATE), INVEST_STRENGTH =sum(LCY_AMOUNT))
