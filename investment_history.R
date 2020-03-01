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
  summarize(BOOKING_DATE=max(BOOKING_DATE), MATURITY_DATE=max(MATURITY_DATE), COMBINED_TENOR= sum(TENOR), MAIN_COMP_RATE=max(MAIN_COMP_RATE), INVESTMENT =sum(LCY_AMOUNT))

investHistory$R.O.INVESTMENT <- investHistory$MAIN_COMP_RATE * investHistory$INVESTMENT

#DROPPING TABLES 
investmentHistory <- subset(investHistory, select = -c(BOOKING_DATE, MATURITY_DATE, MAIN_COMP_RATE))

# RATING THE INVESTMENTS AND THE ROI OF THE CUSTOMERS

investmentHistory$Investment_Rating = 
  ifelse(investmentHistory$`INVESTMENT` <= 0, 0,
    ifelse(investmentHistory$`INVESTMENT` <= 1000, 2,
      ifelse(investmentHistory$`INVESTMENT` <= 5000, 3,
        ifelse(investmentHistory$`INVESTMENT` <= 10000, 4, 
          ifelse(investmentHistory$`INVESTMENT` <= 50000, 5,
            ifelse(investmentHistory$`INVESTMENT` <= 100000, 7,
              ifelse(investmentHistory$`INVESTMENT` <= 500000, 8,
                ifelse(investmentHistory$`INVESTMENT` <= 1000000, 9, 10))))))))

# RATING THE ROI OF THE CUSTOMERS

investmentHistory$ROI_Rating = 
  ifelse(investmentHistory$`R.O.INVESTMENT` <= 0, 10000,
    ifelse(investmentHistory$`R.O.INVESTMENT` <= 100000, 4,
      ifelse(investmentHistory$`R.O.INVESTMENT` <= 500000, 5,
        ifelse(investmentHistory$`R.O.INVESTMENT` <= 1000000, 6, 
          ifelse(investmentHistory$`R.O.INVESTMENT` <= 5000000, 6,
            ifelse(investmentHistory$`R.O.INVESTMENT` <= 10000000, 7,
              ifelse(investmentHistory$`R.O.INVESTMENT` <= 50000000, 8,
                ifelse(investmentHistory$`R.O.INVESTMENT` <= 100000000, 9, 10))))))))

Investment_Profile <- subset(investmentHistory, select =c(CUSTOMER_UNIQUE_ID, Investment_Rating, ROI_Rating))


### THIS CONTAINS THE AMOUNT OF INVESTMENT A CUSTOMER HAS IN THE BANK. IT IS EQUAL TO THE INVESTMENT STRENGTH OF THE CUSTOMER. WE WILL MAKE USE OF THE INVESTMENT PROFILE TABLE FOR FURTHER ANALYSIS

