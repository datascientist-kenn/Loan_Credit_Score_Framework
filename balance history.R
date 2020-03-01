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

balanceHistory = subset(allBalances, select= -c(VALUE_DATE, MEAN_BALANCE))

#GRADING THE HIGHEST BALANCE OF THE CUSTOMERS
balanceHistory$HIGHEST_BALANCE_RATING = 
  ifelse(balanceHistory$`HIGHEST_BALANCE` <= 1000, 4,
    ifelse(balanceHistory$`HIGHEST_BALANCE` <= 5000, 5,
      ifelse(balanceHistory$`HIGHEST_BALANCE` <= 10000, 6, 
        ifelse(balanceHistory$`HIGHEST_BALANCE` <= 50000, 6,
          ifelse(balanceHistory$`HIGHEST_BALANCE` <= 100000, 7,
            ifelse(balanceHistory$`HIGHEST_BALANCE` <= 500000, 8,
              ifelse(balanceHistory$`HIGHEST_BALANCE` <= 1000000, 9, 10)))))))


#GRADING THE BALANCE_HISTORY OF THE CUSTOMERS
balanceHistory$BALANCE_HISTORY_RATING = 
  ifelse(balanceHistory$`BALANCE_HISTORY` <= 1000, 4,
    ifelse(balanceHistory$`BALANCE_HISTORY` <= 5000, 5,
      ifelse(balanceHistory$`BALANCE_HISTORY` <= 10000, 6, 
        ifelse(balanceHistory$`BALANCE_HISTORY` <= 50000, 6,
          ifelse(balanceHistory$`BALANCE_HISTORY` <= 100000, 7,
            ifelse(balanceHistory$`BALANCE_HISTORY` <= 500000, 8,
              ifelse(balanceHistory$`BALANCE_HISTORY` <= 1000000, 9, 10)))))))

count(balanceHistory, HIGHEST_BALANCE_RATING)
count(balanceHistory, BALANCE_HISTORY_RATING)

balanceHistoryRating <- subset(balanceHistory, select = -c(HIGHEST_BALANCE, BALANCE_HISTORY))

### WE MAKE USE OF BOTH THE HIGHEST_BALANCE AND BALANCE_HISTORY TABLE FOR OUR ANALYSIS.THIS TABLE CONTAINS THE HIGHEST ACCOUNT BALANCE IN THIS CUSTOMER'S HISTORY. IT ALSO CONTAINS THE SUM OF ALL THE AMOUNTS THAT HAS PASSED THROUGH THIS CUSTOMER'S ACCOUNT.