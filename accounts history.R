library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)

accounts <- fread('all_accounts.csv')

count(accounts, CCY)

newCCY <- accounts %>%
  mutate(CCY=recode(CCY, `NGN`=1, `GBP`=471.9, `USD`= 365.5, `EUR`=398.33))

#setting customer id as a string
accounts$CUSTOMER_UNIQUE_ID <- sprintf("%f", accounts$CUSTOMER_UNIQUE_ID)
newCCY$CUSTOMER_UNIQUE_ID <- sprintf("%f", newCCY$CUSTOMER_UNIQUE_ID)

#multiplying the balance and the new CCY in the newCCY table
newAVL_BAL <- as.data.frame(newCCY$ACY_AVL_BAL*newCCY$CCY)
newAVL_BAL$new_AVL_BAL <- newAVL_BAL$`newCCY$ACY_AVL_BAL * newCCY$CCY`

#Drop column on the new table
newAVL_BAL <- subset(newAVL_BAL, select = c(new_AVL_BAL))

#merging newAVL_BAL to the accounts table
accounts <- c(accounts, newAVL_BAL)
accounts <- as.data.frame(accounts)

# Dropping columns on the accounts table
accounts <- subset(accounts, select = -c(ACY_AVL_BAL, CCY))
accounts <- subset(accounts, select = -c(RECORD_STAT, MASKED_ACCOUNT))

#Sum the available balance
availBalance <- accounts %>% 
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarise(AVAIL_BALANCE = sum(new_AVL_BAL))

availBalance$AVAIL_BALANCE <- availBalance$AVAIL_BALANCE

availBalance$BALANCE_RATING = ifelse(availBalance$`AVAIL_BALANCE` <= 1000, 4,
  ifelse(availBalance$`AVAIL_BALANCE` <= 5000, 5,
    ifelse(availBalance$`AVAIL_BALANCE` <= 10000, 6, 
      ifelse(availBalance$`AVAIL_BALANCE` <= 50000, 6,
        ifelse(availBalance$`AVAIL_BALANCE` <= 100000, 7,
          ifelse(availBalance$`AVAIL_BALANCE` <= 500000, 8,
            ifelse(availBalance$`AVAIL_BALANCE` <= 1000000, 9, 10)))))))

acctBalanceRating = subset(availBalance, select = -c(AVAIL_BALANCE))

count(availBalance, BALANCE_RATING)

### WE WILL MAKE USE OF THE ACCOUNT_BALANCE_RATING TABLE FOR MERGING. THIS IS THE AMOUNT OF MONEY AVAILABLE IN THE CUSTOMER'S ACCOUNT AS AT THE LAST BOOKING DATE ON THE DATASET.
