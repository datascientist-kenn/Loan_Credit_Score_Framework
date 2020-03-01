library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(future)


trans <- fread('all_transactions_digital.csv')

count(trans, trans$AC_CCY)

#setting customer id as a string
trans$CUSTOMER_UNIQUE_ID <- sprintf("%f", trans$CUSTOMER_UNIQUE_ID)

newTrans <- subset(trans, select= c(CUSTOMER_UNIQUE_ID, AC_CCY, LCY_AMOUNT, DRCR_IND))

newTrans <- newTrans %>%mutate(AC_CCY=recode(AC_CCY, `NGN`=1, `GBP`=471.9, `USD`= 365.5, `EUR`=398.33))

#multiplying the LCY_AMOUNT and EX_RATE in the table
newTrans$AMOUNT <- as.data.frame(newTrans$AC_CCY*newTrans$LCY_AMOUNT)

colnames(newTrans)

newTrans$AMOUNT <- AMOUNT.newTrans$AC_CCY*newTrans$LCY_AMOUNT

# Categorising the Product with Dummy Variables
library(dummies)
transClean <- dummy(newTrans$DRCR_IND, sep="_")
transClean <- as.data.frame(transClean)
transClean$DRCR_IND <- transClean #this puts all the new dummy columns back into the data frame


newTrans$CREDIT <- transClean$DRCR_IND_C
newTrans$DEBIT <- transClean$DRCR_IND_D

newTrans$CUSTOMER_UNIQUE_ID = sprintf("%f", newTrans$CUSTOMER_UNIQUE_ID)


cashFlow <- newTrans %>%
  group_by(CUSTOMER_UNIQUE_ID, CREDIT, DEBIT) %>%
  summarize(TOTAL_CREDIT = CREDIT%*%LCY_AMOUNT, TOTAL_DEBIT = LCY_AMOUNT%*%DEBIT)


library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)

cashFlow <- fread('cashFlow.csv')

cashFlow$CUSTOMER_UNIQUE_ID = sprintf("%f", cashFlow$CUSTOMER_UNIQUE_ID)

cashFlowHistory <- cashFlow %>%
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarize(TOTAL_CREDIT = sum(TOTAL_CREDIT), TOTAL_DEBIT = sum(TOTAL_DEBIT))

cashFlowHistory$Balance <- cashFlowHistory$TOTAL_CREDIT - cashFlowHistory$TOTAL_DEBIT


cashFlowHistory$Balance_Rating = ifelse(cashFlowHistory$`Balance` <= 0, 0,
  ifelse(cashFlowHistory$`Balance` <= 1000, 4,
    ifelse(cashFlowHistory$`Balance` <= 5000, 5,
      ifelse(cashFlowHistory$`Balance` <= 10000, 6, 
        ifelse(cashFlowHistory$`Balance` <= 50000, 6,
          ifelse(cashFlowHistory$`Balance` <= 100000, 7,
            ifelse(cashFlowHistory$`Balance` <= 500000, 8,
              ifelse(cashFlowHistory$`Balance` <= 1000000, 9, 10))))))))

count(cashFlowHistory, Balance_Rating)

# DROPPING COLUMNS
cashFlowBalanceRating <- subset(cashFlowHistory, select = c(CUSTOMER_UNIQUE_ID, Balance_Rating))

### THIS DATASET CONTAINS THE SUM OF BOTH CREDIT AND DEBIT TRANSACTIONS ON EACH CUSTOMER'S ACCOUNT WITHIN THE PERIOD SPECIFIED. IT ALSO CONTAINS THE NET DIFFERENCE BETWEEN THEM, BUR WE WILL USE THE CASHFLOW$BALANCE_RATING FOR OUR CREDIT SCORE FRAMEWORK