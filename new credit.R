library(plyr)
library(dplyr)
library(data.table)
library(corrplot)


loansApp <- fread('all_loans_application.csv')
count(loansApp, APPLICATION_STATUS)
unique(loansApp)
count(loansApp, MODULE)
count(loansApp, DATE_OF_APPLY)
count(loansApp, CUSTOMER_UNIQUE_ID)

#Setting Customer ID as a character variable
loansApp$CUSTOMER_UNIQUE_ID <- sprintf("%f", loansApp$CUSTOMER_UNIQUE_ID)

#dropping missing values
library(tidyr)
loansApp %>% drop_na()
#aggregating the date of application per customer
appDate <- loansApp %>%
  group_by(CUSTOMER_UNIQUE_ID, MODULE, APPLICATION_STATUS, LOAN_CLASSIFICATION) %>%
  summarize(DATE_OF_APPLICATION = max(DATE_OF_APPLY), MEAN_APPLIED_AMOUNT = mean(APPLIED_AMOUNT), MEAN_INCR_ALLOWED= mean(INCR_ALLOWED))
  
LoanAppCredit <- subset(appDate, select = c(CUSTOMER_UNIQUE_ID, MEAN_APPLIED_AMOUNT, LOAN_CLASSIFICATION))

LoanAppCredit$NEW_LOAN_RATING = 
  ifelse(LoanAppCredit$`MEAN_APPLIED_AMOUNT` < 10000, 5,
    ifelse(LoanAppCredit$`MEAN_APPLIED_AMOUNT` <= 10000, 6, 
      ifelse(LoanAppCredit$`MEAN_APPLIED_AMOUNT` <= 50000, 7,
        ifelse(LoanAppCredit$MEAN_APPLIED_AMOUNT <= 100000, 8,
          ifelse(LoanAppCredit$MEAN_APPLIED_AMOUNT <= 500000, 8,
            ifelse(LoanAppCredit$MEAN_APPLIED_AMOUNT <= 1000000, 9, 10))))))


count(LoanAppCredit, NEW_LOAN_RATING)
