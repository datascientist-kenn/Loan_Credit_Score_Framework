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
  
loanAppCred <- DATE_OF_APPLICATION
