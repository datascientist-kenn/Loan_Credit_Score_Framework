library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(caret)
library(mlbench)
library(logitnorm)

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

availBalance$BALANCE_RATING = 
  ifelse(availBalance$`AVAIL_BALANCE` <= 1000, 4,
    ifelse(availBalance$`AVAIL_BALANCE` <= 5000, 5,
      ifelse(availBalance$`AVAIL_BALANCE` <= 10000, 6, 
        ifelse(availBalance$`AVAIL_BALANCE` <= 50000, 6,
          ifelse(availBalance$`AVAIL_BALANCE` <= 100000, 7,
            ifelse(availBalance$`AVAIL_BALANCE` <= 500000, 8,
              ifelse(availBalance$`AVAIL_BALANCE` <= 1000000, 9, 10)))))))

acctBalanceRating = subset(availBalance, select = -c(AVAIL_BALANCE))

count(availBalance, BALANCE_RATING)

### WE WILL MAKE USE OF THE ACCOUNT_BALANCE_RATING TABLE FOR MERGING. THIS IS THE AMOUNT OF MONEY AVAILABLE IN THE CUSTOMER'S ACCOUNT AS AT THE LAST BOOKING DATE ON THE DATASET.



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



########## I DID NOT RUN THE FOLLOWING CODES BELOW BECAUSE OF THE TOLL THEY TAKE ON MY PC. INSTEAD, I TOOK THE PATIENCE OF RECODING THE DATASET, DROPPING SOME COLUMNS AND PARSING INTO A NEW CSV FILE 
#Importing the all_transactions_digital table

#trans <- fread('all_transactions_digital.csv')

#setting customer id as a string
#trans$CUSTOMER_UNIQUE_ID <- sprintf("%f", trans$CUSTOMER_UNIQUE_ID)

#newTrans <- subset(trans, select= c(CUSTOMER_UNIQUE_ID, AC_CCY, LCY_AMOUNT, DRCR_IND))

#newTrans <- newTrans %>%mutate(AC_CCY=recode(AC_CCY, `NGN`=1, `GBP`=471.9, `USD`= 365.5, `EUR`=398.33))

#multiplying the LCY_AMOUNT and EX_RATE in the table
#newTrans$AMOUNT <- as.data.frame(newTrans$AC_CCY*newTrans$LCY_AMOUNT)
#newTrans$AMOUNT <- AMOUNT.newTrans$AC_CCY*newTrans$LCY_AMOUNT

# Categorising the Product with Dummy Variables
#library(dummies)
#transClean <- dummy(newTrans$DRCR_IND, sep="_")
#transClean <- as.data.frame(transClean)
#transClean$DRCR_IND <- transClean #this puts all the new dummy columns back into the data frame

#newTrans$CREDIT <- transClean$DRCR_IND_C
#newTrans$DEBIT <- transClean$DRCR_IND_D

#newTrans$CUSTOMER_UNIQUE_ID = sprintf("%f", newTrans$CUSTOMER_UNIQUE_ID)

#cashFlow <- newTrans %>%
#  group_by(CUSTOMER_UNIQUE_ID, CREDIT, DEBIT) %>%
#  summarize(TOTAL_CREDIT = CREDIT%*%LCY_AMOUNT, TOTAL_DEBIT = LCY_AMOUNT%*%DEBIT)
#write.csv(cashFlow, file = 'cashFlow.csv')


cashFlow <- fread('cashFlow.csv')

cashFlow$CUSTOMER_UNIQUE_ID = sprintf("%f", cashFlow$CUSTOMER_UNIQUE_ID)

cashFlowHistory <- cashFlow %>%
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarize(TOTAL_CREDIT = sum(TOTAL_CREDIT), TOTAL_DEBIT = sum(TOTAL_DEBIT))

cashFlowHistory$Balance <- cashFlowHistory$TOTAL_CREDIT - cashFlowHistory$TOTAL_DEBIT


cashFlowHistory$Balance_Rating = 
  ifelse(cashFlowHistory$`Balance` <= 0, 0,
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


bureau <- fread('bureau_score.csv')
count(bureau, bureau$BUREAU_SCORE)
count(bureau, bureau$BAL_NON_DELQ)
count(bureau, bureau$BAL_30DPD)
colnames(bureau)

#customer unique id as a string
bureau$CUSTOMER_UNIQUE_ID <- sprintf("%f", bureau$CUSTOMER_UNIQUE_ID)

debt_history <- bureau%>%
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarise(total30DPD=sum(BAL_30DPD), total60DPD=sum(BAL_60DPD), total90DPD=sum(BAL_PL_90DPD))

debt_history$total_debt <- debt_history$total30DPD + debt_history$total60DPD + debt_history$total90DPD

debt_history$Debt_Rating = 
  ifelse(debt_history$`total_debt` <= 0, 10,
    ifelse(debt_history$`total_debt` <= 1000, 9,
      ifelse(debt_history$`total_debt` <= 5000, 8,
        ifelse(debt_history$`total_debt` <= 10000, 7, 
          ifelse(debt_history$`total_debt` <= 50000, 6,
            ifelse(debt_history$`total_debt` <= 100000, 5,
              ifelse(debt_history$`total_debt` <= 500000, 3,
                ifelse(debt_history$`total_debt` <= 1000000, 1, 0))))))))

count(debt_history, Debt_Rating)
unique(debt_history)

# Dropping columns
Debt_Profile <- subset(debt_history, select = c(CUSTOMER_UNIQUE_ID, Debt_Rating))

## WE WILL MAKE USE OF THE DEBT_PROFILE TABLE FOR FURTHER ANALYSIS



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




loans <- fread('all_loans.csv')
count(loans, loans$CURRENCY)
count(loans, loans$ACCOUNT_STATUS)

#Finding the duration of the loan
TENURE <- as.Date.character((loans$MATURITY_DATE), format="%m/%d/%Y")- as.Date.character((loans$BOOK_DATE), format="%m/%d/%Y")
loans$TENURE <- TENURE

#recoding the Product code
library(dummies)
loansClean <- dummy(loans$PRODUCT_CODE, sep="_")
loansClean <- as.data.frame(loansClean)
loansClean <- subset(loansClean, select= c(PRODUCT_CODE_AMPC, PRODUCT_CODE_PDLP))
#this puts all the new dummy columns back into the data frame
loans$PRODUCT_CODE <- loansClean
loans <- c(loans, loansClean)
loans <- as.data.frame(loans)

#Renaming the new columns
#loansClean$PRODUCT_CODE_AMPC <- loans$PRODUCT_CODE.PRODUCT_CODE_AMPC
#loansClean$PRODUCT_CODE_PDLP <- loans$PRODUCT_CODE.PRODUCT_CODE_PLDP

#Making customer ID a character variable
loans$CUSTOMER_UNIQUE_ID <- sprintf("%f", loans$CUSTOMER_UNIQUE_ID)

#Summing up tenure to get the loan history by customer id
HISTORY <-
  aggregate(loans$TENURE, by=list(CUSTOMER_UNIQUE_ID=loans$CUSTOMER_UNIQUE_ID), FUN=sum )

HISTORY$history <- HISTORY$x
HISTORY <- subset(HISTORY, select= -c(x))


#Summing up amount the amount financed to get the total amount of loans each customer has recieved
loanHistory <-
  aggregate(loans$AMOUNT_FINANCED, by=list(CUSTOMER_UNIQUE_ID=loans$CUSTOMER_UNIQUE_ID), FUN=sum )
loanHistory$loanHistory <- loanHistory$x
loanHistory<- subset(loanHistory, select = -c(x))

#merging the history and loan history tables as a scoring table
Credit <- merge(HISTORY, loanHistory, by=intersect(names(HISTORY), names(loanHistory)))

#Removing colums
#newLoans <- subset(loans, select = -c(LOAN_REF, PRODUCT_CODE, PRODUCT_NAME, BOOK_DATE, MATURITY_DATE, CURRENCY, ACCOUNT_STATUS))


# Rating the history(days) and loanHistory (total loan amount) per customer
Credit$history_Rating =
  ifelse(Credit$`history` <= 300, 4,
    ifelse(Credit$`history` <= 500, 5,
      ifelse(Credit$`history` <= 1000, 6, 
        ifelse(Credit$`history` <= 2000, 6,
          ifelse(Credit$`history` <= 3000, 7,
            ifelse(Credit$`history` <= 4000, 8,
              ifelse(Credit$`history` <= 5000, 9, 10)))))))

Credit$loanHistory_Rating =
  ifelse(Credit$`loanHistory` <= 3000, 3,
    ifelse(Credit$`loanHistory` <= 10000, 4,
      ifelse(Credit$`loanHistory` <= 100000, 5, 
        ifelse(Credit$`loanHistory` <= 1000000, 6,
          ifelse(Credit$`loanHistory` <= 10000000, 7,
            ifelse(Credit$`loanHistory` <= 100000000, 8,
              ifelse(Credit$`loanHistory` <= 900000000, 9, 10)))))))

CreditProfile <- subset(Credit, select =  c(CUSTOMER_UNIQUE_ID, history_Rating, loanHistory_Rating))

## WE WILL USE THE CREDITPROFILE TABLE FOR FURTHER ANALYSIS



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
  ifelse(LoanAppCredit$`MEAN_APPLIED_AMOUNT` <= 5000, 5,
    ifelse(LoanAppCredit$`MEAN_APPLIED_AMOUNT` <= 10000, 6, 
      ifelse(LoanAppCredit$`MEAN_APPLIED_AMOUNT` <= 50000, 7,
        ifelse(LoanAppCredit$MEAN_APPLIED_AMOUNT <= 100000, 8,
          ifelse(LoanAppCredit$MEAN_APPLIED_AMOUNT <= 500000, 8,
            ifelse(LoanAppCredit$MEAN_APPLIED_AMOUNT <= 1000000, 9, 10))))))



demo <- fread(file='all_demographics.csv', header = TRUE, sep = "|", stringsAsFactors = FALSE)

## Selecting columns 
newDemo <- subset(demo, select = c(CUSTOMER_UNIQUE_ID, CUSTOMER_CATEGORY, MINOR, MARITAL_STATUS, DEPENDENT_OTHERS, SEX))

demographics <- newDemo

demographics <- demographics %>% mutate(CUSTOMER_CATEGORY=recode(CUSTOMER_CATEGORY, `C`= 10, `CORP`=10, `GOVT`=10, `HNI`=10, `I`=5, `IND`=7, `MINORS`=1, `RSA`=10, `S`=5, `SAL`=7, `STUDENTS`=4, `SUPERMARKT`=7, `TIER1`=10, `TIER2`=8, `WALKIN`=3))

demographics <- demographics %>% mutate(MINOR=recode(MINOR, `N`= 10, `Y`=1, `M`=1, `NA `=1))

demographics <- demographics %>% mutate(MARITAL_STATUS=recode(MARITAL_STATUS, `S`= 10, `P`=5, `M`=8, `E`=5, `D`=5, `NA`=1))

demographics <- demographics %>% mutate(DEPENDENT_OTHERS=recode(DEPENDENT_OTHERS, `0`= 10, `NA`=1))

demographics <- demographics %>% mutate(SEX=recode(SEX, `M`=7 , `F`=10, `P`=1, `NA `=1))

demographics <- demographics %>% mutate_all(~replace(.,is.na(.), 1))


#count(demographics, demographics$MARITAL_STATUS)



### MERGING ALL THE DATA SETS TOGETHER

df <-merge(acctBalanceRating, balanceHistoryRating, by=intersect(names(acctBalanceRating), names(balanceHistoryRating)), by.acctBalanceRating ="CUSTOMER_UNIQUE_ID", by.balanceHistoryRating ="CUSTOMER_UNIQUE_ID", all=TRUE, all.acctBalanceRating=all, all.balanceHistoryRating=all, sort=TRUE, suffixes=c(".acctBalanceRating", ".balanceHistoryRating"), no.dups=TRUE, incomparables = NULL)

df <-merge(df, CreditProfile, by=intersect(names(df), names(CreditProfile)), by.df ="CUSTOMER_UNIQUE_ID", by.CreditProfile ="CUSTOMER_UNIQUE_ID", all=TRUE, all.df=all, all.CreditProfile=all, sort=TRUE, suffixes=c(".df", ".CreditProfile"), no.dups=TRUE, incomparables = NULL)

df <-merge(df, cashFlowBalanceRating, by=intersect(names(df), names(cashFlowBalanceRating)), by.df ="CUSTOMER_UNIQUE_ID", by.cashFlowBalanceRating ="CUSTOMER_UNIQUE_ID", all=TRUE, all.df=all, all.cashFlowBalanceRating=all, sort=TRUE, suffixes=c(".df", ".cashFlowBalanceRating"), no.dups=TRUE, incomparables = NULL)

df <-merge(df, Debt_Profile, by=intersect(names(df), names(Debt_Profile)), by.df ="CUSTOMER_UNIQUE_ID", by.Debt_Profile ="CUSTOMER_UNIQUE_ID", all=TRUE, all.df=all, all.Debt_Profile=all, sort=TRUE, suffixes=c(".df", ".Debt_Profile"), no.dups=TRUE, incomparables = NULL)

df <-merge(df, demographics, by=intersect(names(df), names(demographics)), by.df ="CUSTOMER_UNIQUE_ID", by.demographics ="CUSTOMER_UNIQUE_ID", all=TRUE, all.df=all, all.demographics=all, sort=TRUE, suffixes=c(".df", ".demographics"), no.dups=TRUE, incomparables = NULL)

df <-merge(df, LoanAppCredit, by=intersect(names(df), names(LoanAppCredit)), by.df ="CUSTOMER_UNIQUE_ID", by.LoanAppCredit ="CUSTOMER_UNIQUE_ID", all=TRUE, all.df=all, all.LoanAppCredit=all, sort=TRUE, suffixes=c(".df", ".LoanAppCredit"), no.dups=TRUE, incomparables = NULL)


#Assigning all NA values to 1. This is to be on the safe side as the customers affected do not have these information on their profile. Rather than completely discard them, which will ultimately have a negative effect on our data set, we set the NA values to the least rating (just to assume worse case scenerios for the customers whose details are missing, resulting in the NA values). When the customers eventually provide these information when they come to apply for loans, their credit scores can then be boosted appropriately.

# Recoding the Loan_Classification column
df <- df %>% mutate(LOAN_CLASSIFICATION=recode(LOAN_CLASSIFICATION, `PERFORMING`= 1, `NON-PERFORMING`=0))

df <- df %>% mutate_all(~replace(.,is.na(.), 1))
df <- as.numeric(df)


count(df, df$BALANCE_RATING)
count(df, df$HIGHEST_BALANCE_RATING)
count(df, df$BALANCE_HISTORY_RATING)
count(df, df$history_Rating)
count(df, df$loanHistory_Rating)
count(df, df$Balance_Rating)
count(df, df$Debt_Rating)
unique(df)
count(df, df$CUSTOMER_CATEGORY)
count(df, df$MARITAL_STATUS)
count(df, df$LOAN_CLASSIFICATION)

#dropping columns
df <- subset(df, select = -c(MEAN_APPLIED_AMOUNT))

#Summing up all rows
df$TOTAL_SCORE <- df$BALANCE_RATING + df$HIGHEST_BALANCE_RATING + df$BALANCE_HISTORY_RATING + df$history_Rating + df$loanHistory_Rating + df$Balance_Rating + df$Debt_Rating + df$CUSTOMER_CATEGORY + df$MINOR + df$MARITAL_STATUS + df$DEPENDENT_OTHERS + df$SEX + df$NEW_LOAN_RATING

# Assigning the TOTAL POSSIBLE SCORE
df$TOTAL_POSSIBLE_SCORE <- 130

# Finding the PERCENTAGE of the CREDIT_SCORES
df$CREDIT_SCORE_PERCENT <- ((df$TOTAL_SCORE/df$TOTAL_POSSIBLE_SCORE)*100)
df$CREDIT_SCORE_PERCENT <-round(df$CREDIT_SCORE_PERCENT)

# Assigning the MINIMUM_PERCENT of SCORES allowed
df$MINIMUM_PERCENT_SCORE <- 50

# Indexing the CUSTOMER_UNIQUE_ID to become a row name, as a means of identifying the customers rather than a variable in the data set

count <- count(df, df$CREDIT_SCORE_PERCENT)
max(df$TOTAL_SCORE)

df$CUSTOMER_UNIQUE_ID <- as.numeric(df$CUSTOMER_UNIQUE_ID)



# Regression Analysis for my credit score model

# First dividing the data set into train and test set...

dt = sort(sample(nrow(df), nrow(df)*.7))
train <- (df[dt,])
test <- (df[-dt,])

# Search for redundant variables

set.seed(1000000000)
CorrelationMatrix <- cor(df)
library(corrplot)
corrplot::corrplot(CorrelationMatrix,  method = 'square') 
corrplot(CorrelationMatrix, type = "lower")  
corrplot.mixed(CorrelationMatrix, lower.col = "black", number.cex = .9)



# train the model: 
#I tried to do the recursive feature selection for these variables, but because their correlation values are very similar with regards to our dependent variable, the RFE didn't work. So now, we'll try a linear model.

CSF <- lm(CREDIT_SCORE_PERCENT~ train$BALANCE_RATING+train$HIGHEST_BALANCE_RATING+train$BALANCE_HISTORY_RATING+train$history_Rating+train$loanHistory_Rating+train$Balance_Rating+train$Debt_Rating+train$CUSTOMER_CATEGORY+train$MINOR+train$MARITAL_STATUS+train$DEPENDENT_OTHERS+train$SEX+train$NEW_LOAN_RATING, data=train)

plot(CSF)
summary(CSF)
anova(CSF)

## Predictive analysis for the credit score framework
predict(CSF, data.frame(test))
predCSF <- as.data.frame(predict(CSF, data.frame(test)))


# Loan Classification Model (Logit Model)...
LCM <- glm(LOAN_CLASSIFICATION ~ BALANCE_RATING + HIGHEST_BALANCE_RATING + BALANCE_HISTORY_RATING + history_Rating + loanHistory_Rating + Balance_Rating + Debt_Rating + CUSTOMER_CATEGORY + MINOR + MARITAL_STATUS + DEPENDENT_OTHERS + SEX + NEW_LOAN_RATING, data=train, family = binomial(link = "logit"))
summary(LCM)

# Predictive analysis for the loan classification model
predLCM <- predict(LCM, data.frame(test))
invlogit(predLCM)

table(test$LOAN_CLASSIFICATION, invlogit(predLCM) > 0.5)

predLCM <- as.data.frame(round(as.numeric(invlogit(predLCM))))

# Conclusion