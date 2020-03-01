

# MAKE SURE TO CONVERT ALL CURRENCY AMOUNTS TO THE NAIRA EQUIVALENT

# Set the all_loans_application data frame = df
df <- read.csv('all_loans_application.csv')
head(df)
df

unique(df)

# Performing a summary on our target variable
summary(df$LOAN_CLASSIFICATION)
# Plotting the summary
plot(df$LOAN_CLASSIFICATION)

#Recoding the target variable to PERFORMING =1 and NON-PERFORMING =0
library(dplyr)
df <- df %>% mutate(LOAN_CLASSIFICATION=recode(LOAN_CLASSIFICATION, `PERFORMING`= 1, `NON-PERFORMING`=0))
tail(df)

# Corrplot to show the correlation of the data set with our target variable
library(corrplot)
cordf <- cor(df)
corrplot::corrplot(cordf,  method = 'square') 
corrplot(cordf, type = "lower")  
corrplot.mixed(cordf, lower.col = "black", number.cex = .9)

#Removing columns
df= subset(df, select=-c(OFFERID, DATE_OF_APPLY, APPLICATION_STATUS, MODULE))
df

#Removing duplicate rows in df
newDf<-unique(df)

# Corrplot to show the correlation of the data set with our target variable
library(corrplot)
corNewdf <- cor(newDf)
corrplot::corrplot(corNewdf,  method = 'square') 
corrplot(corNewdf, type = "lower")  
corrplot.mixed(corNewdf, lower.col = "black", number.cex = .9)


# Cleaning the all_loans.csv dataset
Loans <- read.csv('all_loans.csv')
Loans

#Finding the duration of the loan
TENURE_DAYS <- as.Date.character((Loans$MATURITY_DATE), format="%m/%d/%Y")- as.Date.character((Loans$BOOK_DATE), format="%m/%d/%Y")

Loans$TENURE_DAYS <- TENURE_DAYS
# Extracting the numbers from the date and leaving the character part
TENURE_DAYS <- (as.difftime(Loans$LOAN_TENURE, "[0-9]+"))
TENURE_DAYS
Loans$TENURE_DAYS <- TENURE_DAYS
Loans

# Summary on the dataset
summary(Loans$CURRENCY)
summary(Loans$PRODUCT_NAME)
summary(Loans$PRODUCT_CODE)
plot(Loans$PRODUCT_CODE)

#Categorising the product code with dummy variables
library(dummies)
loansClean <- dummy(Loans$PRODUCT_CODE, sep="_")
loansClean <- as.data.frame(loansClean)
loansClean <- subset(loansClean, select= c(PRODUCT_CODE_AMPC, PRODUCT_CODE_PDLP))
#this puts all the new dummy columns back into the data frame
Loans$PRODUCT_CODE <- loansClean
# Separing the AMPC and PLDP product names so we can get to see their impact on our target variable later
Loans$PRODUCT_CODE.PRODUCT_CODE_AMPC <- loansClean$PRODUCT_CODE_AMPC
Loans$PRODUCT_CODE.PRODUCT_CODE_PLDP <- loansClean$PRODUCT_CODE_PDLP
Loans

# Dropping all character variables
newLoans=  subset(Loans, select= c(CUSTOMER_UNIQUE_ID, PRODUCT_CODE.PRODUCT_CODE_AMPC, PRODUCT_CODE.PRODUCT_CODE_PLDP, AMOUNT_FINANCED, TENURE_DAYS))
newLoans

#dropping repeated rows
unique(newLoans)
distinct(newLoans)
newLoansClean <- unique(newLoans)

#Converting the dataframe to a matrix to check for correlation 
numNewLoansClean <- sapply(newLoansClean, as.numeric)

# Correlation plot
library(corrplot)
corNewLoansClean <- cor(numNewLoansClean)
corrplot::corrplot(corNewLoansClean,  method = 'square') 
corrplot(corNewLoansClean, type = "lower")  
corrplot.mixed(corNewLoansClean, lower.col = "black", number.cex = .9)


#setting the CUSTOMER_UNIQUE_ID as a large 9 digit number for both dataframes
newDf$CUSTOMER_UNIQUE_ID <- sprintf("%f", newDf$CUSTOMER_UNIQUE_ID)

newLoansClean$CUSTOMER_UNIQUE_ID <- sprintf("%f", newLoansClean$CUSTOMER_UNIQUE_ID)

# Merging the two datasets
mDf <- merge(newDf, newLoansClean, by=intersect(names(newDf), names(newLoansClean)), by.newDf ="CUSTOMER_UNIQUE_ID", by.newLoansClean ="CUSTOMER_UNIQUE_ID", all=TRUE, all.newDf=all, all.newLoansClean=all, sort=TRUE, suffixes=c(".newDf",".newLoansClean"), no.dups=TRUE, incomparables = NULL)
mDf
mDf<- as.data.frame(mDf)

duplicated(mDf)
distinct(mDf)
unique(mDf)

# Convert Customer ID back to numeric value to check for correlation
mDf$CUSTOMER_UNIQUE_ID<- as.numeric(mDf$CUSTOMER_UNIQUE_ID)

# Converting the mDf dataframe to a numeric dataframe
numMdf <- sapply(mDf, as.numeric)

# Corrplot to show the correlation of the data set with our target variable
library(corrplot)
corMdf <- cor(numMdf)
corrplot::corrplot(corMdf,  method = 'square') 
corrplot(corMdf, type = "upper")  
corrplot.mixed(corMdf, lower.col = "black", number.cex = .9)


# Loading the Bureau Score Dataset
bdf <- read.csv('bureau_score.csv')
bdf
distinct(bdf)
summary(bdf$BUREAU_SCORE, bdf$CUSTOMER_TYPE, bdf$LOAN_CT_60DPD, bdf$BAL_60DPD)
# Dropping columns
newBdf=  subset(bdf, select= -c(CUSTOMER_TYPE, BUREAU_SCORE, CUSTOMER_TYPE, LOAN_CT_60DPD, BAL_60DPD))
newBdf

# Checking for duplicates
unique(newBdf)

#Converting all columns to numeric
numBdf <- sapply(newBdf, as.numeric)

# Corrplot to show the correlation of the data set with our target variable
library(corrplot)
corBdf <- cor(numBdf)
corrplot::corrplot(corBdf,  method = 'square') 
corrplot(corBdf, type = "upper")  
corrplot.mixed(corBdf, lower.col = "black", number.cex = .9)



#setting the CUSTOMER_UNIQUE_ID as a large 9 digit number for both dataframes
mDf$CUSTOMER_UNIQUE_ID <- sprintf("%f", mDf$CUSTOMER_UNIQUE_ID)

#Changing Customer unique ID to character variable
newBdf$CUSTOMER_UNIQUE_ID <- sprintf("%f", newBdf$CUSTOMER_UNIQUE_ID)


#Merging with the master dataset
mDf1 <- merge(mDf, newBdf, by=intersect(names(mDf), names(newBdf)), by.mDf ="CUSTOMER_UNIQUE_ID", by.newBdf ="CUSTOMER_UNIQUE_ID", all=TRUE, all.mDf=all, all.newBdf=all, sort=TRUE, suffixes=c(".mDf",".newBdf"), no.dups=TRUE, incomparables = NULL)
mDf1

#Converting the character table to both matrix
numMDf1 <- sapply(mDf1, as.numeric)


#Corrplot to show correlation
library(corrplot)
corMDf1 <- cor(numMDf1)
corrplot::corrplot(corMDf1,  method = 'square') 
corrplot(corMDf1, type = "upper")  
corrplot.mixed(corMDf1, lower.col = "black", number.cex = .5)



# Loading the all_investment.csv file
invest <- read.csv('all_investments.csv')
invest

summary(invest)
summary(invest$PRODUCT_NAME)
summary(invest$PAYMENT_METHOD)
summary(invest$ROLLOVER_ALLOWED)
summary(invest$ROLL_OVER_COUNT)
summary(invest$CONTRACT_STATUS)

# Categorising the Product with Dummy Variables
library(dummies)
investClean <- dummy(investClean$PRODUCT, sep="_")
investClean <- as.data.frame(investClean)
investClean <- subset(investClean, select= c(PRODUCT_MTDB, PRODUCT_TBCS))
investClean$PRODUCT <- investClean #this puts all the new dummy columns back into the data frame

invest$PRODUCT.PRODUCT_MTDB <- investClean$PRODUCT_MTDB
invest$PRODUCT.PRODUCT_TBCS <- investClean$PRODUCT_TBCS
invest


invest$PRODUCT <- as.data.frame(investClean)
invest$PRODUCT
invest

# Dropping irrelevant columns
newInvest=  subset(invest, select= -c(CONTRACT_REF_NO, PRODUCT_NAME, PAYMENT_METHOD, CURRENCY, BOOKING_DATE, MATURITY_DATE, ROLLOVER_ALLOWED, ROLLOVER_COUNT, CONTRACT_STATUS, MASKED_ACCOUNT))
newInvest

# Setting the Customer ID to character variable
newInvest$CUSTOMER_UNIQUE_ID<- sprintf("%f", newInvest$CUSTOMER_UNIQUE_ID)
newInvest$CUSTOMER_UNIQUE_ID
newInvest

#Merging with the master df
mDf2 <- merge(mDf1, newInvest,  by=intersect(names(mDf1), names(newInvest)), by.mDf1 ="CUSTOMER_UNIQUE_ID", by.newInvest ="CUSTOMER_UNIQUE_ID", all=TRUE, all.mDf1=all, all.newInvest=all, sort=TRUE, suffixes=c(".mDf1",".new_aidf"), no.dups=TRUE, incomparables = NULL)
mDf2
# It is probably best if we leave this table as it is and not merge it because of its relatively small size.

# Loading the all_accounts.csv
account <- read.csv('all_accounts.csv')
account
summary(account)
summary(account$CCY)
plot(account$CCY)
summary(account$RECORD_STAT)
summary(account$ACY_AVL_BAL)

#This table does not have much information for us. So we drop the table altogether


# Loading the all_balances.csv file
balance <- read.csv("all_balances.csv")
balance
# Seeing the duplicate CustomerID and MASKED ACCOUNT number, it is possible to have duplicates. SO we check for unique values
unique(balance)
summary(balance)

#Writing the CUSTOMER_ID as a character variable to avoid loss of information
balance$CUSTOMER_UNIQUE_ID <- sprintf("%f", abdf$CUSTOMER_UNIQUE_ID)
balance

#Dropping columns
newBalance=  subset(abdf, select= -c(MASKED_ACCOUNT, VAL_DT))
newBalance

#Merging with the Master dataset
mDf2 <- merge(mDf1, newBalance,  by=intersect(names(mDf1), names(newBalance)), by.mDf1 ="CUSTOMER_UNIQUE_ID", by.newBalance ="CUSTOMER_UNIQUE_ID", all=TRUE, all.mDf1=all, all.newBalance=all, sort=TRUE, suffixes=c(".mDf1",".new_abdf"), no.dups=TRUE, incomparables = NULL)
mDf2


library(future)
demo %<-% read.csv(file='all_demographics.csv', header = TRUE, sep = "|", stringsAsFactors = FALSE)
demo
colnames(demo)

summary(demo)
count(demo, demo$SALARY)
count(demo, demo$CUSTOMER_TYPE)
count(demo, demo$CUSTOMER_CATEGORY)
count(demo, demo$CUST_CLASSIFICATION)
count(demo, demo$CUST_CLG_GROUP)
count(demo, demo$CUSTOMER_PREFIX)
count(demo, demo$CUST_COMM_MODE)
count(demo, demo$COUNTRY)
count(demo, demo$NATIONALITY)
count(demo, demo$LANGUAGE)
count(demo, demo$FROZEN)
count(demo, demo$HO_AC_NO)
count(demo, demo$CUSTOMER_UNIQUE_ID)










#dropping columns
newDemo=  subset(demo, select= -c(CUSTOMER_TYPE, CUSTOMER_CATEGORY, CUST_CLASSIFICATION, CUST_CLG_GROUP, ADDRESS_LINE3, ADDRESS_LINE4, NATIONALITY, LANGUAGE, EXPOSURE_COUNTRY, LOCAL_BRANCH, FROZEN, DECEASED, WHEREABOUTS_UNKNOWN, HO_AC_NO))

colnames(newDemo)
summary(demo$CCY_PERS_INCEXP)
summary(newDemo$COUNTRY)
summary(demo$PAST_DUE_FLAG)
count(demo, PAST_DUE_FLAG)#rework the past due flag variable, it is important
demo$PAST_DUE_FLAG
count(demo, RISK_PROFILE)#rework this and RISK category to logical types
count(demo, demo$CIF_STATUS)


newDemo=  subset(newDemo, select= -c(CCY_PERS_INCEXP, E_COUNTRY, CREDIT_CARDS, PREV_EMPLOYER, PREV_DESIGNATION, PREF_CONTACT_TIME, PREF_CONTACT_DT, P_PINCODE, D_PINCODE, VST_US_PREV, US_RES_STATUS, BIRTH_COUNTRY, PLACE_OF_BIRTH, AGE_PROOF_SUBMITTED, DATE_OF_BIRTH, PINCODE))
colnames(newDemo)

newDemo<-  subset(newDemo, select= -c(UTILITY_PROVIDER_ID, UDF_1, UDF_2, UDF_3, UDF_4, UDF_5, CAS_CUST, CIF_STATUS, CIF_STATUS_SINCE, CHARGE_GROUP, SSN, SWIFT_CODE, LOC_CODE, UTILITY_PROVIDER_ID, RISK_PROFILE, RISK_CATEGORY, AML_CUSTOMER_GRP, GROUP_CODE, EXPOSURE_CATEGORY, CHK_DIGIT_VALID_REQD, ALG_ID, TAX_GROUP))
colnames(newDemo)

summary(newDemo$SEC_CUST_CLEAN_RISK_LIMIT)
typeof(newDemo$KYC_DETAILS)
count(newDemo, newDemo$DEBTOR_CATEGORY)

newDemo<-  subset(newDemo, select= -c(COUNTRY, LIMIT_CCY, LIAB_NODE, FREQUENCY, AUTOGEN_STMTPLAN, RM_ID, AUTO_CUST_AC_NO, AUTO_CREATE_ACCOUNT, ELCM_CUSTOMER_NO, ELCM_CUSTOMER, LC_COLLATERAL_PCT, PRIVATE_CUSTOMER, JOINT_VENTURE, UTILITY_PROVIDER_TYPE, KYC_REF_NO, RP_CUSTOMER, WHT_PCT, CIF_CREATION_DATE, EXT_REF_NO, INTRODUCER, DEBTOR_CATEGORY))


library(tidyverse)

library(dplyr)
demo <- demo %>% mutate(SALARY=recode(SALARY, `50000`= 1, `NA`=0))
demo$SALARY[is.na(demo$SALARY)]<-0
demo$SALARY



# In the all transactions data set, make sure you convert all currencies to naira