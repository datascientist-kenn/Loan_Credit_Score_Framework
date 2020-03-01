library(plyr)
library(dplyr)
library(data.table)
library(corrplot)

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




max(Credit$loanHistory)

unique(Credit$history)
