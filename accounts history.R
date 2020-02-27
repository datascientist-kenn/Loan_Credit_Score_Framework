

accounts <- fread('all_accounts.csv')

count(accounts, CCY)

library(sqldf)
library(DBI)
library(dbplyr)
#UPDATE 'accounts'
#SET 'ACY_AVL_BAL'=values*470
#WHERE 'CCY'='GBP';

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
accounts %>% 
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarize(totalBAL = sum(newAVL_BAL))
