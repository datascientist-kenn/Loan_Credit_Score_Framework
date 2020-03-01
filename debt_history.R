library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)

bureau <- fread('bureau_score.csv')
count(bureau, bureau$BUREAU_SCORE)
count(bureau, bureau$BAL_NON_DELQ)
count(bureau, bureau$BAL_30DPD)
colnames(bureau)
unique(bureau$CUSTOMER_UNIQUE_ID)

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