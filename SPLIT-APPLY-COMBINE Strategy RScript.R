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

#customer unique id as a string
bureau$CUSTOMER_UNIQUE_ID <- sprintf("%f", bureau$CUSTOMER_UNIQUE_ID)

debt_history <- bureau%>%
  group_by(CUSTOMER_UNIQUE_ID) %>%
  summarise(total30DPD=sum(BAL_30DPD), total60DPD=sum(BAL_60DPD), total90DPD=sum(BAL_PL_90DPD))

debt_history$total_debt <- debt_history$total30DPD + debt_history$total60DPD + debt_history$total90DPD
