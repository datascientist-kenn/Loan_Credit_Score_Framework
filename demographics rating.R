library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)

demo <- fread(file='all_demographics.csv', header = TRUE, sep = "|", stringsAsFactors = FALSE)

## Selecting columns 
newDemo <- subset(demo, select = c(CUSTOMER_UNIQUE_ID, CUSTOMER_CATEGORY, MINOR, MARITAL_STATUS, DEPENDENT_OTHERS, SEX))

demographics <- newDemo

demographics <- demographics %>% mutate(CUSTOMER_CATEGORY=recode(CUSTOMER_CATEGORY, `C`= 10, `CORP`=10, `GOVT`=10, `HNI`=10, `I`=5, `IND`=7, `MINORS`=1, `RSA`=10, `S`=5, `SAL`=7, `STUDENTS`=4, `SUPERMARKT`=7, `TIER1`=10, `TIER2`=8, `WALKIN`=3))

demographics <- demographics %>% mutate(MINOR=recode(MINOR, `N`= 10, `Y`=1, `M`=1, `NA `=1))

demographics <- demographics %>% mutate(MARITAL_STATUS=recode(MARITAL_STATUS, `S`= 10, `P`=5, `M`=8, `E`=5, `D`=5, `NA`=1))

demographics <- demographics %>% mutate(DEPENDENT_OTHERS=recode(DEPENDENT_OTHERS, `0`= 10, `NA`=1))

demographics <- demographics %>% mutate(SEX=recode(SEX, `M`=7 , `F`=10, `P`=1, `NA `=1))

#colnames(demographics)[apply(demographics,1,anyNA)

#count(demographics, demographics$MARITAL_STATUS)
