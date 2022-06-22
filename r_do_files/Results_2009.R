######################### 
######################### Code
getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
library(Hmisc) #used for weighted mean and standard deviation

options(scipen=999) # avoids scientific notation
######################### 
#########################
#
#
#
######################### 
######################### 
######################### Output of this file is one data-sets "result_2009.csv",
######                    with household-level observations of weekly consumption and its sub-aggregates (food, non-durables, durables).
######
######
###### In this file, besides aggregating the do_items into result_2009, two implementations are made:
######
###### 1. creation of quintiles (with self-made function) and assign to each household a number from 1 (bottom) to 5 (top).
###### 2. application of per adult equivalence scale
######  2.1 mutate variable "n_ind_household" into "adult_equivalent", applying the following scale:
######        - age: 0-6,    weight: 0.3,
######        - age: 7-14,   weight: 0.4,
######        - age: 15-60,  weight: 1,
######        - age: 61-100, weight: 0.6,
######        - age: > 100,  weight: 0.3.

######################### Data required (and will be merged)



b1_food09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_food.csv") 
b1_durables09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_durables.csv") 
b1_non_durables09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_non-durab.csv") 



######################### 
# STEP 1

b1_food09 <- b1_food09 %>% select(c(1:9))   %>% arrange(case_id)  


a <- left_join(b1_food09, b1_non_durables09) %>% 
  subset(select = -total_consumption_monthly) %>% 
  rename(tot_consump_weekly_non_durab = total_consumption_weekly)

a <- left_join(a, b1_durables09) %>% 
  subset(select = -total_consumption_annual) %>% 
  rename(tot_consump_weekly_durab = total_consumption_weekly)


result_2009 <- a[1:2157,] %>% 
  mutate(TOT_consumption =  tot_consump_weekly_durab + tot_consump_weekly_non_durab + total_consumption_food,
         food_percentage = total_consumption_food*100/TOT_consumption,
         non_durable_percentage = tot_consump_weekly_non_durab*100/TOT_consumption,
         durable_percentage = tot_consump_weekly_durab*100/TOT_consumption,
         total_percentage_test = TOT_consumption*100/TOT_consumption)
rm(a)

## add number of individuals to compute consumption per capita

hh_roster09 <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2009.csv")

# number of adult equivalent in each household
individuals <- hh_roster09 %>% 
  group_by(id_hh, n_ind_household) %>%
  mutate(adult_equivalent = case_when(between(ind_age,0, 6) ~ 0.3,
                                      between(ind_age,7, 14) ~ 0.4,
                                      between(ind_age,15, 60) ~ 1,
                                      between(ind_age,61, 100) ~ 0.6)) %>% 
  group_by(id_hh) %>% 
  summarise(number_adults = sum(adult_equivalent, na.rm = T))


result_2009$number_adults <- individuals$number_adults
rm(individuals)

# STEP 1.1  -> add per capita consumption

result_2009$total_consumption_capita <- result_2009$TOT_consumption/result_2009$number_adults
result_2009$total_consumption_food_per_capita <- result_2009$total_consumption_food/result_2009$number_adults
result_2009$total_consumption_non_durab_per_capita <- result_2009$tot_consump_weekly_non_durab/result_2009$number_adults
result_2009$total_consumption_durab_per_capita <- result_2009$tot_consump_weekly_durab/result_2009$number_adults


### STEP 1.2  -> divide in quantiles!

#  we assign to thee problematic household in 2009 the same values as household 1021000203, selected randomly
result_2009[2068,] <- c(113100030302, filter(select(result_2009, -c("case_id")), result_2009$case_id == 1021000203))




to_split <- result_2009$TOT_consumption
threshold <- quantile(result_2009$TOT_consumption, c(0.2,0.4,0.6,0.8,1), na.rm = T) # 5 quantiles 

collect <- vector(mode = "list", length = length(to_split))
x=1
for (i in to_split) {
  
  # print(paste("ITERATION NUMBER", x, "!!!!!!"))
  
  collect[[x]] <- which.min(abs(threshold - i))[[1]]
  
  # print(collect[[x]])
  
  x=x+1
}



result_2009$category_quantile <- collect

rm(to_split, threshold, collect)

result_2009$category_quantile <- as.data.frame(as.numeric(append(unlist(result_2009$category_quantile), 5)))[1:2157,]
  
 



##################################################################################### 
write_csv(result_2009, "/Users/andreasalem/R_oba/Thesis/Data_clean/result_2009.csv")


rm(b1_durables09,b1_food09, b1_non_durables09, hh_roster09, x, i)                 






