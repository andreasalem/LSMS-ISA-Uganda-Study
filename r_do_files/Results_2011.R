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
######################### Output of this file is one data-sets "result_2011.csv",
######                    with household-level observations of weekly consumption and its sub-aggregates (food, non-durables, durables).
######
######
###### In this file, besides aggregating the do_items into result_2011, 3 implementations are made:
######
###### 1. creation of quintiles (with self-made function) and assign to each household a number from 1 (bottom) to 5 (top).
###### 2. application of per adult equivalence scale
######  2.1 mutate variable "n_ind_household" into "adult_equivalent", applying the following scale:
######        - age: 0-6,    weight: 0.3,
######        - age: 7-14,   weight: 0.4,
######        - age: 15-60,  weight: 1,
######        - age: 61-100, weight: 0.6,
######        - age: > 100,  weight: 0.3.
###### 3. three .csv files with prices of all items (foods, non-durables, durables). These .csv files will be imported in the other years

######################### Data required (and will be merged)


b1_food11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2011_food.csv") 
b1_durables11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2011_durables.csv") 
b1_non_durables11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2011_non-durab.csv") 





######################### 
# STEP 1



b1_food11 <- b1_food11 %>% select(c(1:9))   %>% arrange(case_id)

a <- left_join(b1_food11, b1_non_durables11) %>% 
  subset(select = -total_consumption_monthly) %>% 
  rename(tot_consump_weekly_non_durab = total_consumption_weekly)

a <- left_join(a, b1_durables11) %>% 
  subset(select = -total_consumption_annual) %>% 
  rename(tot_consump_weekly_durab = total_consumption_weekly)


result_2011 <- a %>% 
  mutate(TOT_consumption =  tot_consump_weekly_durab + tot_consump_weekly_non_durab + total_consumption_food,
         food_percentage = total_consumption_food*100/TOT_consumption,
         non_durable_percentage = tot_consump_weekly_non_durab*100/TOT_consumption,
         durable_percentage = tot_consump_weekly_durab*100/TOT_consumption,
         total_percentage_test = TOT_consumption*100/TOT_consumption)


rm(a)

## add number of individuals to roster dataset to compute consumption per capita

hh_roster11 <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2011.csv")

# number of adult equivalent in each household
individuals <- hh_roster11 %>% 
  group_by(id_hh, n_ind_household) %>%
  mutate(adult_equivalent = case_when(between(ind_age,0, 6) ~ 0.3,
                                      between(ind_age,7, 14) ~ 0.4,
                                      between(ind_age,15, 60) ~ 1,
                                      between(ind_age,61, 100) ~ 0.6)) %>% 
  group_by(id_hh) %>% 
  summarise(number_adults = sum(adult_equivalent, na.rm = T))


result_2011$number_adults <- individuals$number_adults
rm(individuals)


# STEP 1.1  -> add per capita consumption

result_2011$total_consumption_capita <- result_2011$TOT_consumption/result_2011$number_adults
result_2011$total_consumption_food_per_capita <- result_2011$total_consumption_food/result_2011$number_adults
result_2011$total_consumption_non_durab_per_capita <- result_2011$tot_consump_weekly_non_durab/result_2011$number_adults
result_2011$total_consumption_durab_per_capita <- result_2011$tot_consump_weekly_durab/result_2011$number_adults


# STEP 1.2  -> divide in quantiles!


to_split <-  result_2011$TOT_consumption
threshold <- quantile(result_2011$TOT_consumption, c(0.2,0.4,0.6,0.8,1), na.rm = T) # 5 quantiles 

collect <- vector(mode = "list", length = length(to_split))
x=1
for (i in to_split) {
  collect[[x]] <- which.min(abs(threshold - i))[[1]]
  x=x+1
}


result_2011 <- result_2011

result_2011$category_quantile <- collect
rm(to_split, threshold, collect)
result_2011$category_quantile <- as.numeric(unlist(result_2011$category_quantile))

##################################################################################### 
write_csv(result_2011, "/Users/andreasalem/R_oba/Thesis/Data_clean/result_2011.csv")     # result_2011.csv

            
rm(b1_durables11,b1_food11, b1_non_durables11, hh_roster11, x, i)                   


