########################################################################
########################################################################
##                                                                    ##       
##               National Panel Survey 2010-2011                      ##       
##                                                                    ##       
########################################################################
########################################################################
getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
library(Hmisc) #used for weighted mean and standard deviation

options(scipen=999) # avoids scientific notation



##################################################################################### Analysis


result_2010 <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2010.csv")


##################################################################################### 

# STEP 2  -> table with means

result_2010_means <- setNames(data.frame(matrix(ncol = 5, nrow = 3)), c("Panel", "total_consumption_percapita", "food_consumption_percapita", "non_durable_consumption_percapita", "durable_consumption_percapita"))



result_2010_means$Panel <- c("A: full sample", "B: rural areas", "C: urban areas")

result_2010_means$total_consumption_percapita <- c(
  wtd.mean(result_2010$total_consumption_capita, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$total_consumption_capita, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$total_consumption_capita, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

result_2010_means$food_consumption_percapita <- c(
  wtd.mean(result_2010$total_consumption_food_per_capita, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$total_consumption_food_per_capita, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$total_consumption_food_per_capita, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

result_2010_means$non_durable_consumption_percapita<- c(
  wtd.mean(result_2010$total_consumption_non_durab_per_capita, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$total_consumption_non_durab_per_capita, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$total_consumption_non_durab_per_capita, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

result_2010_means$durable_consumption_percapita<- c(
  wtd.mean(result_2010$total_consumption_durab_per_capita, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$total_consumption_durab_per_capita, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$total_consumption_durab_per_capita, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

nrow(filter(result_2010, urban == 1))
nrow(filter(result_2010, urban == 0))

# STEP 3  -> table with percentage values

result_2010_percentages <- setNames(data.frame(matrix(ncol = 5, nrow = 3)), c("Panel", "total_consumption_percapita", "food_consumption_percapita", "non_durable_consumption_percapita", "durable_consumption_percapita"))



result_2010_percentages$Panel <- c("A: full sample (%)", "B: rural areas (%)", "C: urban areas (%)")

result_2010_percentages$total_consumption_percapita <- c(
  wtd.mean(result_2010$total_percentage_test, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$total_percentage_test, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$total_percentage_test, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

result_2010_percentages$food_consumption_percapita <- c(
  wtd.mean(result_2010$food_percentage, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$food_percentage, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$food_percentage, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

result_2010_percentages$non_durable_consumption_percapita<- c(
  wtd.mean(result_2010$non_durable_percentage, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$non_durable_percentage, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$non_durable_percentage, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)

result_2010_percentages$durable_consumption_percapita<- c(
  wtd.mean(result_2010$durable_percentage, result_2010$wgt10, na.rm = T), 
  wtd.mean(filter(result_2010, urban == 0)$durable_percentage, filter(result_2010, urban == 0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, urban == 1)$durable_percentage, filter(result_2010, urban == 1)$wgt10, na.rm = T)
)


# STEP 4  -> add same info but by quartiles

result_2010_quantiles <- setNames(data.frame(matrix(ncol = 5, nrow = 5)), c("Panel", "total_consumption_percapita", "food_consumption_percapita", "non_durable_consumption_percapita", "durable_consumption_percapita"))



result_2010_quantiles$Panel <- c("Quintile 1 (poorest 20%)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (richest 20%)")

result_2010_quantiles$total_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1)$total_consumption_capita, filter(result_2010, category_quantile == 1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2)$total_consumption_capita, filter(result_2010, category_quantile == 2)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3)$total_consumption_capita, filter(result_2010, category_quantile == 3)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4)$total_consumption_capita, filter(result_2010, category_quantile == 4)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5)$total_consumption_capita, filter(result_2010, category_quantile == 5)$wgt10, na.rm = T))


result_2010_quantiles$food_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 2)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 3)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 4)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 5)$wgt10, na.rm = T))

result_2010_quantiles$non_durable_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 2)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 3)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 4)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 5)$wgt10, na.rm = T))

result_2010_quantiles$durable_consumption_percapita<- c(
  wtd.mean(filter(result_2010, category_quantile == 1)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 2)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 3)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 4)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 5)$wgt10, na.rm = T))


result_2010_table <- as.data.frame(rbind(result_2010_means, result_2010_percentages))
result_2010_table <- as.data.frame(rbind(result_2010_table, result_2010_quantiles))

result_2010_table$non_food_per_capita <- result_2010_table$non_durable_consumption_percapita + result_2010_table$durable_consumption_percapita

result_2010$non_food_per_capita <- result_2010$total_consumption_durab_per_capita + result_2010$total_consumption_non_durab_per_capita







rm(result_2010_means, result_2010_percentages, result_2010_quantiles)

# gg <- result_2010_table %>% select(Panel, food_consumption_percapita, non_food_per_capita)

######## quintiles rural
result_2010_quantiles <- setNames(data.frame(matrix(ncol = 6, nrow = 5)), c("Panel", "total_consumption_percapita", "food_consumption_percapita", "non_durable_consumption_percapita", "durable_consumption_percapita", "non_food_per_capita"))



result_2010_quantiles$Panel <- c("Quintile 1 (poorest 20%) rural ", "Quintile 2 rural ", "Quintile 3 rural ", "Quintile 4 rural ", "Quintile 5 (richest 20%) rural")

result_2010_quantiles$total_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban == 0)$total_consumption_capita, filter(result_2010, category_quantile == 1 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==0)$total_consumption_capita, filter(result_2010, category_quantile == 2 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==0)$total_consumption_capita, filter(result_2010, category_quantile == 3 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==0)$total_consumption_capita, filter(result_2010, category_quantile == 4 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==0)$total_consumption_capita, filter(result_2010, category_quantile == 5 & urban==0)$wgt10, na.rm = T))


result_2010_quantiles$food_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==0)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 1 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==0)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 2 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==0)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 3 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==0)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 4 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==0)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 5 & urban==0)$wgt10, na.rm = T))

result_2010_quantiles$non_durable_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==0)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 1 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==0)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 2 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==0)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 3 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==0)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 4 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==0)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 5 & urban==0)$wgt10, na.rm = T))

result_2010_quantiles$durable_consumption_percapita<- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==0)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 1 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==0)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 2 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==0)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 3 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==0)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 4 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==0)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 5 & urban==0)$wgt10, na.rm = T))

result_2010_quantiles$non_food_per_capita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==0)$non_food_per_capita, filter(result_2010, category_quantile == 1 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==0)$non_food_per_capita, filter(result_2010, category_quantile == 2 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==0)$non_food_per_capita, filter(result_2010, category_quantile == 3 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==0)$non_food_per_capita, filter(result_2010, category_quantile == 4 & urban==0)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==0)$non_food_per_capita, filter(result_2010, category_quantile == 5 & urban==0)$wgt10, na.rm = T))

result_2010_table <- as.data.frame(rbind(result_2010_table, result_2010_quantiles))



######## quintiles urban
result_2010_quantiles <- setNames(data.frame(matrix(ncol = 6, nrow = 5)), c("Panel", "total_consumption_percapita", "food_consumption_percapita", "non_durable_consumption_percapita", "durable_consumption_percapita", "non_food_per_capita"))



result_2010_quantiles$Panel <- c("Quintile 1 (poorest 20%) urban ", "Quintile 2 urban ", "Quintile 3 urban ", "Quintile 4 urban ", "Quintile 5 (richest 20%) urban")

result_2010_quantiles$total_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==1)$total_consumption_capita, filter(result_2010, category_quantile == 1 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==1)$total_consumption_capita, filter(result_2010, category_quantile == 2 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==1)$total_consumption_capita, filter(result_2010, category_quantile == 3 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==1)$total_consumption_capita, filter(result_2010, category_quantile == 4 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==1)$total_consumption_capita, filter(result_2010, category_quantile == 5 & urban==1)$wgt10, na.rm = T))


result_2010_quantiles$food_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==1)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 1 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==1)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 2 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==1)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 3 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==1)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 4 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==1)$total_consumption_food_per_capita, filter(result_2010, category_quantile == 5 & urban==1)$wgt10, na.rm = T))

result_2010_quantiles$non_durable_consumption_percapita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==1)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 1 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==1)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 2 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==1)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 3 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==1)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 4 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==1)$total_consumption_non_durab_per_capita, filter(result_2010, category_quantile == 5 & urban==1)$wgt10, na.rm = T))

result_2010_quantiles$durable_consumption_percapita<- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==1)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 1 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==1)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 2 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==1)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 3 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==1)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 4 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==1)$total_consumption_durab_per_capita, filter(result_2010, category_quantile == 5 & urban==1)$wgt10, na.rm = T))

result_2010_quantiles$non_food_per_capita <- c(
  wtd.mean(filter(result_2010, category_quantile == 1 & urban==1)$non_food_per_capita, filter(result_2010, category_quantile == 1 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 2 & urban==1)$non_food_per_capita, filter(result_2010, category_quantile == 2 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 3 & urban==1)$non_food_per_capita, filter(result_2010, category_quantile == 3 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 4 & urban==1)$non_food_per_capita, filter(result_2010, category_quantile == 4 & urban==1)$wgt10, na.rm = T),
  wtd.mean(filter(result_2010, category_quantile == 5 & urban==1)$non_food_per_capita, filter(result_2010, category_quantile == 5 & urban==1)$wgt10, na.rm = T))


##########
result_2010_table <- as.data.frame(rbind(result_2010_table, result_2010_quantiles))
rm(result_2010_quantiles)
##########

result_2010_table$share_non_food <- result_2010_table$non_food_per_capita*100/result_2010_table$total_consumption_percapita
result_2010_table$share_food <-result_2010_table$food_consumption_percapita*100/result_2010_table$total_consumption_percapita

# result_2010_table[12:21,c(1,7:8)] %>% view()



#### save analytical results in .csv file!
write.csv(result_2010_table, "/Users/andreasalem/R_oba/Thesis/Data_clean/result_2010_table.csv")












