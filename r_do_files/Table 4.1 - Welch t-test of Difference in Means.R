######################### 
#
#
#
######################### 
######################### Table of Contents

###### --- TABLE 4.1...........Welch t-test of Difference in Means


######################### Data required 

b1_food09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_food.csv") 
b1_food10 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2010_food.csv") 
b1_food11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/items_2011_food.csv") 



######################### Code
getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
library(Hmisc) #used for weighted mean and standard deviation

options(scipen=999) # avoids scientific notation


################################################################### 
################################################################### TABLE 4.1 


##################
################## 2009/10
################## 
################################################################### Analysis of food sources 2009. Output ---> table_2009_food 
###################################################################

mean_purchases <- wtd.mean(b1_food09$perc_purch, b1_food09$wgt09, na.rm = T)
mean_home_prod  <- wtd.mean(b1_food09$perc_home_prod, b1_food09$wgt09, na.rm = T)
mean_other  <- wtd.mean(b1_food09$perc_gift_and_other, b1_food09$wgt09, na.rm = T)

sum(mean_purchases, mean_home_prod, mean_other) # check. = 1 OK!

##### now we do the same put divide in urban vs. rural

mean_purchases_urban <- wtd.mean(filter(b1_food09, urban == 1)$perc_purch, filter(b1_food09, urban == 1)$wgt09, na.rm = T)
mean_purchases_rural <- wtd.mean(filter(b1_food09, urban == 0)$perc_purch, filter(b1_food09, urban == 0)$wgt09, na.rm = T)


mean_home_prod_urban <- wtd.mean(filter(b1_food09, urban == 1)$perc_home_prod, filter(b1_food09, urban == 1)$wgt09, na.rm = T)
mean_home_prod_rural <- wtd.mean(filter(b1_food09, urban == 0)$perc_home_prod, filter(b1_food09, urban == 0)$wgt09, na.rm = T)

mean_other_urban <- wtd.mean(filter(b1_food09, urban == 1)$perc_gift_and_other, filter(b1_food09, urban == 1)$wgt09, na.rm = T)
mean_other_rural <- wtd.mean(filter(b1_food09, urban == 0)$perc_gift_and_other, filter(b1_food09, urban == 0)$wgt09, na.rm = T)


sum(mean_purchases_urban, mean_home_prod_urban, mean_other_urban)  # check =1 OK!
sum(mean_purchases_rural, mean_home_prod_rural, mean_other_rural)  # check =1 OK!


### create table with results
Category <- c("purchases", "home production", "gifts&other")

percentage_urban <- c(mean_purchases_urban*100, mean_home_prod_urban*100, mean_other_urban*100)
df_urban <- data.frame(Category, percentage_urban)

percentage_rural <- c(mean_purchases_rural*100, mean_home_prod_rural*100, mean_other_rural*100)
df_rural <- data.frame(Category, percentage_rural)


table_2009_food <- merge(df_urban, df_rural)

rm(Category, percentage_urban, df_urban, percentage_rural, df_rural)

##### finally, we perform some t-tests to examine if means are stat different for urban vs. rural for each category. # we reject null (that means are the same) if the t-statistic is larger than 1.96

# purchases
t.test(filter(b1_food09, urban == 1)$purchases,    filter(b1_food09, urban == 0)$purchases, alternative = "two.sided", var.equal = FALSE)

# home_prod
t.test(filter(b1_food09, urban == 1)$home_prod,    filter(b1_food09, urban == 0)$home_prod, alternative = "two.sided", var.equal = FALSE)

# gifts_and_other (not statisically different)
t.test(filter(b1_food09, urban == 1)$gifts_and_other,    filter(b1_food09, urban == 0)$gifts_and_other, alternative = "two.sided", var.equal = FALSE)


##################
################## 2010/11
################## 
################################################################### Analysis of food sources 2010. Output ---> table_2010_food 
###################################################################

mean_purchases <- wtd.mean(b1_food10$perc_purch, b1_food10$wgt10, na.rm = T)
mean_home_prod  <- wtd.mean(b1_food10$perc_home_prod, b1_food10$wgt10, na.rm = T)
mean_other  <- wtd.mean(b1_food10$perc_gift_and_other, b1_food10$wgt10, na.rm = T)

sum(mean_purchases, mean_home_prod, mean_other) # check = 1 OK!

### now we do the same put divide in urban vs. rural

mean_purchases_urban <- wtd.mean(filter(b1_food10, urban == 1)$perc_purch, filter(b1_food10, urban == 1)$wgt10, na.rm = T)
mean_purchases_rural <- wtd.mean(filter(b1_food10, urban == 0)$perc_purch, filter(b1_food10, urban == 0)$wgt10, na.rm = T)


mean_home_prod_urban <- wtd.mean(filter(b1_food10, urban == 1)$perc_home_prod, filter(b1_food10, urban == 1)$wgt10, na.rm = T)
mean_home_prod_rural <- wtd.mean(filter(b1_food10, urban == 0)$perc_home_prod, filter(b1_food10, urban == 0)$wgt10, na.rm = T)

mean_other_urban <- wtd.mean(filter(b1_food10, urban == 1)$perc_gift_and_other, filter(b1_food10, urban == 1)$wgt10, na.rm = T)
mean_other_rural <- wtd.mean(filter(b1_food10, urban == 0)$perc_gift_and_other, filter(b1_food10, urban == 0)$wgt10, na.rm = T)


sum(mean_purchases_urban, mean_home_prod_urban, mean_other_urban)  # check =1 OK!
sum(mean_purchases_rural, mean_home_prod_rural, mean_other_rural)  # check =1 OK!


### we create table with the results
Category <- c("purchases", "home production", "gifts&other")

percentage_urban <- c(mean_purchases_urban*100, mean_home_prod_urban*100, mean_other_urban*100)
df_urban <- data.frame(Category, percentage_urban)

percentage_rural <- c(mean_purchases_rural*100, mean_home_prod_rural*100, mean_other_rural*100)
df_rural <- data.frame(Category, percentage_rural)


table_2010_food <- merge(df_urban, df_rural)

rm(Category, percentage_urban, df_urban, percentage_rural, df_rural)

##### finally, we perform some t-tests to examine if means are stat different for urban vs. rural for each category

# purchases
t.test(filter(b1_food10, urban == 1)$purchases_food,    filter(b1_food10, urban == 0)$purchases_food, alternative = "two.sided", var.equal = FALSE)

# home_prod
t.test(filter(b1_food10, urban == 1)$home_prod_food,    filter(b1_food10, urban == 0)$home_prod_food, alternative = "two.sided", var.equal = FALSE)

# gifts_and_other
t.test(filter(b1_food10, urban == 1)$gifts_and_other_food,    filter(b1_food10, urban == 0)$gifts_and_other_food, alternative = "two.sided", var.equal = FALSE)



##################
################## 2011/12
################## 
################################################################### Analysis of food sources 2011 Output ---> table_2011_food 
###################################################################

mean_purchases <- wtd.mean(b1_food11$perc_purch, b1_food11$wgt11, na.rm = T)
mean_home_prod  <- wtd.mean(b1_food11$perc_home_prod, b1_food11$wgt11, na.rm = T)
mean_other  <- wtd.mean(b1_food11$perc_gift_and_other, b1_food11$wgt11, na.rm = T)

sum(mean_purchases, mean_home_prod, mean_other) # check. = 1 OK!

### now we do the same put divide in urban vs. rural

mean_purchases_urban <- wtd.mean(filter(b1_food11, urban == 1)$perc_purch, filter(b1_food11, urban == 1)$wgt11, na.rm = T)
mean_purchases_rural <- wtd.mean(filter(b1_food11, urban == 0)$perc_purch, filter(b1_food11, urban == 0)$wgt11, na.rm = T)


mean_home_prod_urban <- wtd.mean(filter(b1_food11, urban == 1)$perc_home_prod, filter(b1_food11, urban == 1)$wgt11, na.rm = T)
mean_home_prod_rural <- wtd.mean(filter(b1_food11, urban == 0)$perc_home_prod, filter(b1_food11, urban == 0)$wgt11, na.rm = T)

mean_other_urban <- wtd.mean(filter(b1_food11, urban == 1)$perc_gift_and_other, filter(b1_food11, urban == 1)$wgt11, na.rm = T)
mean_other_rural <- wtd.mean(filter(b1_food11, urban == 0)$perc_gift_and_other, filter(b1_food11, urban == 0)$wgt11, na.rm = T)


sum(mean_purchases_urban, mean_home_prod_urban, mean_other_urban)  # check =1 OK!
sum(mean_purchases_rural, mean_home_prod_rural, mean_other_rural)  # check =1 OK!


### we create table with the results
Category <- c("purchases", "home production", "gifts&other")

percentage_urban <- c(mean_purchases_urban*100, mean_home_prod_urban*100, mean_other_urban*100)
df_urban <- data.frame(Category, percentage_urban)

percentage_rural <- c(mean_purchases_rural*100, mean_home_prod_rural*100, mean_other_rural*100)
df_rural <- data.frame(Category, percentage_rural)


table_2011_food <- merge(df_urban, df_rural)

rm(Category, percentage_urban, df_urban, percentage_rural, df_rural)

##### finally, we perform some t-tests to examine if means are stat different for urban vs. rural for each category

# purchases (not statistically different. Strange!!)
t.test(filter(b1_food11, urban == 1)$purchases,    filter(b1_food11, urban == 0)$purchases, alternative = "two.sided", var.equal = FALSE) #  but if we use "perc_purch" they indeed are statistically different

# home_prod
t.test(filter(b1_food11, urban == 1)$home_prod,    filter(b1_food11, urban == 0)$home_prod, alternative = "two.sided", var.equal = FALSE)

# gifts_and_other
t.test(filter(b1_food11, urban == 1)$gifts_and_other,    filter(b1_food11, urban == 0)$gifts_and_other, alternative = "two.sided", var.equal = FALSE)

