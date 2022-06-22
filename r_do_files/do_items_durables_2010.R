######

library(tidyverse)
library(dplyr)

########################################################################
########################################################################
####                                                                ####   
####                       DURABLES 2010                            ####  
####                                                                ####   
########################################################################
########################################################################
######
# case_id      = Unique HH Identifier
# ea_id        = Unique EA Itentifier
# wgt10        = Household Sampling Weight
# urban        = urban (=1) vs. rural (=0)?
# regionXurban = urbanXregion

# hh_g02       = Item Code

# hh_g03a      = How much [ITEM] in total did your household consume in total in the past week (Quantity)?
# hh_g03b      = How much [ITEM] in total did your household consume in total in the past week (Value)?

# hh_g04a      = How much came from purchases (Quantity)?
# hh_g04b      = How much came from purchases (Value)?


# hh_g06a      = How much came from home-production (Quantity)?
# hh_g06b      = How much came from home-production (Value)?

# hh_g07a      = How much came from gifts/in-kind sources (Quantity)?
# hh_g07b      = How much came from gifts/in-kind sources (Value)?

# hh_g08       = Market Price


getwd()
setwd("/Users/andreasalem/R_oba/Thesis")

options(scipen=999) # avoids scientific notation



# Semi-Durable Goods and Durable Goods and Service
GSEC15d_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15d.csv")  # 2511 
names(GSEC15d_2010)[names(GSEC15d_2010) == "hh"] <- "id_hh"

# files with household level information 
hh_2010 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2010.csv")                                # 2716 HH in 2010
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")       # 2167 in our analysis

# create final dataset for analysis
matched_hh_2010 <- filter(hh_2010, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

durable_2010 <- filter(GSEC15d_2010, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2010, durable_2010, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset10_durable <- as.data.frame(merged$id_hh)
names(final_dataset10_durable)[names(final_dataset10_durable) == "merged$id_hh"] <- "case_id"


final_dataset10_durable <- summarise(final_dataset10_durable,
                                         case_id = merged$id_hh,
                                         ea_id = merged$comm,
                                         wgt10 = merged$wgt10,
                                         urban  = merged$urban,
                                         regionXurban = merged$regurb,
                                         hh_g02 = merged$h15dq2,
                                         hh_g03a = merged$h15dq10_1,
                                         hh_g03b = merged$h15dq10_2,
                                         hh_g04a = merged$h15dq4,
                                         hh_g04b = merged$h15dq5,
                                         hh_g06a = merged$h15dq6,
                                         hh_g06b = merged$h15dq7,
                                         hh_g07a = merged$h15dq8,
                                         hh_g07b = merged$h15dq9,
                                         hh_g08 = merged$h15dq10)



nrow(as.data.frame(unique(final_dataset10_durable$case_id))) # 2157

rm(durable_2010, final_vector_sample, GSEC15d_2010, matched_hh_2010, hh_2010, merged)


final_dataset10_durable$hh_g04a <- 1  # all values in hh_g04a that are not NA, are TRUE, so we assign 1 for quantity 
final_dataset10_durable$hh_g06a <- 1  # all values in hh_g06a that are not NA, are TRUE, so we assign 1 for quantity 
final_dataset10_durable$hh_g07a <- 1  # all values in hh_g07a that are not NA, are TRUE, so we assign 1 for quantity 

# the variable h15cq2_1 in the original dataset (did you consume [ITEM] in the [RECALL]?) is always "Yes", so what we do is justified
####################

colSums(is.na(final_dataset10_durable))


##################
# a)        ###### How many and which items are there?
##################

final_dataset10_durable %>% 
  summarise(item = unique(hh_g02)) %>% arrange(item) # 36 items



##################
# 1)        ###### MEAN market price for each item code
##################

as.data.frame(final_dataset10_durable %>%                          
                group_by(hh_g02) %>%                                                 #  hh_g02 == item code
                summarise(avg_market_p = (mean(hh_g08, na.rm = TRUE))))              #  hh_g08 == market price

filter(final_dataset10_durable, hh_g02 == 701) # outlier ???????

# but for the majority of items, no market price is reported. 
# To compute manually a "market price" for these items, we use variables on value ("hh_g04b", "hh_g06b", "hh_g07b"), sum them and 
# also sum with average market price computed above, and take the mean

price_list10_durable <- final_dataset10_durable %>% 
  group_by(hh_g02) %>% 
  summarise(mean_value_purch     = (mean(hh_g04b, na.rm = T)),
            mean_value_home_prod = (mean(hh_g06b, na.rm = T)),
            mean_value_gift      = (mean(hh_g07b, na.rm = T)),
            mean_market_p = (mean(hh_g08, na.rm = TRUE))) %>%    # this column is the same as the one created above
  rowwise() %>% 
  mutate(mean_market_price = mean(c(mean_value_purch, mean_value_home_prod, mean_value_gift, mean_market_p), na.rm = T)) %>% select(hh_g02,mean_market_price)


# add prices to dataset, but we use same market prices as in 2011
price_list11_durable <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_durable.csv")

final_dataset10_durable <- right_join(final_dataset10_durable, price_list11_durable, by = "hh_g02") # # replace price_list11_durable, with price_list10_durable if want to use 2010 prices



##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################


##################
# 2)        ###### frequency of occurrence of each item. E.g., item 604 (Books and supplies) is the one most occurring 
##################

final_dataset10_durable %>% 
  group_by(hh_g02) %>% 
  summarise(count = (count= n())) %>%
  arrange(desc(count))


##################
# 3)        ###### for each item, manually compute quantity consumed in each household 
##################

# Variable "hh_g03a" is part of the questionnaire and contains this info. Here we want to do it manually by addition of variables "hh_g04a", "hh_g05a", "hh_g06a", and "hh_g07a" 
final_dataset10_durable$hh_g03a # almost all NAs


# We manually sum "hh_g04a", "hh_g06a", and "hh_g07a" 
final_dataset10_durable <- final_dataset10_durable %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = 1) # replace 1 with sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T)


##################
# 4)        ###### multiply quantity of item by price of item (MEAN market price)
##################

final_dataset10_durable <- final_dataset10_durable %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price)
     
# ---> we don't distinguish anymore between source of goods, so we omit the following lines

# purchas1_quantity_and_price  = hh_g04a*mean_market_price, 
# home_prod_quantity_and_price = hh_g06a*mean_market_price,
# gifts_quantity_and_price     = hh_g07a*mean_market_price)



##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################


# for quantities, we should use sum of 4 variables:  hh_g04a, hh_g06a, hh_g07a. But for durables, we don't distinguish between sources


# alternative b1, where each item is consumer once and we can't diistinguih the origin of the item
b1_durables10 <- final_dataset10_durable %>%                                 
  group_by(case_id, ea_id, wgt10, urban, regionXurban) %>%  
  summarise(total_consumption_annual = sum(tot_quantity_and_price, na.rm = T),
            total_consumption_weekly = total_consumption_annual/52.14286) %>%
  as.data.frame() 

# tests for nr. of households
nrow(as.data.frame(unique(b1_durables10$case_id))) # 2157


# total weekly consumption in sample
sum(b1_durables10$total_consumption_weekly)
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")
b1_durables10 <- filter(b1_durables10, case_id %in% final_vector_sample$id_hh) %>% arrange(case_id)
rm(final_vector_sample)







# save household level durable consumption in 2010 wave
write_csv(b1_durables10, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2010_durables.csv") 









