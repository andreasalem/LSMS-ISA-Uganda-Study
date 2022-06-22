######

library(tidyverse)
library(dplyr)
library(haven) #used to open .dta files
########################################################################
########################################################################
####                                                                ####   
####                       NON - DURABLES 2009                      ####  
####                                                                ####   
########################################################################
########################################################################

getwd()
setwd("/Users/andreasalem/R_oba/Thesis")

options(scipen=999) # avoids scientific notation





# Non-Durable Goods and Frequently Purchased Services
GSEC15c_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15C.csv") # 2752  
names(GSEC15c_2009)[names(GSEC15c_2009) == "HHID"] <- "id_hh"

# files with household level information 
hh_2009 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2009.csv")                                # 2975 HH in 2009
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")       # 2157 in our analysis
 
# create final dataset for analysis
matched_hh_2009 <- filter(hh_2009, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

non_durable_2009 <- filter(GSEC15c_2009, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2009, non_durable_2009, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset09_non_durable <- as.data.frame(merged$id_hh)
names(final_dataset09_non_durable)[names(final_dataset09_non_durable) == "merged$id_hh"] <- "case_id"



final_dataset09_non_durable <- summarise(final_dataset09_non_durable,
                                         case_id = merged$id_hh,
                                         ea_id = merged$comm,
                                         wgt09 = merged$wgt09,
                                         urban  = merged$urban,
                                         regionXurban = merged$regurb,
                                         hh_g02 = merged$h15cq2,
                                         hh_g04a = merged$h15cq4,
                                         hh_g04b = merged$h15cq5,
                                         hh_g06a = merged$h15cq6,
                                         hh_g06b = merged$h15cq7,
                                         hh_g07a = merged$h15cq8,
                                         hh_g07b = merged$h15cq9,
                                         hh_g08 = merged$h15cq10)



nrow(as.data.frame(unique(final_dataset09_non_durable$case_id))) # 2157 households present in 15c

rm(non_durable_2009, final_vector_sample, GSEC15c_2009, matched_hh_2009, hh_2009, merged)

colSums(is.na(final_dataset09_non_durable))

final_dataset09_non_durable$hh_g04a <- 1
final_dataset09_non_durable$hh_g06a <- 1
final_dataset09_non_durable$hh_g07a <- 1

####################




##################
# a)        ###### How many and which items are there?
##################

final_dataset09_non_durable %>% 
  summarise(item = unique(hh_g02)) %>% arrange(item) # 39 items


##################
# 1)        ###### MEAN market price for each item code
##################

as.data.frame(final_dataset09_non_durable %>%                          
                group_by(hh_g02) %>%                                                 #  hh_g02 == item code
                summarise(avg_market_p = (mean(hh_g08, na.rm = TRUE))))              #  hh_g08 == market price

# but for the majority of items, no market price is reported. 
# To compute manually a "market price" for these items, we use variables on value ("hh_g04b", "hh_g06b", "hh_g07b"), sum them and 
# also sum with average market price computed above, and take the mean

price_list09_non_durable <- final_dataset09_non_durable %>% 
  group_by(hh_g02) %>% 
  summarise(mean_value_purch     = (mean(hh_g04b, na.rm = T)),
            mean_value_home_prod = (mean(hh_g06b, na.rm = T)),
            mean_value_gift      = (mean(hh_g07b, na.rm = T)),
            mean_market_p = (mean(hh_g08, na.rm = TRUE))) %>%    # this column is the same as the one created above
  rowwise() %>% 
  mutate(mean_market_price = mean(c(mean_value_purch, mean_value_home_prod, mean_value_gift, mean_market_p), na.rm = T)) %>% select(hh_g02,mean_market_price )


# add prices to dataset, but we use same market prices as in 2011
price_list11_non_durable <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_non_durable.csv")

final_dataset09_non_durable <- right_join(final_dataset09_non_durable, price_list11_non_durable, by = "hh_g02")   # replace price_list11_non_durable, with price_list09_non_durable



##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################


##################
# 2)        ###### frequency of occurrence of each item. E.g., item 452 (washing soap) is the one most occurring 
##################

final_dataset09_non_durable %>% 
  group_by(hh_g02) %>% 
  summarise(count = (count= n())) %>%
  arrange(desc(count))


##################
# 3)        ###### for each item, manually compute quantity consumed in each household 
##################


# We manually sum "hh_g04a", "hh_g06a", and "hh_g07a" 
final_dataset09_non_durable <- final_dataset09_non_durable %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = 1) # replace 1 with sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T)

# we note that there are many 0s, too many. Is this a problem?
final_dataset09_non_durable %>% 
  filter(tot_quantity_per_item_manual == 0) 


final_dataset09_non_durable %>% 
  filter(tot_quantity_per_item_manual == 0) %>% 
  select(hh_g02, tot_quantity_per_item_manual) %>% 
  group_by(hh_g02) %>% 
  summarise(c = (count = n())) %>% 
  arrange(desc(c)) 

# so, for example for item 302 (rent) if it's 0, we automatically put 1. This is justified becasue the variable h15cq2_1 in the original dataset (did you consume [ITEM] in the [RECALL]?) is always "Yes"

#final_dataset10_non_durable <- final_dataset10_non_durable %>% 
#  rowwise() %>% 
#  mutate(tot_quantity_per_item_manual = if_else((sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T) == 0 & hh_g02 == 302), 1, sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T)))   

# we do this for other items as well and repeat it for item 302

#final_dataset10_non_durable <- final_dataset10_non_durable %>% 
#  rowwise() %>% 
#   mutate(tot_quantity_per_item_manual = if_else((sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T) == 0 & ((hh_g02 == 302) | (hh_g02 == 455) | (hh_g02 == 305) | (hh_g02 == 467) | (hh_g02 == 502) | (hh_g02 == 604) | (hh_g02 == 465) )), 1, sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T))) 



##################
# 4)        ###### multiply quantity of item by price of item (MEAN market price)
##################

final_dataset09_non_durable <- final_dataset09_non_durable %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price)

# ---> we don't distinguish anymore between source of goods, so we omit the following lines

# purchas1_quantity_and_price  = hh_g04a*mean_market_price, 
# home_prod_quantity_and_price = hh_g06a*mean_market_price,
# gifts_quantity_and_price     = hh_g07a*mean_market_price)


##################
# 5)        ###### aggregate items into consumption per FOOD CATEGORY
##################


##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################



# for quantities, we should use sum of 4 variables:  hh_g04a, hh_g05a, hh_g06a, hh_g07a. But for durables, we donn't distinguish

#  b1 <- final_dataset10_non_durable %>%                                 
#   group_by(case_id, ea_id, wgt10, urban, regionXurban) %>%  
#   summarise(quantity_purchas1 = sum(hh_g04a, na.rm = T),
#             quantity_home_prod = sum(hh_g06a, na.rm = T),
#             quantity_gifts = sum(hh_g07a, na.rm = T),
#             quantity_tot = quantity_purchas1 + quantity_home_prod + quantity_gifts,
#             total_consumption = sum(tot_quantity_and_price, na.rm = T),
#             purchases = sum(purchas1_quantity_and_price, na.rm = T),
#             home_prod = sum(home_prod_quantity_and_price, na.rm = T),
#             gifts_and_other = sum(gifts_quantity_and_price, na.rm = T)) %>% 
#   mutate(perc_tot = total_consumption/total_consumption,
#          perc_purch = purchases/total_consumption,
#          perc_home_prod = home_prod/total_consumption,
#          perc_gift_and_other = gifts_and_other/total_consumption,
#          test = perc_purch + perc_home_prod + perc_gift_and_other) %>% 
#   as.data.frame() 


# alternative b1, where each item is consumer once and we can't diistinguih the origin of the item
b1_non_durables09 <- final_dataset09_non_durable %>%                                 
  group_by(case_id, ea_id, wgt09, urban, regionXurban) %>%  
  summarise(total_consumption_monthly = sum(tot_quantity_and_price, na.rm = T),
            total_consumption_weekly = total_consumption_monthly/4.345238) %>%
  as.data.frame() 

# tests for nr. of households
nrow(as.data.frame(unique(b1_non_durables09$case_id))) # 2393 

# total weekly consumption in sample
sum(b1_non_durables09$total_consumption_weekly)





# save household level non-durbable consumption in 2009 wave
write_csv(b1_non_durables09, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_non-durab.csv") 












