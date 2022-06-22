#
##                                                                   
##
########################################################################
########################################################################
##                                                                    ##       
##                food_household_items_2011.R                         ##       
##                                                                    ##       
########################################################################
########################################################################

######
# case_id      = Unique HH Identifier
# ea_id        = Unique EA Itentifier
# wgt11       = Household Sampling Weight
# urban        = urban (=1) vs. rural (=0)?
# regionXurban = urbanXregion
# hh_g01       = Consumed Item?
# hh_g02       = Item Code
# hh_g02_os    = Item (Other Specify - Consumption Category code)
# hh_g02_os1   = Item (Other Specify - Consumption Category label (character))

# hh_g03a      = How much [ITEM] in total did your household consume in total in the past week (Quantity)?
# hh_g03b      = How much [ITEM] in total did your household consume in total in the past week (Value)?

# hh_g04a      = How much came from purchases consumed at home (Quantity)?
# hh_g04b      = How much came from purchases consumed at home (Value)?
# hh_g05a      = How much came from purchases away from home (Quantity)?
# hh_g05b      = How much came from purchases away from home (Value)?

# hh_g06a      = How much came from home-production (Quantity)?
# hh_g06b      = How much came from home-production (Value)?

# hh_g07a      = How much came from gifts/in-kind sources (Quantity)?
# hh_g07b      = How much came from gifts/in-kind sources (Value)?

# hh_g08       = Market Price
# hh_g09       = Farm Gate Price
#
#
# 
#
#
#
#
#
#
#
######

library(tidyverse)
library(dplyr)
library(haven) #used to open .dta files



########################################################################
########################################################################

getwd()
setwd("/Users/andreasalem/R_oba/Thesis")

options(scipen=999) # avoids scientific notation

#### NOW we work with 2011/12

# food consumption file
GSEC15B_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15B.dta") # 2830
names(GSEC15B_2011)[names(GSEC15B_2011) == "HHID"] <- "id_hh"

# files with household level information 
hh_2011 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2011.csv")                                # 2850 HH in 2011
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv") # 2402 in our analysis

# create final dataset for analysis
matched_hh_2011 <- filter(hh_2011, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

food_2011 <- filter(GSEC15B_2011, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2011, food_2011, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset11_food <- as.data.frame(merged$id_hh)
names(final_dataset11_food)[names(final_dataset11_food) == "merged$id_hh"] <- "case_id"


final_dataset11_food <- summarise(final_dataset11_food,
                             case_id = merged$id_hh,
                             ea_id = merged$comm,
                             wgt11 = merged$wgt11,
                             urban  = merged$urban,
                             regionXurban = merged$regurb,
                             hh_g01 = merged$h15bq3a,
                             hh_g02 = merged$itmcd,
                             hh_g02_os = merged$h15bq2c,
                             hh_g02_os1 = merged$h15bq2d,
                             hh_g03a = merged$h15bq14,
                             hh_g03b = merged$h15bq15,
                             hh_g04a = merged$h15bq4,
                             hh_g04b = merged$h15bq5,
                             hh_g05a = merged$h15bq6,
                             hh_g05b = merged$h15bq7,
                             hh_g06a = merged$h15bq8,
                             hh_g06b = merged$h15bq9,
                             hh_g07a = merged$h15bq10,
                             hh_g07b = merged$h15bq11,
                             hh_g08 = merged$h15bq12,
                             hh_g09 = merged$h15bq13)


nrow(as.data.frame(unique(final_dataset11_food$case_id))) # final_dataset11_food contains 2157 households

final_dataset11_food <- final_dataset11_food %>% 
  group_by(hh_g02_os1) %>% arrange(case_id, hh_g02_os) # same dataset, just more clear to read!


rm(food_2011, final_vector_sample, GSEC15B_2011, matched_hh_2011, hh_2011, merged)

##################
# a)        ###### How many and which items per food category
##################

item_list11 <- final_dataset11_food %>% group_by(hh_g02_os) %>% 
  summarise(type = paste(sort(unique(hh_g02)),collapse=", ")) # last row is with items beelonging to NA. I checked and they are all already inckluded i nprevious categories, so we remove the row

item_list11[-nrow(item_list11),] # remove last row

as.data.frame(unique(final_dataset11_food$hh_g02)) # 71 items


rm(item_list11)
##################
# 1)        ###### MEAN market price for each item code
##################

price_list11_food <- final_dataset11_food %>%                                     #  should I use GSEC15b_2010 to compute average market price?
  group_by(hh_g02) %>%                                  #  hh_g02 == item code
  summarise((mean(hh_g08, na.rm = TRUE))) %>%           #  hh_g08 == market price
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))")  

# add prices to dataset

final_dataset11_food <- final_dataset11_food %>% 
  group_by(hh_g02) %>% 
  mutate(mean_market_price = (mean(hh_g08, na.rm = TRUE))) 


##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################

final_dataset11_food %>%                                       # 
  group_by(hh_g02, regionXurban) %>%                      #  
  summarise((mean(hh_g08, na.rm = TRUE))) %>%             # 
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))") 


final_dataset11_food <- final_dataset11_food %>% 
  group_by(hh_g02,regionXurban) %>% 
  mutate(mean_market_price_regions = (mean(hh_g08, na.rm = TRUE))) 

filter(final_dataset11_food, hh_g02 == 105 & (regionXurban == 10| regionXurban == 20)) # %>% view() # TEST. OK!



##################
# 2)        ###### frequency of occurrence of each item. E.g., item 150 (salt) is the most consumed
##################

final_dataset11_food %>% 
  group_by(hh_g02) %>% 
  summarise((count= n())) %>%
  rename(count = "(count = n())") %>% 
  arrange(desc(count))


##################
# 2.1)        ###### frequency of occurrence of each consumption category. E.g., category 6 (vegetables) is the most consumed
##################

final_dataset11_food %>% 
  group_by(hh_g02_os) %>% 
  summarise((count= n())) %>%
  rename(count = "(count = n())") %>% 
  arrange(desc(count))


##################
# 3)        ###### manually compute quantity of each items in each household
##################

# total quantity of that item by that household. Built-in variable
final_dataset11_food$hh_g03a 

# total quantity of that item by that household. Manually computed variable
final_dataset11_food <- final_dataset11_food %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = sum(hh_g04a, hh_g05a, hh_g06a, hh_g07a, na.rm = T)) 


# tests
final_dataset11_food[,c("hh_g03a", "tot_quantity_per_item_manual")] # check for discrepancy. 

all(final_dataset11_food$hh_g03a == final_dataset11_food$tot_quantity_per_item_manual) # FALSE

sum(final_dataset11_food$hh_g03a, na.rm = T)                        # 170938.8 
sum(final_dataset11_food$tot_quantity_per_item_manual, na.rm = T)   # 157910.8 (smaller difference compared to 2010)



##################
# 4)        ###### multiply quantity of item by price of item (MEAN)
##################

final_dataset11_food <- final_dataset11_food %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price,          # FROM here on, I have to decide whether I use "hh_g03a" or "tot_quantity_per_item_manual"
         purchas1_quantity_and_price = hh_g04a*mean_market_price,
         purchas2_quantity_and_price = hh_g05a*mean_market_price,
         home_prod_quantity_and_price = hh_g06a*mean_market_price,
         gifts_quantity_and_price = hh_g07a*mean_market_price)



##################
# 5)        ###### aggregate consumption of items into consumption per FOOD CATEGORY
##################

# for quantities, I use sum of 4 variables:  hh_g04a, hh_g05a, hh_g06a, hh_g07a  
b <- final_dataset11_food %>%                                 
  group_by(case_id, ea_id, wgt11, urban, regionXurban, hh_g02_os, hh_g02_os1) %>%
  summarise(quantity_purchas1 = sum(hh_g04a, na.rm = T),
            quantity_purchas2 = sum(hh_g05a, na.rm = T),
            quantity_home_prod = sum(hh_g06a, na.rm = T),
            quantity_gifts = sum(hh_g07a, na.rm = T),
            quantity_food_category_tot = quantity_purchas1 + quantity_purchas2 + quantity_home_prod + quantity_gifts,
            total_consume = sum(tot_quantity_and_price, na.rm = T),
            total_consume_purch1 = sum(purchas1_quantity_and_price , na.rm = T),
            total_consume_purch2 = sum(purchas2_quantity_and_price, na.rm = T),
            total_consume_home_prod = sum(home_prod_quantity_and_price, na.rm = T),
            total_consume_gifts = sum(gifts_quantity_and_price, na.rm = T)) %>% 
  as.data.frame() 

# tests
nrow(as.data.frame(unique(b$case_id))) # 2402 OK!



##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################


b1_food11 <- b %>% 
  group_by(case_id, ea_id, wgt11, urban, regionXurban) %>% 
  summarise(total_consumption_food = sum(total_consume, na.rm = T),
            purchases = sum(total_consume_purch1, na.rm = T) + sum(total_consume_purch2, na.rm = T),
            home_prod = sum(total_consume_home_prod, na.rm = T),
            gifts_and_other = sum(total_consume_gifts, na.rm = T)) %>% 
  mutate(perc_tot = total_consumption_food/total_consumption_food,
         perc_purch = purchases/total_consumption_food,
         perc_home_prod = home_prod/total_consumption_food,
         perc_gift_and_other = gifts_and_other/total_consumption_food,
         test = perc_purch + perc_home_prod + perc_gift_and_other)

rm(b)


# save household level food consumption in 2011 wave
write_csv(b1_food11, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2011_food.csv") 

# save price list to apply it to other years' food items
write_csv(price_list11_food, "/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_food.csv") 














