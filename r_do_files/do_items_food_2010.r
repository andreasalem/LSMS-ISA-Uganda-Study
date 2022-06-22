#
##                                                                   
##
########################################################################
########################################################################
##                                                                    ##       
##                food_household_items_2010.R                         ##       
##                                                                    ##       
########################################################################
########################################################################

######
# case_id      = Unique HH Identifier
# ea_id        = Unique EA Itentifier
# wgt10        = Household Sampling Weight
# urban        = urban (=1) vs. rural (=0)?
# regionXurban = urbanXregion
# hh_g01       = Consumed Item?
# hh_g02       = Item Code
# hh_g02b      = Item Label
# hh_g02_os    = Item (Other Specify - Consumption Category code)
# hh_g02_os1   = Item (Other Specify - Consumption Category label)

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

########################################################################
########################################################################

getwd()
setwd("/Users/andreasalem/R_oba/Thesis")

options(scipen=999) # avoids scientific notation

#### NOW we work with 2010

# food consumption file
GSEC15b_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15b.csv") # 2657 households
names(GSEC15b_2010)[names(GSEC15b_2010) == "hh"] <- "id_hh"

# files with household level information 
hh_2010 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2010.csv")                                # 2716 HH in 2010
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")       # 2157 in our analysis

# create final dataset for analysis
matched_hh_2010 <- filter(hh_2010, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

food_2010 <- filter(GSEC15b_2010, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2010, food_2010, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset10_food <- as.data.frame(merged$id_hh)
names(final_dataset10_food)[names(final_dataset10_food) == "merged$id_hh"] <- "case_id"



final_dataset10_food <- summarise(final_dataset10_food,
                           case_id = merged$id_hh,
                           ea_id = merged$comm,
                           wgt10 = merged$wgt10,
                           urban  = merged$urban,
                           regionXurban = merged$regurb,
                           hh_g01 = merged$h15bq3a,
                           hh_g02 = merged$itmcd,
                           hh_g02b = merged$h15bq2b,
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


nrow(as.data.frame(unique(final_dataset10_food$case_id))) # final_dataset10_food contains 2157 households

final_dataset10_food <- final_dataset10_food %>% 
  group_by(hh_g02_os1) %>% arrange(case_id, hh_g02_os) # same dataset, just more clear to read!

rm(food_2010, GSEC15b_2010, matched_hh_2010, hh_2010, merged)
## we compute the average market price --> this gives real consumption share, not nominal 
## (if each household faces different prices or assignes differeent values, then we would get effect of prices)
## (instead, by focusing on real consumption, i.e., assign to everyone the same price, we get insight about quantities consumed)




##################
# a)        ###### How many and which items per food category
##################

item_list10 <- final_dataset10_food %>% group_by(hh_g02_os) %>% 
  summarise(type = paste(sort(unique(hh_g02)),collapse=", ")) # last row is with items beelonging to NA. I checked and they are all already inckluded i nprevious categories, so we remove the row

item_list10[-nrow(item_list10),] # remove last row

as.data.frame(unique(final_dataset10_food$hh_g02)) # 70 items

rm(item_list10)
##################
# 1)        ###### MEAN market price for each item code
##################

final_dataset10_food %>%                                     #  should I use GSEC15b_2010 to compute average market price?
  group_by(hh_g02) %>%                                  #  hh_g02 == item code
  summarise((mean(hh_g08, na.rm = TRUE))) %>%           #  hh_g08 == market price
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))") 



# compute 2010 food price list, even if we will use 2011 fod prices
price_list10_food <- final_dataset10_food %>%                                    
  group_by(hh_g02) %>%                                  #  hh_g02 == item code
  summarise((mean(hh_g08, na.rm = TRUE))) %>%           #  hh_g08 == market price
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))")  


# add prices to dataset, but we use same market prices as in 2011
price_list11_food <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_food.csv")


final_dataset10_food <- right_join(final_dataset10_food, price_list11_food, by = "hh_g02")  ### erase this!

final_dataset10_food <- final_dataset10_food %>% 
  rename(mean_market_price = avg_market_p)


##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################

final_dataset10_food %>%                                       #  here I do the same thing but with the mean (result change a bit)
  group_by(hh_g02, regionXurban) %>%                      #  
  summarise((mean(hh_g08, na.rm = TRUE))) %>%             # 
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))") 

# add prices to dataset

final_dataset10_food <- final_dataset10_food %>% 
  group_by(hh_g02,regionXurban) %>% 
  mutate(mean_market_price_regions = (mean(hh_g08, na.rm = TRUE))) 

filter(final_dataset10_food, hh_g02 == 105 & (regionXurban == 10| regionXurban == 20)) # %>% view()  # TEST. OK!


##################
# 2)        ###### frequency of occurrence of each item. E.g., item 150 (salt) is the one most occurring 
##################

final_dataset10_food %>% 
  group_by(hh_g02) %>% 
  summarise((count= n())) %>%
  rename(count = "(count = n())") %>% 
  arrange(desc(count))


##################
# 2.1)        ###### frequency of occurrence of each consumption category. E.g., category 6 (vegetables) is the one most occurring 
##################

final_dataset10_food %>% 
  group_by(hh_g02_os) %>% 
  summarise((count= n())) %>%
  rename(count = "(count = n())") %>% 
  arrange(desc(count))


##################
# 3)        ###### for each item, manually compute quantity consumed in each household 
##################

# Variable "hh_g03a" is part of the questionnaire and contains this info. Here we want to do it manually by addition of variables "hh_g04a", "hh_g05a", "hh_g06a", and "hh_g07a" 
final_dataset10_food$hh_g03a 

# We manually sum "hh_g04a", "hh_g05a", "hh_g06a", and "hh_g07a" 
final_dataset10_food <- final_dataset10_food %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = sum(hh_g04a, hh_g05a, hh_g06a, hh_g07a, na.rm = T)) 


# tests
final_dataset10_food[,c("hh_g03a", "tot_quantity_per_item_manual")] # check for discrepancy between "hh_g03a" and our computed variable "tot_quantity_per_item_manual"

all(final_dataset10_food$hh_g03a == final_dataset10_food$tot_quantity_per_item_manual) # FALSE

sum(final_dataset10_food$hh_g03a, na.rm = T)                        # BIG DIFFERENCE!
sum(final_dataset10_food$tot_quantity_per_item_manual, na.rm = T)   # BIG DIFFERENCE! In the analysis, we decide to use the variable we built manually



##################
# 4)        ###### multiply quantity of item by price of item (MEAN)
##################

final_dataset10_food <- final_dataset10_food %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price,          # from HERE on, I have to decide whether I use "hh_g03a" or "tot_quantity_per_item_manual"
         purchas1_quantity_and_price  = hh_g04a*mean_market_price,
         purchas2_quantity_and_price  = hh_g05a*mean_market_price,
         home_prod_quantity_and_price = hh_g06a*mean_market_price,
         gifts_quantity_and_price     = hh_g07a*mean_market_price)


##################
# 5)        ###### aggregate items into consumption per FOOD CATEGORY
##################


# for quantities, I use sum of 4 variables:  hh_g04a, hh_g05a, hh_g06a, hh_g07a  
b <- final_dataset10_food %>%                                 
  group_by(case_id, ea_id, wgt10, urban, regionXurban, hh_g02_os, hh_g02_os1) %>%  
  summarise(quantity_purchas1 = sum(hh_g04a, na.rm = T),
            quantity_purchas2 = sum(hh_g05a, na.rm = T),
            quantity_home_prod = sum(hh_g06a, na.rm = T),
            quantity_gifts = sum(hh_g07a, na.rm = T),
            quantity_food_category_tot = quantity_purchas1 + quantity_purchas2 + quantity_home_prod + quantity_gifts,
            total_consume = sum(tot_quantity_and_price, na.rm = T),
            total_consume_purch1 = sum(purchas1_quantity_and_price, na.rm = T),
            total_consume_purch2 = sum(purchas2_quantity_and_price, na.rm = T),
            total_consume_home_prod = sum(home_prod_quantity_and_price, na.rm = T),
            total_consume_gifts = sum(gifts_quantity_and_price, na.rm = T)) %>% 
  as.data.frame() 

# tests for nr. of households
nrow(as.data.frame(unique(na.omit(b$case_id)))) # 2157 OK!



##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################


b1_food10 <- b %>% 
  group_by(case_id, ea_id, wgt10, urban, regionXurban) %>% 
  summarise(total_consumption_food = sum(total_consume, na.rm = T),
            purchases_food = sum(total_consume_purch1, na.rm = T) + sum(total_consume_purch2, na.rm = T),
            home_prod_food = sum(total_consume_home_prod, na.rm = T),
            gifts_and_other_food = sum(total_consume_gifts, na.rm = T)) %>% 
  mutate(perc_tot = total_consumption_food/total_consumption_food,
         perc_purch = purchases_food/total_consumption_food,
         perc_home_prod = home_prod_food/total_consumption_food,
         perc_gift_and_other = gifts_and_other_food/total_consumption_food,
         test = perc_purch + perc_home_prod + perc_gift_and_other)

rm(b)

b1_food10 <- filter(b1_food10, case_id %in% final_vector_sample$id_hh) %>% arrange(case_id)
rm(final_vector_sample)




# save household level food consumption in 2010 wave
write_csv(b1_food10, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2010_food.csv") 




