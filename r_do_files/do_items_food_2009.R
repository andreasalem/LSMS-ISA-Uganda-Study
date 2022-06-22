#
##                                                                   
##
########################################################################
########################################################################
##                                                                    ##       
##                food_household_items_2009.R                         ##       
##                                                                    ##       
########################################################################
########################################################################

######
# case_id      = Unique HH Identifier
# ea_id        = Unique EA Itentifier
# wgt09       = Household Sampling Weight
# urban        = urban (=1) vs. rural (=0)?
# regionXurban = urbanXregion
# hh_g01       = Consumed Item?
# hh_g02       = Item Code
# hh_g02_os    = Item (Other Specify - Consumption Category label)
# hh_g02_os1   = Item (Other Specify - Consumption Category)

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

#### NOW we work with 2009

# food consumption file
GSEC15B_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15B.csv") # 2931  
names(GSEC15B_2009)[names(GSEC15B_2009) == "HHID"] <- "id_hh"

# files with household level information 
hh_2009 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2009.csv")                                # 2975 HH in 2009
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")       # 2157 in our analysis

# create final dataset for analysis
matched_hh_2009 <- filter(hh_2009, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

food_2009 <- filter(GSEC15B_2009, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2009, food_2009, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset09_food <- as.data.frame(merged$id_hh)
names(final_dataset09_food)[names(final_dataset09_food) == "merged$id_hh"] <- "case_id"


final_dataset09_food <- summarise(final_dataset09_food,
                             case_id = merged$id_hh,
                             ea_id = merged$comm,
                             wgt09 = merged$wgt09,
                             urban  = merged$urban,
                             regionXurban = merged$regurb,
                             hh_g01 = NA,                   # this info is not present in 2009 data
                             hh_g02 = merged$h15bq2,
                             hh_g02_os = NA,                # this info is not present in 2009 data
                             hh_g02_os1 = NA,               # this info is not present in 2009 data
                             hh_g03a = NA,                  # this info is not present in 2009 data
                             hh_g03b = NA,                  # this info is not present in 2009 data
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


nrow(as.data.frame(unique(na.omit(final_dataset09_food$case_id)))) # final_dataset09_food contains 2157 households


final_dataset09_food <- final_dataset09_food %>% 
 arrange(case_id, hh_g02) # same dataset, just more clear to read!

rm(food_2009, final_vector_sample, GSEC15B_2009, matched_hh_2009, hh_2009, merged)

##################
# a)        ###### How many items? in 2009 we don't have info. on consumption category, rather only item codes
##################

as.data.frame(unique(final_dataset09_food$hh_g02)) # 64 items



##################
# 1)        ###### MEAN market price for each item code
##################

final_dataset09_food %>%                                     #  should I use GSEC15b_2010 to compute average market price?
  group_by(hh_g02) %>%                                  #  hh_g02 == item code
  summarise((mean(hh_g08, na.rm = TRUE))) %>%           #  hh_g08 == market price
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))") 

# add prices to dataset

# final_dataset09_food <- final_dataset09_food %>% 
#   group_by(hh_g02) %>% 
#   mutate(mean_market_price = (mean(hh_g08, na.rm = TRUE))) 


# add prices to dataset, but we use same market prices as in 2011
price_list11_food <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_food.csv")


final_dataset09_food <- right_join(final_dataset09_food, price_list11_food, by = "hh_g02")  


final_dataset09_food <- final_dataset09_food %>% 
  rename(mean_market_price = avg_market_p)


##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################

final_dataset09_food %>%                                       #  here I do the same thing but with the median (result change a bit)
  group_by(hh_g02, regionXurban) %>%                      #  
  summarise((mean(hh_g08, na.rm = TRUE))) %>%             # 
  rename(avg_market_p = "(mean(hh_g08, na.rm = TRUE))") 

# add prices to dataset

final_dataset09_food <- final_dataset09_food %>% 
  group_by(hh_g02,regionXurban) %>% 
  mutate(mean_market_price_regions = (mean(hh_g08, na.rm = TRUE))) 

filter(final_dataset09_food, hh_g02 == 105 & (regionXurban == 10| regionXurban == 20)) # %>% view()  # TEST. OK!



##################
# 2)        ###### frequency of occurrence of each item. E.g., item 150 (salt) is the most consumed
##################

final_dataset09_food %>% 
  group_by(hh_g02) %>% 
  summarise((count= n())) %>%
  rename(count = "(count = n())") %>% 
  arrange(desc(count))


##################
# 2.1)        ###### frequency of occurrence of each consumption category. E.g., category 6 (vegetables) is the most consumed
##################


# no info.


##################
# 3)        ###### manually compute quantity of each items in each household
##################

# total quantity of that item by that household. Built-in variable is not present in 2009


# total quantity of that item by that household. Manually computed variable
final_dataset09_food <- final_dataset09_food %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = sum(hh_g04a, hh_g05a, hh_g06a, hh_g07a, na.rm = T)) 



##################
# 4)        ###### multiply quantity of item by price of item (MEAN)
##################

final_dataset09_food <- final_dataset09_food %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price,          # FROM here on, I have to decide whether I use "hh_g03a" or "tot_quantity_per_item_manual"
         purchas1_quantity_and_price = hh_g04a*mean_market_price,
         purchas2_quantity_and_price = hh_g05a*mean_market_price,
         home_prod_quantity_and_price = hh_g06a*mean_market_price,
         gifts_quantity_and_price = hh_g07a*mean_market_price)



##################
# 5)        ###### aggregate consumption of items into consumption per FOOD CATEGORY
##################


# for quantities, I use sum of 4 variables:  hh_g04a, hh_g05a, hh_g06a, hh_g07a  
b <- final_dataset09_food %>%                                 
  group_by(case_id, ea_id, wgt09, urban, regionXurban) %>%
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

# tests
nrow(as.data.frame(unique(na.omit(b$case_id)))) # 2156! 
# Last row is NA! We observe that one household is missing it is missing after the right joint when we added the 2011 prices above. 
# We assign all NAs to this household. If we observe BEFORE THE RIGHT JOINT the information on this household, we will see that it's almost all NAs anyway

b <- add_row(b, case_id = 113100030302) # we add this household, because 

b <- b %>% 
  arrange(case_id)

b <- b[1:2157,]

  
##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################


b1_food09 <- b %>% 
  group_by(case_id, ea_id, wgt09, urban, regionXurban) %>% 
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




# save household level food consumption in 2009 wave
write_csv(b1_food09, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_food.csv") 














