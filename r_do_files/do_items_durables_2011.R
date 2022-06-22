######

library(tidyverse)
library(dplyr)
library(haven) #used to open .dta files
########################################################################
########################################################################
####                                                                ####   
####                       DURABLES 2011                            ####  
####                                                                ####   
########################################################################
########################################################################

getwd()
setwd("/Users/andreasalem/R_oba/Thesis")

options(scipen=999) # avoids scientific notation




# Semi-Durable Goods and Durable Goods and Service
GSEC15D_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15D.dta") # 2829
names(GSEC15D_2011)[names(GSEC15D_2011) == "HHID"] <- "id_hh"

# files with household level information 
hh_2011 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2011.csv")                                # 2850 HH in 2011
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv") # 2402 in our analysis

# create final dataset for analysis
matched_hh_2011 <- filter(hh_2011, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

durable_2011 <- filter(GSEC15D_2011, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2011, durable_2011, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset11_durable <- as.data.frame(merged$id_hh)
names(final_dataset11_durable)[names(final_dataset11_durable) == "merged$id_hh"] <- "case_id"



final_dataset11_durable <- summarise(final_dataset11_durable,
                                     case_id = merged$id_hh,
                                     ea_id = merged$comm,
                                     wgt11 = merged$wgt11,
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


nrow(as.data.frame(unique(final_dataset11_durable$case_id))) # 2157 households


rm(durable_2011, final_vector_sample, GSEC15D_2011, hh_2011, merged)


####################
colSums(is.na(final_dataset11_durable))

final_dataset11_durable$hh_g04a <- 1  # all values are NA, so we assign 1 for quantity
final_dataset11_durable$hh_g06a <- 1
final_dataset11_durable$hh_g07a <- 1

# in 2011, each household appears 36 times, once for each item, even for items they did not consume. The items they consumed can be identifies becasue they are the only onces for which a value is reported. 
# WIth the following line, we filter for consumed items. 
final_dataset11_durable <- final_dataset11_durable %>% filter((is.na(hh_g03b) == F) & (is.na(hh_g04b) == F) & (is.na(hh_g06b) == F) & (is.na(hh_g07b) ==F)) # %>%  view()



  

##################
# a)        ###### How many and which items are there?
##################

final_dataset11_durable %>% 
  summarise(item = unique(hh_g02)) %>% arrange(item) # 56 items # we need to exclude "non-consumption expenditure" and convert e.g., 3011 to 301, as they represent the same item (UBOS, p. 11, Basic info 2011/12) 

final_dataset11_durable <- final_dataset11_durable %>% 
  mutate(hh_g02 = ifelse(hh_g02 == 3011, 301, hh_g02),
         hh_g02 = ifelse(hh_g02 == 3022, 302, hh_g02),
         hh_g02 = ifelse(hh_g02 == 3033, 303, hh_g02),
         hh_g02 = ifelse(hh_g02 == 3044, 304, hh_g02),
         hh_g02 = ifelse(hh_g02 == 3055, 305, hh_g02),
         hh_g02 = ifelse(hh_g02 == 3066, 306, hh_g02),
         hh_g02 = ifelse(hh_g02 == 5011, 501, hh_g02),
         hh_g02 = ifelse(hh_g02 == 5021, 502, hh_g02),
         hh_g02 = ifelse(hh_g02 == 5031, 503, hh_g02),
         hh_g02 = ifelse(hh_g02 == 5041, 504, hh_g02),
         hh_g02 = ifelse(hh_g02 == 6011, 601, hh_g02),
         hh_g02 = ifelse(hh_g02 == 6022, 602, hh_g02),
         hh_g02 = ifelse(hh_g02 == 6033, 603, hh_g02),
         hh_g02 = ifelse(hh_g02 == 6044, 604, hh_g02),
         hh_g02 = ifelse(hh_g02 == 6055, 605, hh_g02))


as.data.frame(arrange(summarise(final_dataset11_durable, item = unique(hh_g02)), item))[1:40,1] # keep only till 703 and exclude "non-consumption expenditure"

final_dataset11_durable <- final_dataset11_durable %>% 
  filter(final_dataset11_durable$hh_g02 %in% as.data.frame(arrange(summarise(final_dataset11_durable, item = unique(hh_g02)), item))[1:40,1])

final_dataset11_durable %>% 
  summarise(item = unique(hh_g02)) %>% arrange(item) # 40 items

##################
# 1)        ###### MEAN market price for each item code
##################

as.data.frame(final_dataset11_durable %>%                          
                group_by(hh_g02) %>%                                                 #  hh_g02 == item code
                summarise(avg_market_p = (mean(hh_g08, na.rm = TRUE))))              #  hh_g08 == market price



# but for the majority of items, no market price is reported. 
# To compute manually a "market price" for these items, we use variables on value ("hh_g04b", "hh_g06b", "hh_g07b"), sum them and 
# also sum with average market price computed above, and take the mean

price_list11_durable <- final_dataset11_durable %>% 
  group_by(hh_g02) %>% 
  summarise(mean_value_purch     = (mean(hh_g04b, na.rm = T)),
            mean_value_home_prod = (mean(hh_g06b, na.rm = T)),
            mean_value_gift      = (mean(hh_g07b, na.rm = T)),
            mean_market_p = (mean(hh_g08, na.rm = TRUE))) %>%    # this column is the same as the one created above
  rowwise() %>% 
  mutate(mean_market_price = mean(c(mean_value_purch, mean_value_home_prod, mean_value_gift, mean_market_p), na.rm = T)) %>% select(hh_g02,mean_market_price )


# add prices to dataset

final_dataset11_durable <- right_join(final_dataset11_durable, price_list11_durable, by = "hh_g02")



##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################


##################
# 2)        ###### frequency of occurrence of each item. E.g., item 203 (Chldren's clothing) is the one most occurring 
##################

final_dataset11_durable %>% 
  group_by(hh_g02) %>% 
  summarise(count = (count= n())) %>%
  arrange(desc(count))


##################
# 3)        ###### for each item, manually compute quantity consumed in each household 
##################

# Variable "hh_g03a" is part of the questionnaire and contains this info. Here we want to do it manually by addition of variables "hh_g04a", "hh_g05a", "hh_g06a", and "hh_g07a" 
final_dataset11_durable$hh_g03a # almost all NAs


# We manually sum "hh_g04a", "hh_g06a", and "hh_g07a" 
final_dataset11_durable <- final_dataset11_durable %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = 1) # replace 1 with sum(hh_g04a, hh_g06a, hh_g07a, na.rm = T)


# this shows for each column, how many NAs are there. We have a problem here!
colSums(is.na(final_dataset11_durable))

##################
# 4)        ###### multiply quantity of item by price of item (MEAN market price)
##################

final_dataset11_durable <- final_dataset11_durable %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price)

# ---> we don't distinguish anymore between source of goods, so we omit the following lines

# purchas1_quantity_and_price  = hh_g04a*mean_market_price, 
# home_prod_quantity_and_price = hh_g06a*mean_market_price,
# gifts_quantity_and_price     = hh_g07a*mean_market_price)



##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################


# for quantities, we should use sum of 4 variables:  hh_g04a, hh_g05a, hh_g06a, hh_g07a. But for durables, we donn't distinguish


b1_durables11 <- final_dataset11_durable %>%                                 
  group_by(case_id, ea_id, wgt11, urban, regionXurban) %>%  
  summarise(total_consumption_annual = sum(tot_quantity_and_price, na.rm = T),
            total_consumption_weekly = total_consumption_annual/52.14286) %>%
  as.data.frame() 



#### many households have 0 consumption expenditure on durables. We re-insert these households in the dataset

matched_hh_2011 <- matched_hh_2011 %>% 
  rename(case_id = id_hh)

b1_durables11 <- left_join(matched_hh_2011, b1_durables11, by = "case_id", all.x = T) %>%  arrange(case_id) %>% 
  select(case_id, wgt11.x, comm, urban.x, regurb, total_consumption_annual, total_consumption_weekly) %>% 
  rename(wgt11 = wgt11.x, ea_id = comm, urban = urban.x, regionXurban = regurb)

b1_durables11$total_consumption_annual[is.na(b1_durables11$total_consumption_annual)] <- 0
b1_durables11$total_consumption_weekly[is.na(b1_durables11$total_consumption_weekly)] <- 0





# tests for nr. of households
nrow(as.data.frame(unique(b1_durables11$case_id))) # 2157 


# total weekly consumption in sample
sum(b1_durables11$total_consumption_weekly)



# save household level durable consumption in 2011 wave
write_csv(b1_durables11, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2011_durables.csv") 

# save price list to apply it to other years' durables
write_csv(price_list11_durable, "/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_durable.csv")  

