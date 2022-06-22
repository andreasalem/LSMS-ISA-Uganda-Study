######

library(tidyverse)
library(dplyr)

########################################################################
########################################################################
####                                                                ####   
####                       DURABLES 2009                            ####  
####                                                                ####   
########################################################################
########################################################################

getwd()
setwd("/Users/andreasalem/R_oba/Thesis")

options(scipen=999) # avoids scientific notation




# Semi-Durable Goods and Durable Goods and Service
GSEC15d_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15D.csv")   # 2992
names(GSEC15d_2009)[names(GSEC15d_2009) == "HHID"] <- "id_hh"

# files with household level information 
hh_2009 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2009.csv")                                # 2975 HH in 2009
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv") # 2402 in our analysis

# create final dataset for analysis
matched_hh_2009 <- filter(hh_2009, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

durable_2009 <- filter(GSEC15d_2009, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)

merged <- merge(matched_hh_2009, durable_2009, by = "id_hh")  ###### THIS DATASET IS FINAL

final_dataset09_durable <- as.data.frame(merged$id_hh)
names(final_dataset09_durable)[names(final_dataset09_durable) == "merged$id_hh"] <- "case_id"


final_dataset09_durable <- summarise(final_dataset09_durable,
                                     case_id = merged$id_hh,
                                     ea_id = merged$comm,
                                     wgt09 = merged$wgt09,
                                     urban  = merged$urban,
                                     regionXurban = merged$regurb,
                                     hh_g02 = merged$h15dq2,
                                     hh_g04a = merged$h15dq4,
                                     hh_g04b = merged$h15dq5)


nrow(as.data.frame(unique(final_dataset09_durable$case_id))) # 2157

rm(durable_2009, final_vector_sample, GSEC15d_2009, hh_2009, merged)
####################

colSums(is.na(final_dataset09_durable))

nrow(filter(final_dataset09_durable, is.na(hh_g04a)==F))
nrow(filter(final_dataset09_durable, hh_g04a != 0))  # almost all values of hh_g04a are either 0 or NA
unique(filter(final_dataset09_durable, is.na(hh_g04a)==F)$hh_g04a) # but there are some values (but strange for a quantity)
unique(filter(final_dataset09_durable, hh_g04a != 0)$hh_g04b) # regarding hh_g04b (value) not all are NA

# in 2009, each household appears 36 times, once for each item, even for items they did not consume. The items they consumed can be identified because they are the only ones for which a value is reported. 
# WIth the following line, we filter for consumed items. 

final_dataset09_durable <- final_dataset09_durable[complete.cases(final_dataset09_durable[ , 8]),]  %>% filter(hh_g04b != 0) # we keep values in hh_g04b which are bigger than 0


##################
# a)        ###### How many and which items are there?
##################

final_dataset09_durable %>% 
  summarise(item = unique(hh_g02)) %>% arrange(item) # 40 items



# add prices to dataset, we use same market prices as in 2011
price_list11_durable <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/price_list11_durable.csv")

final_dataset09_durable <- right_join(final_dataset09_durable, price_list11_durable, by = "hh_g02")



##################
# 1.1)     ###### MEAN market price for each item code, by regionXurban (each item has 8 different prices)
##################


##################
# 2)        ###### frequency of occurrence of each item
##################

final_dataset09_durable %>% 
  group_by(hh_g02) %>% 
  summarise(count = (count= n())) %>%
  arrange(desc(count)) 


##################
# 3)        ###### for each item, manually compute quantity consumed in each household --> assumption: each item present is consumed once
##################


# We manually do it
final_dataset09_durable <- final_dataset09_durable %>% 
  rowwise() %>% 
  mutate(tot_quantity_per_item_manual = 1) 



##################
# 4)        ###### multiply quantity of item by price of item (MEAN market price)
##################

final_dataset09_durable <- final_dataset09_durable %>% 
  mutate(tot_quantity_and_price = tot_quantity_per_item_manual*mean_market_price)

##################
# 6)        ###### aggregate consumption of food categories into consumption per HOUSEHOLD
##################

# alternative b1, where each item is consumer once and we can't distinguish the origin of the item
b1_durables09 <- final_dataset09_durable %>%                                 
  group_by(case_id, ea_id, wgt09, urban, regionXurban) %>%  
  summarise(total_consumption_annual = sum(tot_quantity_and_price, na.rm = T),
            total_consumption_weekly = total_consumption_annual/52.14286) %>%
  as.data.frame() 



#### many households have 0 consumption expenditure on durables. We re-insert these households in the dataset

matched_hh_2009 <- matched_hh_2009 %>% 
  rename(case_id = id_hh)

b1_durables09 <- left_join(matched_hh_2009, b1_durables09, by = "case_id", all.x = T) %>%  arrange(case_id) %>% 
  select(case_id, wgt09.x, comm, urban.x, regurb, total_consumption_annual, total_consumption_weekly) %>% 
  rename(wgt09 = wgt09.x, ea_id = comm, urban = urban.x, regionXurban = regurb)

b1_durables09$total_consumption_annual[is.na(b1_durables09$total_consumption_annual)] <- 0
b1_durables09$total_consumption_weekly[is.na(b1_durables09$total_consumption_weekly)] <- 0
  



# tests for nr. of households
nrow(as.data.frame(unique(b1_durables09$case_id))) # 2157


# total weekly consumption in sample
sum(b1_durables09$total_consumption_weekly)


# save household level durable consumption in 2009 wave
write_csv(b1_durables09, "/Users/andreasalem/R_oba/Thesis/Data_clean/items_2009_durables.csv") 



