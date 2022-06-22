######################### 
#
#
#
######################### 
######################### 
######################### Output of this file are three roster data-sets  with individual-level observations: 
######
######    - joined09_1 (=matched_hh_2009.csv) --> imported as "hh_roster09" in later analysis
######    - joined10_1 (=matched_hh_2010.csv) --> imported as "hh_roster10" in later analysis
######    - joined11_1 (=matched_hh_2011.csv) --> imported as "hh_roster11" in later analysis
######
######


######################### Code


getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
options(scipen=999) # avoids scientific notation

####### In this file we merge for each year the datasets from household level info; characteristics info.; and education info.
####### These files are contained in the repository: Data_clean

####### important note:
####### matching for each year the files "hh_[YEAR], "hh_charact[YEAR].csv", and "hh_educ[YEAR].csv"
####### 

## We first import all necessary files:
hh_data09 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2009.csv")              # we will use it later again      # 2975 households
char_data09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_charact2009.csv")                                      # 2885 households, 18'734 individuals
edu_data09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_educ2009.csv")                                          # 2975 households, 13'767 individuals

hh_data10 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2010.csv")              # we will use it later again      # 2975 households
char_data10 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_charact2010.csv")                                      # 2716 households, 19'180 individuals
edu_data10 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_educ2010.csv")                                          # 2714 households, 12'836 individuals

hh_data11 <-read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2011.csv")              # we will use it later again      # 2850 households
char_data11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_charact2011.csv")                                      # 2850 households, 21'487 observations, 21'207 individuals
edu_data11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/hh_educ2011.csv")                                          # 2835 households, 13'772 observations, 13'720 individuals


# Then, for each year, we match by household identifiers

########## 2009 --> with 2975 households and 18'734 individuals
joined09 <- merge(char_data09, edu_data09, by = "id_ind", all.x = TRUE)[,c("id_hh.x", "id_ind", "ind_sex", "ind_position", "ind_age", "ind_marital_status", "n_ind_household", "n_child_15", "n_child_5", "ind_read", "ind_write", "ind_literacy", "ind_any_schooling", "ind_education", "ind_education_years")]
names(joined09)[names(joined09) == "id_hh.x"] <- "id_hh"
joined09 <-left_join(hh_data09, joined09, by = "id_hh")

########## 2010 --> with 2716 households and 19'180 individuals
joined10 <- merge(char_data10, edu_data10, by = "id_ind", all.x = TRUE)[,c("id_hh.x", "id_ind", "ind_sex", "ind_position", "ind_age", "ind_marital_status", "n_ind_household", "n_child_15", "n_child_5", "ind_read", "ind_write", "ind_literacy", "ind_any_schooling", "ind_education", "ind_education_years")]
names(joined10)[names(joined10) == "id_hh.x"] <- "id_hh"
joined10 <-left_join(hh_data10, joined10, by = "id_hh")

########## 2011 --> with 2850 households and 21'207 individuals
joined11 <- merge(char_data11, edu_data11, by = "id_ind", all.x = TRUE)[,c("id_hh.x", "id_ind", "ind_sex", "ind_position", "ind_age", "ind_marital_status", "n_ind_household", "n_child_15", "n_child_5", "ind_read", "ind_write", "ind_literacy", "ind_any_schooling", "ind_education", "ind_education_years")]
names(joined11)[names(joined11) == "id_hh.x"] <- "id_hh"
joined11 <-left_join(hh_data11, joined11, by = "id_hh")





### now, we need to understand which Household appear each year, and keep only those
############################################################## We match id_hh in the 3 do_hh files
#### here we use three files that we already imported above

# here we see 2647 matches for 2009 and 2010
common_hh_09_10 <- as.data.frame(intersect(hh_data09$id_hh, hh_data10$id_hh)) 
names(common_hh_09_10)[names(common_hh_09_10) == "intersect(hh_data09$id_hh, hh_data10$id_hh)"] <- "id_hh"

# here we see that in total, we have 2462 household matched over the three years
hh_matched_2462 <- as.data.frame(intersect(common_hh_09_10$id_hh, hh_data11$id_hh)) 
names(hh_matched_2462)[names(hh_matched_2462) == "intersect(common_hh_09_10$id_hh, hh_data11$id_hh)"] <- "id_hh"

rm(common_hh_09_10)
 
## tests
filter(hh_data09, id_hh %in% hh_matched_2462$id_hh) %>% arrange(id_hh) %>% nrow() # hh_data09 with 2462 households
filter(hh_data10, id_hh %in% hh_matched_2462$id_hh) %>% arrange(id_hh) %>% nrow() # hh_data10 with 2462 households 
filter(hh_data11, id_hh %in% hh_matched_2462$id_hh) %>% arrange(id_hh) %>% nrow() # hh_data11 with 2462 households



####
#### Here, we want to keep (out of the 2462 households) only those who appear EACH year in section 15B, 15C, 15D of the questionnaire
####

# we discover in another file (analysis_general) that the final sample is of 2157 households
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")


# food
GSEC15b_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15B.csv") # 2931  
GSEC15b_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15b.csv") # 2657 households
GSEC15b_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15B.dta") # 2830

# non-durables
GSEC15c_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15C.csv") # 2752  
GSEC15c_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15c.csv")  # 2639 households
GSEC15c_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15C.dta") # 2824


# durables
GSEC15d_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15D.csv")   # 2992
GSEC15d_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15d.csv")  # 2511 
GSEC15d_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15D.dta") # 2829






# identify common id_hh
matched_09food_15B <- filter(GSEC15b_2009, HHID %in% final_vector_sample$id_hh) %>% arrange(HHID)
matched_10food_15B <- filter(GSEC15b_2010, hh %in% final_vector_sample$id_hh) %>% arrange(hh)
matched_11food_15B <- filter(GSEC15b_2011, HHID %in% final_vector_sample$id_hh) %>% arrange(HHID)

matched_09_15C <- filter(GSEC15c_2009, HHID %in% final_vector_sample$id_hh) %>% arrange(HHID)
matched_10_15C <- filter(GSEC15c_2010, hh %in% final_vector_sample$id_hh) %>% arrange(hh)
matched_11_15C <- filter(GSEC15c_2011, HHID %in% final_vector_sample$id_hh) %>% arrange(HHID)

matched_09_15D <- filter(GSEC15d_2009, HHID %in% final_vector_sample$id_hh) %>% arrange(HHID)
matched_10_15D <- filter(GSEC15d_2010, hh %in% final_vector_sample$id_hh) %>% arrange(hh)
matched_11_15D <- filter(GSEC15d_2011, HHID %in% final_vector_sample$id_hh) %>% arrange(HHID)


nrow(as.data.frame(unique(matched_09food_15B$HHID)))   # 2157
nrow(as.data.frame(unique(matched_10food_15B$hh)))   # 2157
nrow(as.data.frame(unique(matched_11food_15B$HHID)))   # 2157


nrow(as.data.frame(unique(matched_09_15C$HHID)))   # 2157
nrow(as.data.frame(unique(matched_10_15C$hh)))   # 2157
nrow(as.data.frame(unique(matched_11_15C$HHID)))   # 2157

nrow(as.data.frame(unique(matched_09_15D$HHID)))   # 2157
nrow(as.data.frame(unique(matched_10_15D$hh)))   # 2157
nrow(as.data.frame(unique(matched_11_15D$HHID)))   # 2157



######### SO, here are the three final data-sets

# 2009 --> with 2157 households
joined09_1 <- filter(joined09, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)
nrow(as.data.frame(unique(joined09_1$id_hh))) # 2157 OK!


# 2010 --> with 2157 households
joined10_1 <- filter(joined10, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)
nrow(as.data.frame(unique(joined10_1$id_hh))) # 2157 OK!

# 2011 --> with 2157 households
joined11_1 <- filter(joined11, id_hh %in% final_vector_sample$id_hh) %>% arrange(id_hh)
nrow(as.data.frame(unique(joined11_1$id_hh))) # 2157 OK!

write_csv(joined09_1, "/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2009.csv")
write_csv(joined10_1, "/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2010.csv")
write_csv(joined11_1, "/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2011.csv")


rm(hh_data09, hh_data10, hh_data11, char_data09, char_data10, char_data11, edu_data09, edu_data10, edu_data11, joined09, joined10, joined11)
rm(hh_matched_2462)
rm(GSEC15b_2009, GSEC15b_2010, GSEC15b_2011, GSEC15c_2009, GSEC15c_2010, GSEC15c_2011, GSEC15d_2009, GSEC15d_2010, GSEC15d_2011)
rm(matched_09food_15B, matched_10food_15B, matched_11food_15B, matched_09_15C, matched_10_15C, matched_11_15C, matched_09_15D, matched_10_15D, matched_11_15D)   




