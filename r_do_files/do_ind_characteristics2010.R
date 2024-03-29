##############
# Andrea Salem
#
# File: do_ind_characteristics2010
############## 
# Comments: -



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


options(scipen=999) # avoids scientific notation



###########  do_characteristics

GSEC2_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC2.csv")     #GSEC2.csv


# h2q3 = sex (0 = female; 1 = male), h2q4= position,  h2q8 = age, h2q10 = marital status, 
ind_char10 <- mutate(GSEC2_2010,
                              id_hh = HHID,
                              id_ind = PID, 
                              ind_sex = case_when(h2q3 == 0~0,
                                                  h2q3 == 1~1),
                              ind_age = h2q8) 

# ind_position
ind_char10<-mutate(ind_char10,
                            ind_position = case_when(h2q4 == 1~ 1,
                                                     h2q4 == 2~ 2,
                                                     h2q4 == 3~ 3,
                                                     h2q4 == 4~ 4,
                                                     h2q4 >= 5~ 5))

# ind_permanent
ind_char10 <- mutate(ind_char10, 
                              ind_permanent = if_else(ind_char10$h2q5 > 4, 1,2, missing = NULL)) # 1 = permanent

# ind_marital_status
ind_char10<-mutate(ind_char10,
                            ind_marital_status = case_when(h2q10 == 1~ 1,
                                                           h2q10 == 2~ 1,
                                                           h2q10 == 3~ 2,
                                                           h2q10 == 4~ 2,
                                                           h2q10 == 5~ 3))

# n_ind_household

ind_char10<-ind_char10 %>% 
  group_by(id_hh) %>% 
  mutate(n_ind_household = n()) %>% 
  ungroup

# n_child_15 & n_child_5
ind_char10<-mutate(group_by(ind_char10, id_hh),
                            n_child_15 = sum(ind_age < 15, na.rm =T),
                            n_child_5 = sum(ind_age < 5, na.rm =T))


# keep only useful variables
clean_ind_char10 <-ind_char10[,c("id_hh", "id_ind", "ind_sex", "ind_position", "ind_age", "ind_marital_status", "n_ind_household", "n_child_15", "n_child_5")]

rm(GSEC2_2010, ind_char10)

write_csv(clean_ind_char10, "/Users/andreasalem/R_oba/Thesis/Data_clean/hh_charact2010.csv")


