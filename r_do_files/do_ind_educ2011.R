##############
# Andrea Salem
#
# File: do_ind_educ2011
############## 
# Comments: -



########################################################################
########################################################################
##                                                                    ##       
##               National Panel Survey 2011-2012                      ##       
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



###########  do_education

GSEC4_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC4.dta")     #GSEC4

# h4q4, h4q5, h4q7, h4q9
ind_educ11 <- select(GSEC4_2011, HHID, PID, h4q4, h4q5, h4q7, h4q9)
ind_educ11 <- mutate(ind_educ11,
                     id_hh = HHID,
                     id_ind = PID)

# ind_read + ind_write + ind_literacy + ind_any_schooling (1 = yes,; 2 = no) + ind_school + ind_education
ind_educ11<-mutate(ind_educ11,
                      ind_read = case_when(h4q4 == 1~ 2,
                                           h4q4 == 2~ 1,
                                           h4q4 == 3~ 2,
                                           h4q4 == 4~ 1),
                      ind_write = case_when(h4q4 == 1~ 2,
                                            h4q4 == 2~ 2,
                                            h4q4 == 3~ 1,
                                            h4q4 == 4~ 1),
                      ind_literacy = case_when(h4q4 == 1~ 2,
                                               h4q4 == 2~ 2,
                                               h4q4 == 3~ 2,
                                               h4q4 == 4~ 1),
                      ind_any_schooling = case_when(h4q5 == 1~ 2, 
                                                    h4q5 == 2~ 1,
                                                    h4q5 == 3~ 1),
                      ind_school = case_when(h4q5 == 1~ 2, 
                                             h4q5 == 2~ 2,
                                             h4q5 == 3~ 1),
                      ind_education = case_when(ind_any_schooling == 2 ~ NA_real_,
                                                h4q7 == 10 ~ 0,
                                                between(h4q7,11,17) ~ 1,
                                                between(h4q7,21,33) ~ 2,
                                                between(h4q7,34,36) ~ 3,
                                                h4q7 == 41 ~ 4,
                                                h4q7 == 51 ~ 4,
                                                h4q7 == 61 ~ 5,
                                                h4q7 == 99 ~ 0))

ind_educ11$ind_education["ind_school" == 1] = NA                                                         

# ind_education_years
ind_educ11<-mutate(ind_educ11,
                      ind_education_years = case_when(h4q9 == 1 ~ 0, 
                                                      h4q9 == 10 ~ 1, 
                                                      h4q9 == 11 ~ 2,
                                                      h4q9 == 12 ~ 3,
                                                      h4q9 == 13 ~ 4,
                                                      h4q9 == 14 ~ 5,
                                                      h4q9 == 15 ~ 6,
                                                      h4q9 == 16 ~ 7,
                                                      h4q9 == 17 ~ NA_real_,
                                                      h4q9 == 36 ~ NA_real_,
                                                      h4q9 == 30 ~ 8,
                                                      h4q9 == 31 ~ 9,
                                                      h4q9 == 32 ~ 10,
                                                      h4q9 == 33 ~ 11,
                                                      h4q9 == 34 ~ 12,
                                                      h4q9 == 35 ~ 13,
                                                      h4q9 == 40 ~ 12,
                                                      h4q9 == 50 ~ 12,
                                                      h4q9 == 61 ~ 15,
                                                      h4q9 == 99 ~ NA_real_,
                                                      ind_school == 1 ~ NA_real_,
                                                      ind_any_schooling == 2 ~ NA_real_))

#
clean_ind_edu11 <- ind_educ11[,c("id_hh", "id_ind", "ind_read", "ind_write", "ind_literacy", "ind_any_schooling", "ind_school", "ind_education", "ind_education_years")]

rm(GSEC4_2011, ind_educ11)

write_csv(clean_ind_edu11, "/Users/andreasalem/R_oba/Thesis/Data_clean/hh_educ2011.csv")



