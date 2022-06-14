##############
# Andrea Salem
#
# File: do_hh2009
############## 
# Comments: -



########################################################################
########################################################################
##                                                                    ##       
##               National Panel Survey 2009-2010                      ##       
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


########### do_hh

# household_id, weight, comm (EAs), urban
GSEC1_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC1.csv")      #2009_GSEC1.csv       


clean_hh2009 <- mutate(GSEC1_2009,
                       id_hh = HHID)
                       

clean_hh2009 <- select(clean_hh2009,id_hh, wgt09, comm, urban, regurb)

rm(GSEC1_2009)

write_csv(clean_hh2009, "/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2009.csv")




