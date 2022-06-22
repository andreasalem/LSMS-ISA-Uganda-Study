##############
# Andrea Salem
#
# File: do_hh2010
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


########### do_hh

# household_id, weight, comm (EAs), urban
GSEC1_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC1.csv")     #GSEC1.csv

clean_hh2010 <- mutate(GSEC1_2010,
                   id_hh = HHID)

clean_hh2010 <- select(clean_hh2010, id_hh, wgt10, comm, urban, regurb)

rm(GSEC1_2010)

write_csv(clean_hh2010, "/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2010.csv")

