##############
# Andrea Salem
#
# File: do_hh2011
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


########### do_hh

# household_id, weight, comm (EAs), urban

GSEC1_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC1.dta")

clean_hh2011 <- mutate(GSEC1_2011,
                   id_hh = HHID,
                   wgt11 = mult)

clean_hh2011 <- select(clean_hh2011, id_hh, wgt11, comm, urban, regurb)

rm(GSEC1_2011)

write_csv(clean_hh2011, "/Users/andreasalem/R_oba/Thesis/Data_clean/hh_2011.csv")

