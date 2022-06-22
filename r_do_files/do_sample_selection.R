##############
# Andrea Salem
#
# File: do_sample_selection
############## 
# Comments: This files determines the sample of households which will be used for analysis. Total sample: 2168 households
#
#
# Output of this file: final_vector_sample.csv

getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)

#############################

# STEP 1  --> Import GSEC15b, GSEC15c, and GSEC15d for each year (info. on consumption of food, non-durables, durables)



# i.	     Food and Beverages 
#############################


GSEC15b_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15B.csv") %>%  # 2931  
  rename(id_hh = HHID)

GSEC15b_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15b.csv") %>%  # 2657 households
  rename(id_hh = hh)

GSEC15b_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15B.dta") %>% # 2830
  rename(id_hh = HHID)


# ii.	  Non-Durable Goods and Frequently Purchased Services
###########################################################
#
#
# 30 days = 1 month = 4.345238 weeks # ----> coverts monthly to weekly (30/4.28)
#
# Divide final household consumption by 4.345238


GSEC15c_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15C.csv") %>%  # 2752  
  rename(id_hh = HHID)

GSEC15c_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15c.csv") %>%   # 2639 households
  rename(id_hh = hh)

GSEC15c_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15C.dta") %>% # 2824
  rename(id_hh = HHID)



# iii.	Semi-Durable Goods and Durable Goods and Service
########################################################
#
#
# 365 days = 12 months = 52.14286 weeks # ----> covert yearly to weekly (365/12/4.28)
#
# Divide final household consumption by 52.14286

GSEC15d_2009 <- read_csv("Data_raw/Uganda/2009-2010/UGA_2005-2009_UNPS_v02_M_CSV/2009/UNPS0910_HH/2009_GSEC15D.csv") %>%   # 2992
  rename(id_hh = HHID)


GSEC15d_2010 <- read_csv("Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/socio/GSEC15d.csv") %>%  # 2511 households
  rename(id_hh = hh)


GSEC15d_2011 <- read_dta("Data_raw/Uganda/2011-2012/stataUGA_2011_UNPS_v01_M_Stata/GSEC15D.dta") %>%  # 2829
  rename(id_hh = HHID)



# STEP 2  -->  for each wave, determine the households that appear in each consumption module


##########################################################################     
##                         SAMPLE SIZE 2009/10                          ##       
##########################################################################     


# In common between GSEC15b and GSEC15c
bXc <- as.data.frame(unique(filter(GSEC15b_2009, id_hh %in% GSEC15c_2009$id_hh)$id_hh)) #  2745
names(bXc)[names(bXc) == "unique(filter(GSEC15b_2009, id_hh %in% GSEC15c_2009$id_hh)$id_hh)"] <- "id_hh"

# In common between GSEC15c and GSEC15d
cXd <- as.data.frame(unique(filter(GSEC15c_2009, id_hh %in% GSEC15d_2009$id_hh)$id_hh)) #  2752
names(cXd)[names(cXd) == "unique(filter(GSEC15c_2009, id_hh %in% GSEC15d_2009$id_hh)$id_hh)"] <- "id_hh"

# In common between GSEC15d and GSEC15b
bXd <- as.data.frame(unique(filter(GSEC15d_2009, id_hh %in% GSEC15b_2009$id_hh)$id_hh)) #  2931
names(bXd)[names(bXd) == "unique(filter(GSEC15d_2009, id_hh %in% GSEC15b_2009$id_hh)$id_hh)"] <- "id_hh"


filter(cXd, id_hh %in% bXc$id_hh) %>% arrange(id_hh)  # 2745 
filter(bXd, id_hh %in% bXc$id_hh) %>% arrange(id_hh)  # 2745

vector_hh_2009 <- as.data.frame(filter(cXd, id_hh %in% bXc$id_hh)) %>% arrange(id_hh) # 2745



##########################################################################     
##                         SAMPLE SIZE 2010/11                          ##       
##########################################################################   


# In common between GSEC15b and GSEC15c
bXc <- as.data.frame(unique(filter(GSEC15b_2010, id_hh %in% GSEC15c_2010$id_hh)$id_hh))   # 2639
names(bXc)[names(bXc) == "unique(filter(GSEC15b_2010, id_hh %in% GSEC15c_2010$id_hh)$id_hh)"] <- "id_hh"

# In common between GSEC15c and GSEC15d
cXd <- as.data.frame(unique(filter(GSEC15c_2010, id_hh %in% GSEC15d_2010$id_hh)$id_hh))  # 2500
names(cXd)[names(cXd) == "unique(filter(GSEC15c_2010, id_hh %in% GSEC15d_2010$id_hh)$id_hh)"] <- "id_hh"

# In common between GSEC15d and GSEC15b
bXd <- as.data.frame(unique(filter(GSEC15d_2010, id_hh %in% GSEC15b_2010$id_hh)$id_hh))  # 2511
names(bXd)[names(bXd) == "unique(filter(GSEC15d_2010, id_hh %in% GSEC15b_2010$id_hh)$id_hh)"] <- "id_hh"


filter(bXc, id_hh %in% bXd$id_hh) %>% arrange(id_hh) %>% nrow()  # 2500 
filter(bXd, id_hh %in% cXd$id_hh) %>% arrange(id_hh)  %>% nrow() # 2500

vector_hh_2010 <- as.data.frame(filter(cXd, id_hh %in% bXc$id_hh)) %>% arrange(id_hh) # 2500



##########################################################################     
##                         SAMPLE SIZE 2011/12                          ##       
##########################################################################   


# In common between GSEC15b and GSEC15c
bXc <- as.data.frame(unique(filter(GSEC15b_2011, id_hh %in% GSEC15c_2011$id_hh)$id_hh))   # 2819
names(bXc)[names(bXc) == "unique(filter(GSEC15b_2011, id_hh %in% GSEC15c_2011$id_hh)$id_hh)"] <- "id_hh"

# In common between GSEC15c and GSEC15d
cXd <- as.data.frame(unique(filter(GSEC15c_2011, id_hh %in% GSEC15d_2011$id_hh)$id_hh))   # 2817
names(cXd)[names(cXd) == "unique(filter(GSEC15c_2011, id_hh %in% GSEC15d_2011$id_hh)$id_hh)"] <- "id_hh"

# In common between GSEC15d and GSEC15b  
bXd <- as.data.frame(unique(filter(GSEC15d_2011, id_hh %in% GSEC15b_2011$id_hh)$id_hh))   # 2822
names(bXd)[names(bXd) == "unique(filter(GSEC15d_2011, id_hh %in% GSEC15b_2011$id_hh)$id_hh)"] <- "id_hh"


filter(bXd, id_hh %in% cXd$id_hh) %>% arrange(id_hh) %>% nrow() # 2812 
filter(cXd, id_hh %in% bXc$id_hh) %>% arrange(id_hh) %>% nrow() # 2812

vector_hh_2011 <- as.data.frame(filter(cXd, id_hh %in% bXc$id_hh)) %>% arrange(id_hh) # 2812


rm(bXc, bXd, cXd)
# rm(GSEC15b_2009, GSEC15b_2010, GSEC15b_2011, GSEC15c_2009, GSEC15c_2010, GSEC15c_2011, GSEC15d_2009, GSEC15d_2010, GSEC15d_2011)


# STEP 3  -->  match households that appear each year in each consumption module: Final sample is of 2157

final_vector_sample <- as.data.frame(intersect(vector_hh_2009$id_hh,vector_hh_2010$id_hh))
names(final_vector_sample)[names(final_vector_sample) == "intersect(vector_hh_2009$id_hh, vector_hh_2010$id_hh)"] <- "id_hh"

final_vector_sample <- as.data.frame(intersect(final_vector_sample$id_hh, vector_hh_2011$id_hh))
names(final_vector_sample)[names(final_vector_sample) == "intersect(final_vector_sample$id_hh, vector_hh_2011$id_hh)"] <- "id_hh"  # 2157. Final Sample of households


rm(vector_hh_2009, vector_hh_2010, vector_hh_2011)

write_csv(final_vector_sample, "/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv")  # 2157 households

rm(GSEC15b_2009, GSEC15b_2010, GSEC15b_2011, GSEC15c_2009, GSEC15c_2010, GSEC15c_2011, GSEC15d_2009, GSEC15d_2010, GSEC15d_2011)



