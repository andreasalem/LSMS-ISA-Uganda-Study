######################### 
#
#
#
######################### 
######################### Table of Contents

###### --- TABLE 3.1...........Descriptive Statistics of roster dataset (2009-2010-2011)

###### --- FIGURE 3.1..........Sample of Households Study
###### --- FIGURE 3.2..........Distribution of age (2009-2010-2011)
###### --- Appendix A.1......CDF of distribution of age (2009-2010-2011)


######################### Data required (i.e., roster datatsets)

# matched_hh_2009.csv
# matched_hh_2010.csv
# matched_hh_2011.csv


######################### Code
getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
library(Hmisc) #used for weighted mean and standard deviation

options(scipen=999) # avoids scientific notation

#################
################# HERE we import the dataset with household members socio-demographic characteristics

hh_roster09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2009.csv") # 2157
hh_roster10 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2010.csv") # 2157 
hh_roster11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2011.csv") # 2157

################################################################### 
################################################################### TABLE 3.1 - Descriptive Statistics 
##################
################## 2009
################## 

# Head of household is male
mean_head_sex09 <- wtd.mean(filter(hh_roster09, ind_position == 1)$ind_sex, filter(hh_roster09, ind_position == 1)$wgt09, na.rm = T)
sd_head_sex09 <- sqrt(wtd.var(filter(hh_roster09, ind_position == 1)$ind_sex, filter(hh_roster09, ind_position == 1)$wgt09, na.rm = T))


# Age of head of household
mean_age_head09 <- wtd.mean(filter(hh_roster09, ind_position == 1)$ind_age, filter(hh_roster09, ind_position == 1)$wgt09, na.rm = T)
sd_age_head09   <- sqrt(wtd.var(filter(hh_roster09, ind_position == 1)$ind_age, filter(hh_roster09, ind_position == 1)$wgt09, na.rm = T))


# average years of schooling of head
mean_educ_head09 <- wtd.mean(filter(hh_roster09, ind_position == 1)$ind_education_years, filter(hh_roster09, ind_position== 1)$wgt09, na.rm = T)
sd_educ_head09 <- sqrt(wtd.var(filter(hh_roster09, ind_position == 1)$ind_education_years, filter(hh_roster09, ind_position == 1)$wgt09, na.rm = T))


# Household Size

# data with 2402 households  
mean_hh_size09 <- wtd.mean(hh_roster09$n_ind_household, hh_roster09$wgt09, na.rm = T)       
sd_hh_size09   <- sqrt(wtd.var(hh_roster09$n_ind_household, hh_roster09$wgt09, na.rm = T))

# Average age of household
mean_age09 <- wtd.mean(hh_roster09$ind_age, hh_roster09$wgt09, na.rm = T)
sd_age09   <- sqrt(wtd.var(hh_roster09$ind_age, hh_roster09$wgt09, na.rm = T))

# Urban dummy
mean_urban09 <- wtd.mean(hh_roster09$urban, hh_roster09$wgt09, na.rm = T)
sd_urban09  <- sqrt(wtd.var(hh_roster09$urban, hh_roster09$wgt09, na.rm = T))


##################
################## 2010
################## 

# Head of household is male (1 = male; 0 = female)
mean_head_sex10 <- wtd.mean(filter(hh_roster10, ind_position == 1)$ind_sex, filter(hh_roster10, ind_position == 1)$wgt10, na.rm = T)
sd_head_sex10  <- sqrt(wtd.var(filter(hh_roster10, ind_position == 1)$ind_sex, filter(hh_roster10, ind_position == 1)$wgt10, na.rm = T))


# Age of head of household
mean_age_head10  <- wtd.mean(filter(hh_roster10, ind_position == 1)$ind_age, filter(hh_roster10, ind_position == 1)$wgt10, na.rm = T)
sd_age_head10    <- sqrt(wtd.var(filter(hh_roster10, ind_position == 1)$ind_age, filter(hh_roster10, ind_position == 1)$wgt10, na.rm = T))

# average years of schooling of head
mean_educ_head10 <- wtd.mean(filter(hh_roster10, ind_position == 1)$ind_education_years, filter(hh_roster10, ind_position== 1)$wgt10, na.rm = T)
sd_educ_head10 <- sqrt(wtd.var(filter(hh_roster10, ind_position == 1)$ind_education_years, filter(hh_roster10, ind_position == 1)$wgt10, na.rm = T))

# Household Size

## data with 2402 households
mean_hh_size10  <- wtd.mean(hh_roster10$n_ind_household, hh_roster10$wgt10, na.rm = T)        
sd_hh_size10    <- sqrt(wtd.var(hh_roster10$n_ind_household, hh_roster10$wgt10, na.rm = T))


# Average age of household
mean_age10  <- wtd.mean(hh_roster10$ind_age, hh_roster10$wgt10, na.rm = T)
sd_age10    <- sqrt(wtd.var(hh_roster10$ind_age, hh_roster10$wgt10, na.rm = T))

# Urban dummy
mean_urban10  <- wtd.mean(hh_roster10$urban, hh_roster10$wgt10, na.rm = T)
sd_urban10    <- sqrt(wtd.var(hh_roster10$urban, hh_roster10$wgt10, na.rm = T))


##################
################## 2011
################## 


# Head of household is male
mean_head_sex11 <- wtd.mean(filter(hh_roster11, ind_position == 1)$ind_sex, filter(hh_roster11, ind_position == 1)$wgt11, na.rm = T)
sd_head_sex11 <- sqrt(wtd.var(filter(hh_roster11, ind_position == 1)$ind_sex, filter(hh_roster11, ind_position == 1)$wgt11, na.rm = T))


# Age of head of household
mean_age_head11 <- wtd.mean(filter(hh_roster11, ind_position == 1)$ind_age, filter(hh_roster11, ind_position == 1)$wgt11, na.rm = T)
sd_age_head11   <- sqrt(wtd.var(filter(hh_roster11, ind_position == 1)$ind_age, filter(hh_roster11, ind_position == 1)$wgt11, na.rm = T))

# average years of schooling of head
mean_educ_head11 <- wtd.mean(filter(hh_roster11, ind_position == 1)$ind_education_years, filter(hh_roster11, ind_position== 1)$wgt11, na.rm = T)
sd_educ_head11 <- sqrt(wtd.var(filter(hh_roster11, ind_position == 1)$ind_education_years, filter(hh_roster11, ind_position == 1)$wgt11, na.rm = T))



# Household Size
mean_hh_size11 <- wtd.mean(hh_roster11$n_ind_household, hh_roster11$wgt11, na.rm = T)       
sd_hh_size11   <- sqrt(wtd.var(hh_roster11$n_ind_household, hh_roster11$wgt11, na.rm = T))


# Average age of household
mean_age11 <- wtd.mean(hh_roster11$ind_age, hh_roster11$wgt11, na.rm = T)
sd_age11   <- sqrt(wtd.var(hh_roster11$ind_age, hh_roster11$wgt11, na.rm = T))

# Urban dummy
mean_urban11 <- wtd.mean(hh_roster11$urban, hh_roster11$wgt11, na.rm = T)
sd_urban11 <- sqrt(wtd.var(hh_roster11$urban, hh_roster11$wgt11, na.rm = T))



################################################################### 
################################################################### FIGURE 3.1 - Sample of Households Study




#### MAP OF STUDY SAMPLE, with residence of the 2157 households in 2011/12
geo_data <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_raw/Uganda/2010-2011/UGA_2010_UNPS_v02_M_CSV/Socio/UNPS_Geovars_1011.csv") 
final_vector_sample <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/final_vector_sample.csv") # sample of 2167 households

geo_data <- geo_data %>% 
  filter(HHID %in% final_vector_sample$id_hh) %>% 
  select(latitude, longitude, urban)

names(geo_data)[names(geo_data) == "lon_mod"] <- "longitude"
names(geo_data)[names(geo_data) == "lat_mod"] <- "latitude"

write_csv(geo_data, "/Users/andreasalem/R_oba/Thesis/graphs/long_lat.csv")

### This data is then imported into Datawrapper (a online service) to create the map. Link: https://www.datawrapper.de/
### We then overlay this map on a background map downloaded from this link: https://commons.wikimedia.org/wiki/File:Uganda_location_map.svg
### We use a simple software online to remove the background from the first map. For example: https://www9.lunapic.com/editor/?action=transparent
###


#### GOOGLE MAPS alternative
library("RgoogleMaps")

png(filename = "wiki_uganda.png", width=880, height=880)

lat    = c(-1,3.5)
lon    = c(30,34)
center = c(mean(lat), mean(lon))
zoom <- min(MaxZoom(range(lat), range(lon)))

MyMap <- GetMap(center   = center, 
                zoom     = zoom,     
                markers  = '&40.702147,-74.015794,blues%7C40.711614,-74.012318,greeng%7C40.718217,-73.998284,redc', 
                destfile = 'original.png');

tmp <- PlotOnStaticMap(MyMap, 
                       lat = c(40.702147, 40.711614, 40.718217), 
                       lon = c(-74.015794,-74.012318,-73.998284), 
                       cex = 1.5, 
                       pch = 20, 
                       col = c('red', 'blue', 'green'), 
                       add = F)














################################################################### 
################################################################### FIGURE 3.2 - Descriptive Statistics: Age distribution (need roster for each year!)

##################
################## Plot population age distribution for each year. 
################## I use data from other years here!


library(AMR)

# 2009
hh_roster09 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2009.csv")
hh_roster09 <- hh_roster09 %>% 
  mutate(ind_age_group = age_groups(hh_roster09$ind_age, 1:20 * 5))

# 2010
hh_roster10 <- hh_roster10 %>% 
  mutate(ind_age_group = age_groups(hh_roster10$ind_age, 1:20 * 5))

# 2011
hh_roster11 <- read_csv("/Users/andreasalem/R_oba/Thesis/Data_clean/matched_hh_2011.csv")
hh_roster11 <- hh_roster11 %>% 
  mutate(ind_age_group = age_groups(hh_roster11$ind_age, 1:20 * 5))



graph_age <- hh_roster09 %>%
  group_by(ind_age_group) %>%
  summarise(n = n()) %>%
  
  mutate(freq09 = n / sum(n)) %>% 
  mutate(freq10 = as.numeric(unlist(select(mutate(summarise(group_by(hh_roster10, ind_age_group), n = n()), freq10 = n / sum(n)), freq10)))) %>% 
  mutate(freq11 = as.numeric(unlist(select(mutate(summarise(group_by(hh_roster11, ind_age_group), n = n()), freq11 = n / sum(n)), freq11))))



dev.new(width=5, height=4) # remove to view avoid new window of plot


# select first 21 rows (since since last row was for count of NAs) and plot


ggplot(graph_age[1:21,], aes(x = ind_age_group, group = 1)) + 
  geom_line(aes( y = freq09, colour = "2009/2010")) +
  geom_point(aes( y = freq09, colour = "2009/2010")) +
  geom_line(aes( y = freq10, colour = "2010/2011")) +
  geom_point(aes( y = freq10, colour = "2010/2011")) +
  geom_line(aes( y = freq11, colour = "2011/2012")) +
  geom_point(aes( y = freq11, colour = "2011/2012")) +
  xlab("age cohort")+
  theme_minimal() + 
  theme( axis.text.x = element_text(size=9, angle = 90)) +
  theme( axis.text.y = element_text(size=9)) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  ylab("%") 


########## ################################################## Figure A.1 - CDF of age distribution
q0 <- data.frame(hh_roster09$ind_age)
q0$year_09 <- 2009

q1 <- data.frame(hh_roster10$ind_age)
q1$year_10 <- 2010

q2 <- data.frame(hh_roster11$ind_age)
q2$year_11 <- 2011

df2 <- data.frame(value = c(q0[,"hh_roster09.ind_age"], q1[,"hh_roster10.ind_age"], q2[,"hh_roster11.ind_age"]), 
                  year = as.factor(c(q0[,"year_09"], q1[,"year_10"], q2[,"year_11"])))

rm(q0, q1, q2)

ggplot(df2, aes(value, colour = year)) +
  stat_ecdf() +
  xlab("age in years")+
  theme_minimal() + 
  theme(legend.position="bottom", legend.title = element_blank()) +
  ylab("") +
  scale_x_discrete(limits=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  theme( axis.text.y = element_text(size=9)) +
  theme( axis.text.x = element_text(size=9)) 
##################################################
rm(df2, graph_age)





