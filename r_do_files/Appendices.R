######################### 
###############
# Andrea Salem
#
# File: do_analysis11
############## 
# Comments: -
#
#
######################### 
######################### Table of Contents

###### --- FIGURE A.2 ..........Relative shares of food expenditure by food sources and along quintiles
###### --- FIGURE A.3 ..........Box-plots of total consumption expenditure 
###### --- FIGURE A.4 ..........Food consumption basket structure by regional setting 
###### --- FIGURE A.5 ..........Distribution of urban and rural food purchases 2011/2012
###### --- FIGURE A.6 ..........LOESS lines of best fit of relative expenditure shares over the distribution of wealth


getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
library(Hmisc) #used for weighted standard deviation

options(scipen=999) # avoids scientific notation

### plots

library(ggplot2)
library(hrbrthemes)
library(tidyr)

######################### Data required 

result_2009 <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2009.csv")
result_2010 <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2010.csv")
result_2011 <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2011.csv")





########## ########## ########## ########## ########## ########## ########## ########## ########## ########## 
##########    Pie chart food basket - FIGURE A.4 (only for 2010)
##########
##########
##########


source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_food_2011.R") # we need just final_dataset11_food
rm(b1_food11, price_list11_food)

####################################### food consumption, analysis of each category  --> Pie Chart

# prepare data 2010
graph_food_categories10 <- final_dataset11_food %>% group_by(case_id, urban, hh_g02_os1) %>% 
  summarise(tot = sum(tot_quantity_and_price, na.rm = T)) %>% 
  group_by(urban, hh_g02_os1) %>% 
  summarise(tot = sum(tot)) %>% 
  spread(key = "urban", value = "tot") %>% 
  rename(rural = "0",
         urban = "1") %>% 
  mutate(rural_share = rural*100/109027494,
         urban_share = urban*100/32795363)


graph_food_categories10 <- cbind(graph_food_categories10, filter(final_dataset11_food, is.na(hh_g02_os1) ==F) %>% group_by(hh_g02_os1) %>% 
                                   summarise(tot_national = sum(tot_quantity_and_price, na.rm = T)) %>% 
                                   mutate(tot_national_share = tot_national*100/sum(tot_national)) %>% 
                                   select(tot_national, tot_national_share)) 


graph_food_categories10 <- graph_food_categories10[2:13,]

graph_food_categories10 <- cbind(graph_food_categories10,filter(final_dataset11_food, is.na(hh_g02_os1) ==F) %>% group_by(hh_g02_os1) %>% 
                                   summarise(tot_national = sum(tot_quantity_and_price, na.rm = T)) %>% 
                                   mutate(tot_national_share = tot_national*100/sum(tot_national)) %>% 
                                   select(tot_national, tot_national_share)) 





# check
sum(graph_food_categories10$rural, na.rm =T) + sum(graph_food_categories10$urban, na.rm =T)
sum(graph_food_categories10$tot_national)
sum(graph_food_categories10$tot_national_share) # OK!


# plot rural

ggplot(graph_food_categories10[,1:3], aes(x="", y=rural, fill=hh_g02_os1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# plot urban --> more diversified!
ggplot(graph_food_categories10[,1:3], aes(x="", y=urban, fill=hh_g02_os1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)






#########################################################
######### Share of food purchased, home-produced or received for free, by quintile - FIGURE A.2

#### 
#### 2009
#### 

# food production
graph_quantiles_food09 <- result_2009 %>% group_by(category_quantile) %>% 
  summarise(tot_food = sum(total_consumption_food, na.rm = T),
            purchases = sum(purchases, na.rm = T),
            "home production" = sum(home_prod, na.rm = T),
            "gits/in-kind sources" = sum(gifts_and_other, na.rm = T)) # WIDE
# LONG format
graph_quantiles_food09 <- gather(graph_quantiles_food09, category, total, tot_food:'gits/in-kind sources', factor_key = T) %>% 
  mutate(total_perc = case_when(category_quantile == 1 ~ total*100/19358170,
                                category_quantile == 2 ~ total*100/26801390,
                                category_quantile == 3 ~ total*100/35346248,
                                category_quantile == 4 ~ total*100/84089757,
                                category_quantile == 5 ~ total*100/4248832))
# plot 1
ggplot(graph_quantiles_food09[6:20,], aes( x = category_quantile, y = total_perc, fill = category)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )

# plot 2
x_labels <- c( "purch", "home_prod", "gifts/other")
label_names <- c( "1" = "First Quantile",
                  "2" = "Second Quantile",
                  "3" = "Third Quantile",
                  "4" = "Fourth Quantile",
                  "5" = "Fifth Quantile")

ggplot(graph_quantiles_food09[6:20,], aes(x = category, y = total_perc / 100, fill = category) ) +
  geom_bar( stat = "identity", width = 0.75, color = "#2b2b2b", size = 0.05 ) + 
  scale_y_continuous(limits = c( 0, 0.7 ) ) + 
  scale_x_discrete( expand = c( 0, 1 ), labels = NULL ) +  # here x_labels if I want
  scale_fill_manual( values = c( "#a6cdd9", "#d2e4ee", "#b7b079", "#efc750" ) ) +
  facet_wrap( ~ category_quantile, labeller = as_labeller(label_names) ) +
  labs( x = NULL, y = NULL, title = "Quantiles and food sources 2009" ) +
  theme( strip.text = element_text( size = 12, color = "white", hjust = 0.5 ),
         strip.background = element_rect( fill = "#858585", color = NA ),    
         panel.background = element_rect( fill = "#efefef", color = NA ),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_line( color = "#b2b2b2" ),
         panel.spacing.x = unit( 1, "cm" ),
         panel.spacing.y = unit( 0.5, "cm" ),
         legend.position =  "bottom") 


#### same but for rural
graph_quantiles_food09_rural <- filter(result_2009, urban ==0) %>% group_by(category_quantile) %>% 
  summarise(tot_food = sum(total_consumption_food, na.rm = T),
            purchases = sum(purchases, na.rm = T),
            "home production" = sum(home_prod, na.rm = T),
            "gits/in-kind sources" = sum(gifts_and_other, na.rm = T)) # WIDE


# LONG format
graph_quantiles_food09_rural <- gather(graph_quantiles_food09_rural, category, total, tot_food:'gits/in-kind sources', factor_key = T) %>% 
  mutate(total_perc = case_when(category_quantile == 1 ~ total*100/17003848,
                                category_quantile == 2 ~ total*100/22282534,
                                category_quantile == 3 ~ total*100/28287664,
                                category_quantile == 4 ~ total*100/59691677,
                                category_quantile == 5 ~ total*100/2727371))

# plot 1
ggplot(graph_quantiles_food09_rural[6:20,], aes( x = category_quantile, y = total_perc, fill = category)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )

#### same but for urban
graph_quantiles_food09_urban <- filter(result_2009, urban ==1) %>% group_by(category_quantile) %>% 
  summarise(tot_food = sum(total_consumption_food, na.rm = T),
            purchases = sum(purchases, na.rm = T),
            "home production" = sum(home_prod, na.rm = T),
            "gits/in-kind sources" = sum(gifts_and_other, na.rm = T)) # WIDE


# LONG format
graph_quantiles_food09_urban <- gather(graph_quantiles_food09_urban, category, total, tot_food:'gits/in-kind sources', factor_key = T) %>% 
  mutate(total_perc = case_when(category_quantile == 1 ~ total*100/2354322,
                                category_quantile == 2 ~ total*100/4518855,
                                category_quantile == 3 ~ total*100/7058584,
                                category_quantile == 4 ~ total*100/24398079,
                                category_quantile == 5 ~ total*100/1521461))

# plot 1
ggplot(graph_quantiles_food09_urban[6:20,], aes( x = category_quantile, y = total_perc, fill = category)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )


#### 
#### 2010
#### 

# use either category_quantile_food, or category_quantile
##############

# food production
graph_quantiles_food <- result_2010 %>% group_by(category_quantile) %>% 
  summarise(tot_food = sum(total_consumption_food, na.rm = T),
            purchases = sum(purchases_food, na.rm = T),
            "home production" = sum(home_prod_food, na.rm = T),
            "gits/in-kind sources" = sum(gifts_and_other_food, na.rm = T)) # WIDE



# LONG format
graph_quantiles_food <- gather(graph_quantiles_food, category, total, tot_food:'gits/in-kind sources', factor_key = T) %>% 
  mutate(total_perc = case_when(category_quantile == 1 ~ total*100/as.numeric(graph_quantiles_food[1,2]), # 
                                category_quantile == 2 ~ total*100/as.numeric(graph_quantiles_food[2,2]),
                                category_quantile == 3 ~ total*100/as.numeric(graph_quantiles_food[3,2]),
                                category_quantile == 4 ~ total*100/as.numeric(graph_quantiles_food[4,2]),
                                category_quantile == 5 ~ total*100/as.numeric(graph_quantiles_food[5,2])))

# plot 1
ggplot(graph_quantiles_food[6:20,], aes( x = category_quantile, y = total_perc, fill = category)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )

# plot 2
x_labels <- c( "purch", "home_prod", "gifts/other")
label_names <- c( "1" = "First Quantile",
                  "2" = "Second Quantile",
                  "3" = "Third Quantile",
                  "4" = "Fourth Quantile",
                  "5" = "Fifth Quantile")


ggplot(graph_quantiles_food[6:20,], aes(x = category, y = total_perc / 100, fill = category) ) +
  geom_bar( stat = "identity", width = 0.75, color = "#2b2b2b", size = 0.05 ) + 
  scale_y_continuous(limits = c( 0, 0.7 ) ) + 
  scale_x_discrete( expand = c( 0, 1 ), labels = NULL ) +  # here x_labels if I want
  scale_fill_manual( values = c( "#a6cdd9", "#d2e4ee", "#b7b079", "#efc750" ) ) +
  facet_wrap( ~ category_quantile, labeller = as_labeller(label_names) ) +
  labs( x = NULL, y = NULL, title = "Quantiles and food sources 2010" ) +
  theme( strip.text = element_text( size = 12, color = "white", hjust = 0.5 ),
         strip.background = element_rect( fill = "#858585", color = NA ),    
         panel.background = element_rect( fill = "#efefef", color = NA ),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_line( color = "#b2b2b2" ),
         panel.spacing.x = unit( 1, "cm" ),
         panel.spacing.y = unit( 0.5, "cm" ),
         legend.position =  "bottom") 

#### 
#### 2011
#### 

# food production
graph_quantiles_food11 <- result_2011 %>% group_by(category_quantile) %>% 
  summarise(tot_food = sum(total_consumption_food, na.rm = T),
            purchases = sum(purchases, na.rm = T),
            "home production" = sum(home_prod, na.rm = T),
            "gits/in-kind sources" = sum(gifts_and_other, na.rm = T)) # WIDE


# LONG format
graph_quantiles_food11 <- gather(graph_quantiles_food11, category, total, tot_food:'gits/in-kind sources', factor_key = T) %>% 
  mutate(total_perc = case_when(category_quantile == 1 ~ total*100/21788847,
                                category_quantile == 2 ~ total*100/26346964,
                                category_quantile == 3 ~ total*100/39132818,
                                category_quantile == 4 ~ total*100/96429865,
                                category_quantile == 5 ~ total*100/25553408))


# plot 1
ggplot(graph_quantiles_food11[6:20,], aes( x = category_quantile, y = total_perc, fill = category)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )

# plot 2
x_labels <- c( "purch", "home_prod", "gifts/other")
label_names <- c( "1" = "First Quantile",
                  "2" = "Second Quantile",
                  "3" = "Third Quantile",
                  "4" = "Fourth Quantile",
                  "5" = "Fifth Quantile")


ggplot(graph_quantiles_food11[6:20,], aes(x = category, y = total_perc / 100, fill = category) ) +
  geom_bar( stat = "identity", width = 0.75, color = "#2b2b2b", size = 0.05 ) + 
  scale_y_continuous(limits = c( 0, 0.7 ) ) + 
  scale_x_discrete( expand = c( 0, 1 ), labels = NULL ) +  # here x_labels if I want
  scale_fill_manual( values = c( "#a6cdd9", "#d2e4ee", "#b7b079", "#efc750" ) ) +
  facet_wrap( ~ category_quantile, labeller = as_labeller(label_names) ) +
  labs( x = NULL, y = NULL, title = "Quantiles and food sources 2009" ) +
  theme( strip.text = element_text( size = 12, color = "white", hjust = 0.5 ),
         strip.background = element_rect( fill = "#858585", color = NA ),    
         panel.background = element_rect( fill = "#efefef", color = NA ),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_line( color = "#b2b2b2" ),
         panel.spacing.x = unit( 1, "cm" ),
         panel.spacing.y = unit( 0.5, "cm" ),
         legend.position =  "bottom") 




#########################################################
######### BOX-PLOTS of TOTAL CONSUMPTION (preliminary work, check for outliers) - FIGURE A.3

#### 2009
boxplot(result_2009$TOT_consumption) # there definitively are outliers
tail(sort(result_2009$TOT_consumption),20)

#### 2010
boxplot(result_2010$TOT_consumption) # there definitively are outliers
result_2010[order(desc(result_2010$TOT_consumption)),]      # %>%  view() 
tail(sort(result_2010$TOT_consumption),20)

#### 2011
boxplot(result_2011$TOT_consumption) # there definitively are outliers
tail(sort(result_2011$TOT_consumption),20)


#########################################################
######### Check various distributions

#### 
#### 2009
#### 
# Total consumption 
ggplot(result_2009, aes(x=TOT_consumption)) +
  geom_density() +
  xlim(c(0, 600000))

# urban vs. rural in Total Consumption
ggplot(data=result_2009, aes(x=TOT_consumption, group=factor(urban), fill=factor(urban))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  xlim(c(0, 400000))


#### 
#### 2010
#### 
# Total consumption                ‹ as I do in result 2, I already check the distribution since I show mean total consumption by quintile!
ggplot(result_2010, aes(x=TOT_consumption)) +
  geom_density() +
  xlim(c(0, 600000))
# urban vs. rural in Total Consumption
ggplot(data=result_2010, aes(x=TOT_consumption, group=factor(urban), fill=factor(urban))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  xlim(c(0, 800000))


#### 
#### 2011
#### 

# total consumption 
ggplot(result_2011, aes(x=TOT_consumption)) +
  geom_density() +
  xlim(c(0, 600000))

# urban vs. rural in Total Consumption
ggplot(data=result_2011, aes(x=TOT_consumption, group=factor(urban), fill=factor(urban))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  xlim(c(0, 800000))



# Problem t-test - FIGURE A.5
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_food_2011.R") 
ggplot(data=b1_food11, aes(x=purchases, group=factor(urban),  fill=factor(urban))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  xlim(c(0, 800000))







########## ########## ########## ########## ########## ########## ########## ########## ########## ########## 
##########    Engel curves - FIGURE A.6
##########
##########
##########


##########
##########   2009
# food
ggplot(data=result_2009, aes(x=TOT_consumption, y = food_percentage)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_ipsum() +
  xlim(c(0, 800000))

# non-durable
ggplot(data=result_2009, aes(x=TOT_consumption, y = non_durable_percentage)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_ipsum() +
  xlim(c(0, 800000))

# durable
ggplot(data=result_2009, aes(x=TOT_consumption, y = durable_percentage)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_ipsum() +
  xlim(c(0, 800000))




##########
##########   2010
# food

ggplot(data=result_2010, aes(x=TOT_consumption, y = food_percentage, weight = result_2010$wgt10)) + # TOT_consumption, try also other measures. 
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  theme_ipsum() +
  xlim(c(0, 800000))

# non-durable
ggplot(data=result_2010, aes(x=TOT_consumption, y = non_durable_percentage)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  theme_ipsum() +
  xlim(c(0, 800000))

# durable
ggplot(data=result_2010, aes(x=TOT_consumption, y = durable_percentage)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  theme_ipsum() +
  xlim(c(0, 800000))


##########
##########   2011
# food
ggplot(data=result_2011, aes(x=TOT_consumption, y = food_percentage)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  theme_ipsum() +
  xlim(c(0, 800000))

# non-durable
ggplot(data=result_2011, aes(x=TOT_consumption, y = non_durable_percentage)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  theme_ipsum() +
  xlim(c(0, 800000))

# durable
ggplot(data=result_2011, aes(x=TOT_consumption, y = durable_percentage)) +
  geom_point() +
  geom_smooth(method = "loess", se = T) +
  theme_ipsum() +
  xlim(c(0, 800000))
















