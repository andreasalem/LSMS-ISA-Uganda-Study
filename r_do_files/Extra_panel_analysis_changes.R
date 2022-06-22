######################### 
#
#
#
######################### 
######################### Table of Contents

# this file produces two graphs, one in absolute terms and one wheree y-axis sums up to 100%.



######################### Data required (i.e., roster datatsets)

# result_2009_table.csv
# result_2010_table.csv
# result_2011_table.csv


######################### Code
getwd()
setwd("/Users/andreasalem/R_oba/Thesis")
head(list.files())

library(tidyverse)
library(haven) #used to open .dta files
library(dplyr)
library(Hmisc) #used for weighted mean and standard deviation

options(scipen=999) # avoids scientific notation





############################################################################

result_2009_table <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2009_table.csv")
result_2010_table <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2010_table.csv")
result_2011_table <- read.csv("/Users/andreasalem/R_oba/Thesis/Data_clean/result_2011_table.csv")

############################################################################ plot for each year, absolute values of consumption categories


# select Full Sample, and firstt 4 columns
graph_absolute <- merge(merge(as.data.frame(gather(result_2009_table[1,])[2:6,]), as.data.frame(gather(result_2010_table[1,])[2:6,]), by ="key"), as.data.frame(gather(result_2011_table[1,])[2:6,]), by = "key") %>% 
  rename("exp_2009" = value.x,
         "exp_2010" = value.y,
         "exp_2011" = value) 

graph_absolute$exp_2009 <- as.numeric(graph_absolute$exp_2009)
graph_absolute$exp_2010 <- as.numeric(graph_absolute$exp_2010)
graph_absolute$exp_2011 <- as.numeric(graph_absolute$exp_2011)


# 2009
g1 <- ggplot(graph_absolute,                                     
             aes(x = key,
                 y = exp_2009,
                 fill = key)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# 2010
g2 <- ggplot(graph_absolute,                                     
             aes(x = key,
                 y = exp_2010,
                 fill = key)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

# 2011
g3 <- ggplot(graph_absolute,                                     
             aes(x = key,
                 y = exp_2011,
                 fill = key)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

########################################################################

gathered_graph_absolute <- gather(graph_absolute[1:3,], year, value, exp_2009:exp_2011, factor_key=TRUE)


# Version 1, absolute
ggplot(gathered_graph_absolute, aes(fill = key,y = value, x = year)) +
  geom_bar(position = "stack",  # replace with position = "fill"
           stat = "identity") +
  theme(legend.position = "bottom") 

# Version 2, relative
ggplot(gathered_graph_absolute, aes(fill = key,y = value, x = year)) +
  geom_bar(position = "fill",  # replace with position = "fill"
           stat = "identity") +
  theme(legend.position = "bottom")




rm(g1, g2, g3, gathered_graph_absolute, graph_absolute)
