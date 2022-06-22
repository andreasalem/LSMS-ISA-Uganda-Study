


# 1) Create separate dataset with individual level and household level covariates

source("/Users/andreasalem/R_oba/Thesis/do_files/do_hh2009.R")  # ----> hh_2009.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_hh2010.R")  # ----> hh_2010.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_hh2011.R")  # ----> hh_2011.csv

source("/Users/andreasalem/R_oba/Thesis/do_files/do_ind_characteristics2009.R")  # ----> hh_charact2009.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_ind_characteristics2010.R")  # ----> hh_charact2010.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_ind_characteristics2011.R")  # ----> hh_charact2011.csv

source("/Users/andreasalem/R_oba/Thesis/do_files/do_ind_educ2009.R")  # ----> hh_educ2009.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_ind_educ2010.R")  # ----> hh_educ2010.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_ind_educ2011.R")  # ----> hh_educ2011.csv


# 2) Sample selection

source("/Users/andreasalem/R_oba/Thesis/do_files/do_sample_selection.R")  # ----> final_vector_sample.csv


# 3) Create roster datasets by merging previous .csv files

source("/Users/andreasalem/R_oba/Thesis/do_files/do_roster_datasets.R")  # returns 3 files: ----> matched_hh_2009.csv
                                                                         #                  ----> matched_hh_2010.csv
                                                                         #                  ----> matched_hh_2011.csv


# 4) Create separate dataset with household aggregate consumption spending on 3 categories: food, non-durables, durables
#
#
# IMPORTANT: since 2011 prices are used for each category, the 2011 do_files need to be run first in order to 
#            create the price lists to apply to other years
#
#
#

#### 4.1) FOOD
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_food_2011.R")  # ----> items_2011_food.csv + price_list11_food.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_food_2009.R")  # ----> items_2009_food.csv 
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_food_2010.R")  # ----> items_2010_food.csv 

##       4.1.1) t-tests FOOD
         source("/Users/andreasalem/R_oba/Thesis/do_files/Table 4.1 - Welch t-test of Difference in Means.R") # ----> TABLE 4.1 (see Thesis)

#### 4.2) NON-DURABLES
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_non_durables_2011.R")  # ----> items_2011_non-durab.csv + price_list11_non_durable.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_non_durables_2009.R")  # ----> items_2009_non-durab.csv 
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_non_durables_2010.R")  # ----> items_2010_non-durab.csv 



#### 4.3) DURABLES
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_durables_2011.R")      # ----> items_2011_durables.csv + price_list11_durable.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_durables_2009.R")      # ----> items_2009_durables.csv 
source("/Users/andreasalem/R_oba/Thesis/do_files/do_items_durables_2010.R")      # ----> items_2010_durables.csv 



# 5) Merge each item category (food, non-durable, durable) to compute household-level total weekly expenditure (per adult equivalent)

source("/Users/andreasalem/R_oba/Thesis/do_files/Results_2009.R")    # ----> result_2009.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/Results_2010.R")    # ----> result_2010.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/Results_2011.R")    # ----> result_2011.csv

# 5.1) Show results by quintiles and geographical setting
source("/Users/andreasalem/R_oba/Thesis/do_files/Results_2009_table.R")    # ----> result_2009_table.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/Results_2010_table.R")    # ----> result_2010_table.csv
source("/Users/andreasalem/R_oba/Thesis/do_files/Results_2011_table.R")    # ----> result_2011_table.csv


# 6) Figures (for example descriptive statistics of the roster datasets) (Important: Tables 4.1, 4.2, and 4.3 are created in step 5 and 5.1 above)


# 6.1) FIGURES main text + one TABLE + one figure Appendix:

source("/Users/andreasalem/R_oba/Thesis/do_files/Figures.R") # creates : ----> FIGURE 3.1, 3.2, A.1 and TABLE 3.1 (see Thesis)


# 6.12 FIGURES Appendix

source("/Users/andreasalem/R_oba/Thesis/do_files/Appendices.R") # creates : ----> FIGURE A.2, A.3, A.4, A.5, A.6


source("/Users/andreasalem/R_oba/Thesis/do_files/Extra_panel_analysis_changes.R") # shows total consumption changes




# 30 .r files in total 

