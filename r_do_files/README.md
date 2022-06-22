

| Household Roster  | Main Results |
| ------------- | ------------- |
| <a href="do_hh2009.R">do_hh2009.R</a>   | <a href="do_items_food_2009.R">do_items_food_2009.R</a>  |
| <a href="do_hh2010.R">do_hh2010.R</a>   | <a href="do_items_food_2010.R">do_items_food_2010.R</a>   |
| <a href="do_hh2011.R">do_hh2011.R</a>   | <a href="do_items_food_2011.R">do_items_food_2011.R</a>   |
| <a href="do_ind_characteristics2009.R">do_ind_characteristics2009.R</a>   | <a href="do_items_non_durables_2009.R">do_items_non_durables_2009.R</a>  |
| <a href="do_ind_characteristics2010.R">do_ind_characteristics2010.R</a>   | <a href="do_items_non_durables_2010.R">do_items_non_durables_2010.R</a>  |
| <a href="do_ind_characteristics2011.R">do_ind_characteristics2011.R</a>   | <a href="do_items_non_durables_2011.R">do_items_non_durables_2011.R</a>  |
| <a href="do_ind_educ2009.R">do_ind_educ2009.R</a>   | <a href="do_items_durables_2009.R">do_items_durables_2009.R</a>  |
| <a href="do_ind_educ2010.R">do_ind_educ2010.R</a>   | <a href="do_items_durables_2010.R">do_items_durables_2010.R</a>  |
| <a href="do_ind_educ2011.R">do_ind_educ2011.R</a>   | <a href="do_items_durables_2011.R">do_items_durables_2011.R</a>  | 
| | &#8594; <a href="Results_2009.R">Results_2009.R</a>  | 
| | &#8594; <a href="Results_2009_table.R">Results_2009_table.R</a>  | 
| | &#8594; <a href="Results_2010.R">Results_2010.R</a>  | 
| | &#8594; <a href="Results_2010_table.R">Results_2010_table.R</a>  | 
| | &#8594; <a href="Results_2011.R">Results_2011.R</a>  | 
| | &#8594; <a href="Results_2011_table.R">Results_2011_table.R</a>  | 




# Sample Selection
This files executes the matching procedure that has led to a sample of study of 2157 households: 

* <a href="do_sample_selection.R">do_sample_selection.R</a>


&#8594; &#8594; &#8594; &#8594; &#8594; Output of this file is the following .csv file: <a href="https://github.com/andreasalem/LSMS-ISA-Uganda-Study/blob/main/data_clean/final_vector_sample.csv"><font color="01DF01">final_vector_sample.csv</font></a>



# Households rosters
This file merges various .csv files in order to create three roster datasets with household as well as individual level information:

* <a href="do_roster_datasets.R">do_roster_datasets.R</a>


&#8594; &#8594; &#8594; &#8594; &#8594; Output of this file is the following .csv file: 

* <a href="https://github.com/andreasalem/LSMS-ISA-Uganda-Study/blob/main/data_clean/matched_hh_2009.csv"><font color="01DF01">matched_hh_2009.csv</font></a>
* <a href="https://github.com/andreasalem/LSMS-ISA-Uganda-Study/blob/main/data_clean/matched_hh_2010.csv">matched_hh_2010.csv</a>
* <a href="https://github.com/andreasalem/LSMS-ISA-Uganda-Study/blob/main/data_clean/matched_hh_2011.csv">matched_hh_2011.csv</a>


This file makes use of .csv files which are generated using the followinig .r files: 


* Household-level info.:
  * <a href="do_hh2009.R">do_hh2009.R</a> &#8594; &#8594; <a href="hh_2009.csv">hh_2009.csv</a>
  * <a href="do_hh2010.R">do_hh2009.R</a> &#8594; &#8594; <a href="hh_2010.csv">hh_2009.csv</a>
  * <a href="do_hh2011.R">do_hh2009.R</a> &#8594; &#8594; <a href="hh_2011.csv">hh_2009.csv</a>

* Individual-level demographic info. 
  * <a href="do_ind_characteristics2009.R">do_ind_characteristics2009.R</a> &#8594; &#8594; <a href="hh_charact2009.csv">hh_charact2009.csv</a>
  * <a href="do_ind_characteristics2010.R">do_ind_characteristics2010.R</a> &#8594; &#8594; <a href="hh_charact2010.csv">hh_charact2010.csv</a>
  * <a href="do_ind_characteristics2011.R">do_ind_characteristics2011.R</a> &#8594; &#8594; <a href="hh_charact2011.csv">hh_charact2011.csv</a>
 
* Individual-level education info. 
  * * <a href="do_ind_educ2009.R">do_ind_educ2009.R</a> &#8594; &#8594; <a href="hh_educ2009.csv">hh_educ2009.csv</a>
  * * <a href="do_ind_educ2010.R">do_ind_educ2010.R</a> &#8594; &#8594;<a href="hh_educ2010.csv">hh_educ2010.csv</a>
  * * <a href="do_ind_educ2011.R">do_ind_educ2011.R</a> &#8594; &#8594; <a href="hh_educ2011.csv">hh_educ2011.csv</a>





# Visual Representations
All figures (Figure 3.1, 3.2, A.1-A.6) and Table 4.1 are generated using the following r files:

* <a href="Table 4.1 - Welch t-test of Difference in Means.R">Table 4.1 - Welch t-test of Difference in Means.R</a>
* <a href="Figures.R">Figures.R</a>
* <a href="Appendices.R">Appendices.R</a>


# Results

Final results are contained in the following files:

* <a href="result_2009.csv">result_2009.csv</a>
* <a href="result_2010.csv">result_2010.csv</a>
* <a href="result_2011.csv">result_2011.csv</a>
