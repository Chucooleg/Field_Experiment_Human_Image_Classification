rm(list = ls())

# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")
source(file = "design1_data_analysis_functions.r")

#---------------------------------------------------------------------#

# read in qualtric output csv
dummy_design2_qualtric_data_path = "../qualtric_data/20171120_qualtric_results_sample_design2.csv"
dummy_design2_task_data = get_current_task_data(dummy_design2_qualtric_data_path)

dummy_design2_task_data[`Timer_CCC_S1_Page Submit`>0,]


