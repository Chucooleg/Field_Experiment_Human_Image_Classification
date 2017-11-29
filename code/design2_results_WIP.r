# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")
source(file = "design1_data_analysis_functions.r")

#---------------------------------------------------------------------#

# read in qualtric output csv
qualtric_data_path = "../qualtric_data/20171127_qualtric_results_design2_main.csv" #!!! UPDATE
MTurk_data_path = "../MTurk_data/20171127_mturk_results_design2_main.csv" #!!! UPDATE

# load supporting functions
source(file = "design2_data_transformation_functions.r")
existing_path = "../MTurk_ID_status/worker_status.csv"

current_task_data = get_current_task_data(csv_path = qualtric_data_path)
MTurk_worker_id = get_MTurk_worker_id(csv_path = MTurk_data_path)

#---------------------------------------------------------------------#
by_HIT.table = evaluate_worker_perf.design2(current_task_data, allQ.design2)

View(by_HIT.table)

stacked = stack(by_HIT.table)

by_Session1.table = data.table(worker_id = stacked[stacked$ind == "worker_id", "values"],
                              group = stacked[stacked$ind == "group", "values"],
                              round = "one", # update
                              round_bonus = as.numeric(substr(stacked[stacked$ind == "group", "values"], start = 1, stop = 1) == "T"), ## update
                              round_timespent = stacked[stacked$ind == "time_spent_S1", "values"], #update
                              round_accuracy = stacked[stacked$ind == "accuracy_S1", "values"] #update
                              CQ1 = stacked[stacked$ind == "CQ1", "values"],
                              CQ2 = stacked[stacked$ind == "CQ1", "values"],
                              CQ3 = stacked[stacked$ind == "CQ1", "values"],
                              CQ4 = stacked[stacked$ind == "CQ1", "values"],
                              CQ5 = stacked[stacked$ind == "CQ1", "values"]
                              )

## DEAL with CQ data types

#---------------------------------------------------------------------#
# check attrition
#---------------------------------------------------------------------#
# check covariate balance