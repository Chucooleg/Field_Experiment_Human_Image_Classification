# Use this code to append updated worker status info to existing worker status sheet

rm(list = ls())

# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")
source(file = "design1_data_analysis_functions.r")

#---------------------------------------------------------------------#

# read in qualtric output csv
qualtric_data_path = "../qualtric_data/20171123_qualtric_results_design2_pilot.csv" #!!! UPDATE
MTurk_data_path = "../MTurk_data/20171123_mturk_results_design2_pilot.csv" #!!! UPDATE

# load supporting functions
source(file = "design2_data_transformation_functions.r")
#source(file = "design1_data_analysis_functions.r")
existing_path = "../MTurk_ID_status/worker_status.csv"


current_task_data = get_current_task_data(csv_path = qualtric_data_path)
MTurk_worker_id = get_MTurk_worker_id(csv_path = MTurk_data_path)


# adjust the payment threshold if 0.25 task accuracy is too low
worderIDs_task_status = construct_frame_worderIDs_task_status.design2(current_task_data = current_task_data, 
                                                                      submitted_MTurk_ids = MTurk_worker_id,
                                                                      allQ = allQ.design2, 
                                                                      payment_accuracy_threshold = 0.75, 
                                                                      task_name = "design2 pilot", #!!! UPDATE
                                                                      treatment_payrate = 0.10, #!!! UPDATE
                                                                      bonus_rate = 0.05,
                                                                      existing_path = existing_path) 

# CAUTION!
# EXIT EXCEL FILE FIRST
# append content
existing_status = fread(existing_path)[,-c("V1")]
all_status = rbind(existing_status, worderIDs_task_status, fill=TRUE)
write.csv(x = all_status, file = existing_path)


