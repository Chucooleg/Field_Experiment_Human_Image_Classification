# Use this code to append updated worker status info to existing worker status sheet

rm(list = ls())
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")

# load supporting functions
source(file = "design1_data_transformation_functions.r")

# existing file, be careful when you overwrite
existing_path = "../MTurk_ID_status/worker_status.csv"

# only run in the very first setup, don't touch afterwards
# DONT_USE_construct_frame_workerIDs_task_empty(custom_path = existing_path)


# this is the qualtric csv file you download from qualtric platform, containing survey results
qualtric_data_path = "../qualtric_data/20171112_qualtric_results_order1_0.25.csv" #!!!UPDATE
MTurk_data_path = "../MTurk_data/20171112_mturk_results_order1_0.25.csv" #!!!UPDATE

# construct contents for file update
current_task_data = get_current_task_data(csv_path = qualtric_data_path)
MTurk_worker_id = get_MTurk_worker_id(csv_path = MTurk_data_path)
# adjust the payment threshold if 0.25 task accuracy is too low
worderIDs_task_status = construct_frame_worderIDs_task_status(current_task_data = current_task_data, 
                                                              submitted_MTurk_ids = MTurk_worker_id,
                                                              allQ = allQ, 
                                                              payment_accuracy_threshold = 0.25, 
                                                              task_name = "order1", #!!!UPDATE
                                                              treatment_payrate = 0.25,
                                                              existing_path = existing_path) #!!! UPADTE

# CAUTION!
# EXIT EXCEL FILE FIRST
# append content
existing_status = fread(existing_path)[,-c("V1")]
all_status = rbind(existing_status, worderIDs_task_status, fill=TRUE)
write.csv(x = all_status, file = existing_path)


# ALREADY APPENDED 
# PILOT TREATMENT 0.10 20171028
# PILOT TREATMENT 0.25 20171029 