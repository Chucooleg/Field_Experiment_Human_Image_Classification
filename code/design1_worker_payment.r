# This code helps to determine whether to pay a worker or not, based on his or her task performance

#------------------------------------------------------------------------------------------------#
# ORDER 2 $0.40 

rm(list = ls())

# this is the qualtric csv file you download from qualtric platform, containing survey results
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")


qualtric_data_path = "../qualtric_data/20171119_qualtric_results_order2_0.10.csv" #!!! UPDATE
MTurk_data_path = "../MTurk_data/20171119_mturk_results_order2_0.10.csv" #!!! UPDATE

# load supporting functions
source(file = "design1_data_transformation_functions.r")
source(file = "design1_data_analysis_functions.r")
existing_path = "../MTurk_ID_status/worker_status.csv"

current_task_data = get_current_task_data(csv_path = qualtric_data_path)
MTurk_worker_id = get_MTurk_worker_id(csv_path = MTurk_data_path)

# adjust the payment threshold if 0.25 task accuracy is too low
worderIDs_task_status = construct_frame_worderIDs_task_status(current_task_data = current_task_data, 
                                                              submitted_MTurk_ids = MTurk_worker_id,
                                                              allQ = allQ, 
                                                              payment_accuracy_threshold = 0.75, 
                                                              task_name = "order2", #!!! UPDATE
                                                              treatment_payrate = 0.10, #!!! UPDATE
                                                              existing_path = existing_path) 


# Who pass this HIT enough to be eligible for payment
eligbile_id = worderIDs_task_status[pay_or_not == 1, c("worker_id","worker_id_found_on_MTurk", "repeater")]


eligbile_id
