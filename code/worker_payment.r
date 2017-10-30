# This code helps to determine whether to pay a worker or not, based on his or her task performance

rm(list = ls())

# this is the qualtric csv file you download from qualtric platform, containing survey results
qualtric_data_path = "../qualtric_data/20171028_qualtric_results_pilot_0.25.csv" #!!! UPDATE
MTurk_data_path = "../MTurk_data/20171028_mturk_results_pilot_0.25.csv" #!!! UPDATE

# load supporting functions
source(file = "data_transformation_functions.r")


current_task_data = get_current_task_data(csv_path = qualtric_data_path)
MTurk_worker_id = get_MTurk_worker_id(csv_path = MTurk_data_path)

# adjust the payment threshold if 0.25 task accuracy is too low
worderIDs_task_status = construct_frame_worderIDs_task_status(current_task_data = current_task_data, 
                                                              submitted_MTurk_ids = MTurk_worker_id,
                                                              allQ = allQ, 
                                                              payment_accuracy_threshold = 0.25, 
                                                              task_name = "pilot", #!!! UPDATE
                                                              treatment_payrate = 0.25) #!!! UPDATE


# Who to pay
worderIDs_task_status[pay_or_not == 1, c("worker_id","worker_id_found_on_MTurk")]
