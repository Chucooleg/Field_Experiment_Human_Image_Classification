# This code helps to determine whether to pay a worker or not, based on his or her task performance

# this is the qualtric csv file you download from qualtric platform, containing survey results
qualtric_data_path = "../qualtric_data/20171028_qualtric_results_pilot_0.25.csv"
MTurk_data_path = "../MTurk_data/20171028_mturk_results_pilot_0.25.csv"

# load supporting functions
source(file = "data_transformation_functions.r")

# construct_frame_worderIDs_task_status(current_task_data, allQ, payment_accuracy_threshold, task_name)
# adjust the payment threshold if 0.25 task accuracy is too low
current_task_data = get_current_task_data(csv_path = qualtric_data_path)
get_MTurk_worker_id(csv_path = MTurk_data_path)

worderIDs_task_status = construct_frame_worderIDs_task_status(current_task_data = current_task_data, 
                                                              allQ = allQ, payment_accuracy_threshold = 0.25, 
                                                              task_name = "pilot 0.25")

# Who to pay
worderIDs_task_status[pay_or_not == 1, "worker_id"]
