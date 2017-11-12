# USE THIS CODE TO TAKE OUT REPEATERS -- PROBLEM IF SPILL OVERS

rm(list = ls())
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")

# load supporting functions
source(file = "order1_data_transformation_functions.r")

# EARLIER EXPERIMENT
qualtric_data_path_0.10 = "../qualtric_data/20171111_qualtric_results_order1_0.10.csv"
# LATER EXPERIMENT
qualtric_data_path_0.55 = "../qualtric_data/20171111_qualtric_results_order1_0.55.csv"

data_0.10 = get_current_task_data(qualtric_data_path_0.10)
data_0.55 = get_current_task_data(qualtric_data_path_0.55)

# REPEATERS
filter = data_0.55$worker_id %in% data_0.10$worker_id
# NUMBER OF REPEATERS
sum(filter)
# WHO THEY ARE
repeater_IDs = data_0.55[filter,]$worker_id


#----------------------------------------------------------------------------#
existing_path = "../MTurk_ID_status/worker_status.csv"

existing_status = fread(existing_path)[,-c("V1")]

repeater_status = existing_status[worker_id %in% repeater_IDs,][order(worker_id),]
repeater_status
View(repeater_status)


