# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
# setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design2_data_transformation_functions.r")
#source(file = "design1_data_analysis_functions.r")

#---------------------------------------------------------------------#

# read in qualtric output csv
qualtric_data_path = "../qualtric_data/20171123_qualtric_results_design2_pilot.csv" #!!! UPDATE
MTurk_data_path = "../MTurk_data/20171123_mturk_results_design2_pilot.csv" #!!! UPDATE

# load supporting functions
source(file = "design2_data_transformation_functions.r")
existing_path = "../MTurk_ID_status/worker_status.csv"

current_task_data = get_current_task_data(csv_path = qualtric_data_path)
MTurk_worker_id = get_MTurk_worker_id(csv_path = MTurk_data_path)

#---------------------------------------------------------------------#
worker_perf.design2 = evaluate_worker_perf.design2(current_task_data, allQ.design2)

View(worker_perf.design2)

stacked = stack(worker_perf.design2)

by_Session1.table = data.table(worker_id = stacked[stacked$ind == "worker_id", "values"],
                              group = stacked[stacked$ind == "group", "values"],
                              round = "one", # update
                              round_bonus = as.numeric(substr(stacked[stacked$ind == "group", "values"], start = 1, stop = 1) == "T"), ## update
                              round_timespent = stacked[stacked$ind == "time_spent_S1", "values"], #update
                              round_accuracy = stacked[stacked$ind == "accuracy_S1", "values"], #update
                              round_screener = stacked[stacked$ind == "screener_S1", "values"], #update                              
                              CQ1 = factor(stacked[stacked$ind == "CQ1", "values"], levels = c("a lot less than half", "around half", "a lot more than half")),
                              CQ2 = stacked[stacked$ind == "CQ2_3", "values"],
                              CQ3 = factor(stacked[stacked$ind == "CQ3", "values"], levels = c("No", "Maybe", "Yes")),
                              CQ4 = factor(stacked[stacked$ind == "CQ4", "values"], levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more")),
                              CQ5 = factor(stacked[stacked$ind == "CQ5", "values"], levels = c("Never heard of Linkedin", "No", "Yes"))
                              )

by_Session2.table = data.table(worker_id = stacked[stacked$ind == "worker_id", "values"],
                               group = stacked[stacked$ind == "group", "values"],
                               round = "two", # update
                               round_bonus = as.numeric(substr(stacked[stacked$ind == "group", "values"], start = 2, stop = 2) == "T"), ## update
                               round_timespent = stacked[stacked$ind == "time_spent_S2", "values"], #update
                               round_accuracy = stacked[stacked$ind == "accuracy_S2", "values"], #update
                               round_screener = stacked[stacked$ind == "screener_S2", "values"], #update                              
                               CQ1 = factor(stacked[stacked$ind == "CQ1", "values"], levels = c("a lot less than half", "around half", "a lot more than half")),
                               CQ2 = stacked[stacked$ind == "CQ2_3", "values"],
                               CQ3 = factor(stacked[stacked$ind == "CQ3", "values"], levels = c("No", "Maybe", "Yes")),
                               CQ4 = factor(stacked[stacked$ind == "CQ4", "values"], levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more")),
                               CQ5 = factor(stacked[stacked$ind == "CQ5", "values"], levels = c("Never heard of Linkedin", "No", "Yes"))
)

by_Session3.table = data.table(worker_id = stacked[stacked$ind == "worker_id", "values"],
                               group = stacked[stacked$ind == "group", "values"],
                               round = "three", # update
                               round_bonus = as.numeric(substr(stacked[stacked$ind == "group", "values"], start = 3, stop = 3) == "T"), ## update
                               round_timespent = stacked[stacked$ind == "time_spent_S3", "values"], #update
                               round_accuracy = stacked[stacked$ind == "accuracy_S3", "values"], #update
                               round_screener = stacked[stacked$ind == "screener_S3", "values"], #update                              
                               CQ1 = factor(stacked[stacked$ind == "CQ1", "values"], levels = c("a lot less than half", "around half", "a lot more than half")),
                               CQ2 = as.numeric(stacked[stacked$ind == "CQ2_3", "values"]),
                               CQ3 = factor(stacked[stacked$ind == "CQ3", "values"], levels = c("No", "Maybe", "Yes")),
                               CQ4 = factor(stacked[stacked$ind == "CQ4", "values"], levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more")),
                               CQ5 = factor(stacked[stacked$ind == "CQ5", "values"], levels = c("Never heard of Linkedin", "No", "Yes"))
)

by_Session.table = rbind(by_Session1.table, by_Session2.table, by_Session3.table)



by_HIT_observed.table = data.table(worker_id = worker_perf.design2$worker_id,
                          group = worker_perf.design2$group,
                          total_bonus = 0.10 * str_count(worker_perf.design2$group, "T"),
                          total_timespent = worker_perf.design2$time_spent,
                          overall_accuracy = worker_perf.design2$accuracy,
                          all_screeners_passed = worker_perf.design2$screener,
                          CQ1 = factor(worker_perf.design2$CQ1, levels = c("a lot less than half", "around half", "a lot more than half")),
                          CQ2 = as.numeric(worker_perf.design2$CQ2_3),
                          CQ3 = factor(worker_perf.design2$CQ3, levels = c("No", "Maybe", "Yes")),
                          CQ4 = factor(worker_perf.design2$CQ4, levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more")),
                          CQ5 = factor(worker_perf.design2$CQ5, levels = c("Never heard of Linkedin", "No", "Yes"))
)


attriters_perf.design2 = convert_raw_attriters.design2(current_task_data)

by_HIT_attrited.table = data.table(worker_id = attriters_perf.design2$worker_id,
                                   group = attriters_perf.design2$group,
                                   CQ1 = factor(attriters_perf.design2$CQ1, levels = c("a lot less than half", "around half", "a lot more than half")),
                                   CQ2 = as.numeric(attriters_perf.design2$CQ2_3),
                                   CQ3 = factor(attriters_perf.design2$CQ3, levels = c("No", "Maybe", "Yes")),
                                   CQ4 = factor(attriters_perf.design2$CQ4, levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more")),
                                   CQ5 = factor(attriters_perf.design2$CQ5, levels = c("Never heard of Linkedin", "No", "Yes")))

#by_HIT.table = rbind(by_HIT_observed.table, by_HIT_attrited.table, fill=TRUE)

#by_HIT.table$observed = as.numeric(!is.na(by_HIT.table$total_bonus))

by_HIT.table = by_HIT_observed.table
View(by_HIT.table)
#---------------------------------------------------------------------#
# write to csv

write.csv(by_Session.table, "../modeling_data/design2_by_Session_pilot.csv")
write.csv(by_HIT.table, "../modeling_data/design2_by_HIT_pilot.csv")

#---------------------------------------------------------------------#
