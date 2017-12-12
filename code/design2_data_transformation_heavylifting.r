# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design2_data_transformation_functions.r")
#source(file = "design1_data_analysis_functions.r")

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

by_HIT.table = rbind(by_HIT_observed.table, by_HIT_attrited.table, fill=TRUE)

by_HIT.table$observed = as.numeric(!is.na(by_HIT.table$total_bonus))

View(by_HIT.table)
#---------------------------------------------------------------------#
# write to csv

write.csv(by_Session.table, "../modeling_data/design2_by_Session.csv")
write.csv(by_HIT.table, "../modeling_data/design2_by_HIT.csv")

#---------------------------------------------------------------------#
# Table of assigned treatment condition, observed outcomes and potential outcomes

to_pivot = data.table(worker_id = by_Session.table$worker_id,
                 round = by_Session.table$round,
                 value = by_Session.table$round_accuracy)

observed_outcomes_table = data.table(cast(to_pivot, worker_id~round))

assigned_treatment_condition_table = observed_outcomes_table

CCC_list = by_Session.table[group == "CCC", worker_id]
CCT_list = by_Session.table[group == "CCT", worker_id]
CTT_list = by_Session.table[group == "CTT", worker_id]
TTT_list = by_Session.table[group == "TTT", worker_id]

assigned_treatment_condition_table[worker_id %in% CCC_list,one_assignment:= "00"]
assigned_treatment_condition_table[worker_id %in% CCC_list,two_assignment:= "00"]
assigned_treatment_condition_table[worker_id %in% CCC_list,three_assignment:= "00"]

assigned_treatment_condition_table[worker_id %in% CCT_list,one_assignment:= "00"]
assigned_treatment_condition_table[worker_id %in% CCT_list,two_assignment:= "00"]
assigned_treatment_condition_table[worker_id %in% CCT_list,three_assignment:= "01"]

assigned_treatment_condition_table[worker_id %in% CTT_list,one_assignment:= "00"]
assigned_treatment_condition_table[worker_id %in% CTT_list,two_assignment:= "01"]
assigned_treatment_condition_table[worker_id %in% CTT_list,three_assignment:= "11"]

assigned_treatment_condition_table[worker_id %in% TTT_list,one_assignment:= "01"]
assigned_treatment_condition_table[worker_id %in% TTT_list,two_assignment:= "11"]
assigned_treatment_condition_table[worker_id %in% TTT_list,three_assignment:= "11"]

assigned_treatment_condition_table = data.table(worker_id = assigned_treatment_condition_table$worker_id,
                                                one = assigned_treatment_condition_table$one_assignment,
                                                two = assigned_treatment_condition_table$two_assignment,
                                                three = assigned_treatment_condition_table$three_assignment)
#---------------------------------------------------------------------#
# write to csv

write.csv(observed_outcomes_table, "../modeling_data/design2_observed_outcomes_table.csv")
write.csv(assigned_treatment_condition_table, "../modeling_data/design2_assigned_treatment_condition_tablee.csv")

#---------------------------------------------------------------------#
# design 2 full potential outcomes schedule

Y01.table = data.table(worker_id = observed_outcomes_table$worker_id,
                       one = NA,
                       two = NA,
                       three = NA)

Y00.table = data.table(worker_id = observed_outcomes_table$worker_id,
                       one = NA,
                       two = NA,
                       three = NA)

Y11.table = data.table(worker_id = observed_outcomes_table$worker_id,
                       one = NA,
                       two = NA,
                       three = NA)

# Y00 one 
Y00.table$one = as.numeric(assigned_treatment_condition_table$one == "00") * observed_outcomes_table$one
# Y00 two 
Y00.table$two = as.numeric(assigned_treatment_condition_table$two == "00") * observed_outcomes_table$two
# Y00 three 
Y00.table$three = as.numeric(assigned_treatment_condition_table$three == "00") * observed_outcomes_table$three


# Y01 one 
Y01.table$one = as.numeric(assigned_treatment_condition_table$one == "01") * observed_outcomes_table$one
# Y01 two
Y01.table$two = as.numeric(assigned_treatment_condition_table$two == "01") * observed_outcomes_table$two
# Y01 three
Y01.table$three = as.numeric(assigned_treatment_condition_table$three == "01") * observed_outcomes_table$three


# Y11 one 
Y11.table$one = as.numeric(assigned_treatment_condition_table$one == "11") * observed_outcomes_table$one
# Y11 two
Y11.table$two = as.numeric(assigned_treatment_condition_table$two == "11") * observed_outcomes_table$two
# Y11 three
Y11.table$three = as.numeric(assigned_treatment_condition_table$three == "11") * observed_outcomes_table$three


Y00.table$one[Y00.table$one == 0] = NA
Y00.table$two[Y00.table$two == 0] = NA
Y00.table$three[Y00.table$three == 0] = NA

Y01.table$one[Y01.table$one == 0] = NA
Y01.table$two[Y01.table$two == 0] = NA
Y01.table$three[Y01.table$three == 0] = NA

Y11.table$one[Y11.table$one == 0] = NA
Y11.table$two[Y11.table$two == 0] = NA
Y11.table$three[Y11.table$three == 0] = NA


Y00.table.dum = Y00.table
Y01.table.dum = Y01.table
Y11.table.dum = Y11.table

Y00.table.dum$one.01 = Y00.table.dum$one + E_Y01_Y00
Y00.table.dum$one.11 = Y00.table.dum$one + E_Y11_Y00
Y00.table.dum$two.01 = Y00.table.dum$two + E_Y01_Y00
Y00.table.dum$two.11 = Y00.table.dum$two + E_Y11_Y00
Y00.table.dum$three.01 = Y00.table.dum$three + E_Y01_Y00
Y00.table.dum$three.11 = Y00.table.dum$three + E_Y11_Y00

Y01.table.dum$one.00 = Y01.table.dum$one - E_Y01_Y00
Y01.table.dum$one.11 = Y01.table.dum$one - E_Y01_Y00 + E_Y11_Y00
Y01.table.dum$two.00 = Y01.table.dum$two - E_Y01_Y00
Y01.table.dum$two.11 = Y01.table.dum$two - E_Y01_Y00 + E_Y11_Y00
Y01.table.dum$three.00 = Y01.table.dum$three - E_Y01_Y00
Y01.table.dum$three.11 = Y01.table.dum$three - E_Y01_Y00 + E_Y11_Y00

Y11.table.dum$one.00 = Y11.table.dum$one - E_Y11_Y00
Y11.table.dum$one.01 = Y11.table.dum$one - E_Y11_Y00 + E_Y01_Y00
Y11.table.dum$two.00 = Y11.table.dum$two - E_Y11_Y00
Y11.table.dum$two.01 = Y11.table.dum$two - E_Y11_Y00 + E_Y01_Y00
Y11.table.dum$three.00 = Y11.table.dum$three - E_Y11_Y00
Y11.table.dum$three.01 = Y11.table.dum$three - E_Y11_Y00 + E_Y01_Y00

Y00.table.dum$one[is.na(Y00.table.dum$one)] = 0
Y00.table.dum$two[is.na(Y00.table.dum$two)] = 0
Y00.table.dum$three[is.na(Y00.table.dum$three)] = 0
Y00.table.dum$one.01[is.na(Y00.table.dum$one.01)] = 0
Y00.table.dum$one.11[is.na(Y00.table.dum$one.11)] = 0
Y00.table.dum$two.01[is.na(Y00.table.dum$two.01)] = 0
Y00.table.dum$two.11[is.na(Y00.table.dum$two.11)] = 0
Y00.table.dum$three.01[is.na(Y00.table.dum$three.01)] = 0
Y00.table.dum$three.11[is.na(Y00.table.dum$three.11)] = 0

Y01.table.dum$one[is.na(Y01.table.dum$one)] = 0
Y01.table.dum$two[is.na(Y01.table.dum$two)] = 0
Y01.table.dum$three[is.na(Y01.table.dum$three)] = 0
Y01.table.dum$one.00[is.na(Y01.table.dum$one.00)] = 0
Y01.table.dum$one.11[is.na(Y01.table.dum$one.11)] = 0
Y01.table.dum$two.00[is.na(Y01.table.dum$two.00)] = 0
Y01.table.dum$two.11[is.na(Y01.table.dum$two.11)] = 0
Y01.table.dum$three.00[is.na(Y01.table.dum$three.00)] = 0
Y01.table.dum$three.11[is.na(Y01.table.dum$three.11)] = 0

Y11.table.dum$one[is.na(Y11.table.dum$one)] = 0
Y11.table.dum$two[is.na(Y11.table.dum$two)] = 0
Y11.table.dum$three[is.na(Y11.table.dum$three)] = 0
Y11.table.dum$one.00[is.na(Y11.table.dum$one.00)] = 0
Y11.table.dum$one.01[is.na(Y11.table.dum$one.01)] = 0
Y11.table.dum$two.00[is.na(Y11.table.dum$two.00)] = 0
Y11.table.dum$two.01[is.na(Y11.table.dum$two.01)] = 0
Y11.table.dum$three.00[is.na(Y11.table.dum$three.00)] = 0
Y11.table.dum$three.01[is.na(Y11.table.dum$three.01)] = 0


Y00.table.filled$one = Y00.table.dum$one + Y01.table.dum$one.00 + Y11.table.dum$one.00
Y00.table.filled$two = Y00.table.dum$two + Y01.table.dum$two.00 + Y11.table.dum$two.00
Y00.table.filled$three = Y00.table.dum$three + Y01.table.dum$three.00 + Y11.table.dum$three.00

Y01.table.filled$one = Y00.table.dum$one.01 + Y01.table.dum$one + Y11.table.dum$one.01
Y01.table.filled$two = Y00.table.dum$two.01 + Y01.table.dum$two + Y11.table.dum$two.01
Y01.table.filled$three = Y00.table.dum$three.01 + Y01.table.dum$three + Y11.table.dum$three.01

Y11.table.filled$one = Y00.table.dum$one.11 + Y01.table.dum$one.11 + Y11.table.dum$one
Y11.table.filled$two = Y00.table.dum$two.11 + Y01.table.dum$two.11 + Y11.table.dum$two
Y11.table.filled$three = Y00.table.dum$three.11 + Y01.table.dum$three.11 + Y11.table.dum$three
#---------------------------------------------------------------------#
# write to csv

write.csv(Y00.table, "../modeling_data/design2_Y00_table.csv")
write.csv(Y01.table, "../modeling_data/design2_Y01_table.csv")
write.csv(Y11.table, "../modeling_data/design2_Y11_table.csv")

write.csv(Y00.table.filled, "../modeling_data/design2_Y00_table_full.csv")
write.csv(Y01.table.filled, "../modeling_data/design2_Y01_table_full.csv")
write.csv(Y11.table.filled, "../modeling_data/design2_Y11_table_full.csv")



#---------------------------------------------------------------------#
# Sampling distribution CI

Y00.table.filled.keyed = Y00.table.filled
Y01.table.filled.keyed = Y01.table.filled
Y11.table.filled.keyed = Y11.table.filled
Y00.table.filled.keyed$outcome.key = "00"
Y01.table.filled.keyed$outcome.key = "01"
Y11.table.filled.keyed$outcome.key = "11"
Y.table.keyed = rbind(Y00.table.filled.keyed,Y01.table.filled.keyed,Y11.table.filled.keyed)
Y00.table.filled.keyed = NA
Y01.table.filled.keyed = NA
Y11.table.filled.keyed = NA

Y.table.keyed = Y.table.keyed[,c("worker_id","one","two","three","outcome.key")]

Y.table.keyed.one = Y.table.keyed[,c("worker_id","one","outcome.key")]
Y.table.keyed.one$round = "one"
names(Y.table.keyed.one)[names(Y.table.keyed.one) == 'one'] <- 'round_accuracy'

Y.table.keyed.two = Y.table.keyed[,c("worker_id","two","outcome.key")]
Y.table.keyed.two$round = "two"
names(Y.table.keyed.two)[names(Y.table.keyed.two) == 'two'] <- 'round_accuracy'

Y.table.keyed.three = Y.table.keyed[,c("worker_id","three","outcome.key")]
Y.table.keyed.three$round = "three"
names(Y.table.keyed.three)[names(Y.table.keyed.three) == 'three'] <- 'round_accuracy'

Y.table.keyed = rbind(Y.table.keyed.one,Y.table.keyed.two,Y.table.keyed.three)
Y.table.keyed

#---------------------------------------------------------------------#
# write to csv
write.csv(Y.table.keyed, "../modeling_data/design2_Y_table_keyed.csv")
