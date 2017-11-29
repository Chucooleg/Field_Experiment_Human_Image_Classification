library(stringr)

source(file = "design1_data_transformation_functions.r")

ans.design2 = c(
  "Boston Bull", 
  "Shih Tzu",
  "Shih Tzu",
  "Cocker Spaniel",
  "Irish Wolfhound",
  "Golden retriever",
  "Cocker Spaniel",
  "Yorkshire Terrier",
  "Not A Dog",
  "Saluki",
  "Shih Tzu",
  "Yorkshire Terrier",
  "Bloodhound",
  "Shih Tzu",
  "Irish Wolfhound",
  "Saluki",
  "Yorkshire Terrier",
  "Cocker Spaniel",
  "Golden retriever",
  "Golden retriever",
  "Irish Wolfhound",
  "Yorkshire Terrier",
  "Bloodhound",
  "Cocker Spaniel",
  "Saluki",
  "Not A Dog",
  "Boston Bull",
  "Saluki",
  "Cocker Spaniel",
  "Irish Wolfhound",
  "Golden retriever",
  "Yorkshire Terrier",
  "Saluki",
  "Cocker Spaniel",
  "Boston Bull",
  "Yorkshire Terrier",
  "Boston Bull",
  "Golden retriever",
  "Bloodhound",
  "Golden retriever",
  "Bloodhound",
  "Saluki",
  "Not A Dog",
  "Boston Bull",
  "Shih Tzu",
  "Bloodhound",
  "Boston Bull",
  "Irish Wolfhound",
  "Shih Tzu",
  "Irish Wolfhound",
  "Bloodhound"        
)

allQ.design2 = c("TQ1", "TQ2", "TQ3", "TQ4", "TQ5", "TQ6","TQ7", "TQ8", "SQ1",
         "TQ9", "TQ10", "TQ11", "TQ12", "TQ13", "TQ14", "TQ15", "TQ16",
         "TQ17", "TQ18","TQ19", "TQ20", "TQ21", "TQ22", "TQ23", "TQ24", "SQ2",
         "TQ25", "TQ26", "TQ27", "TQ28", "TQ29", "TQ30", "TQ31", "TQ32", 
         "TQ33", "TQ34", "TQ35", "TQ36", "TQ37", "TQ38", "TQ39", "TQ40", "SQ3",
         "TQ41", "TQ42", "TQ43", "TQ44", "TQ45", "TQ46", "TQ47", "TQ48")

# --------------------------------------------------------------------------------------------------

# summarize different groups' performance




# get dataframe of worker ids and their task status
construct_frame_worderIDs_task_status.design2 = function(current_task_data, submitted_MTurk_ids, allQ.design2, 
                                                         payment_accuracy_threshold, task_name, treatment_payrate, existing_path, bonus_rate) {
  
  # for those who completed the task
  dt_completer_status = evaluate_worker_perf.design2(current_task_data, allQ.design2)[,c("worker_id",
                                                                                         "group",
                                                                                         "accuracy",
                                                                                         "accuracy_S1",
                                                                                         "accuracy_S2",
                                                                                         "accuracy_S3",
                                                                                         "screener")]
  dt_completer_status[, complete_task := 1][, payment_accuracy_threshold:=payment_accuracy_threshold]
  completer_repeater_ids = identify_repeaters_from_database(existing_path, dt_completer_status$worker_id)
  dt_completer_status[, repeater := as.numeric(worker_id %in% completer_repeater_ids)]
  
  # for those who did not complete the task
  not_completer_id = current_task_data[toupper(Finished) == "FALSE", ]$worker_id
  dt_not_completer_status = data.table(worker_id = toupper(not_completer_id))
  dt_not_completer_status[, accuracy := NA][, accuracy_S1 := NA][, accuracy_S2 := NA][, accuracy_S3 := NA]
  dt_not_completer_status[, group := NA][, screener := NA][, complete_task := 0][,payment_accuracy_threshold:=payment_accuracy_threshold]
  not_completer_repeater_ids = identify_repeaters_from_database(existing_path, dt_not_completer_status$worker_ids)
  dt_not_completer_status[, repeater := as.numeric(toupper(worker_id) %in% not_completer_repeater_ids)]
  
  
  # for everyone with identifiable worker id
  dt_status = rbind(dt_completer_status, dt_not_completer_status)
  dt_status[, worker_id_found_on_MTurk := as.numeric(toupper(worker_id) %in% toupper(submitted_MTurk_ids))]
  dt_status[, task_name := task_name]
  dt_status[, task_date := substr(current_task_data[1]$StartDate, 1, 10)]
  dt_status[, treatment_payrate := treatment_payrate]  
  dt_status[, pay_or_not := as.numeric(accuracy >= payment_accuracy_threshold & 
                                         screener == 1 & 
                                         complete_task == 1 &
                                         worker_id_found_on_MTurk == 1 &
                                         repeater == 0)]
  dt_status[, total_bonus := pay_or_not*( bonus_rate * str_count(group,"T"))]
  
  dt_status
}
# TEST function
# dum = construct_frame_worderIDs_task_status(current_task_data = current_task_data, allQ = allQ, payment_accuracy_threshold = 0.25, task_name = "pilot 0.25")


convert_raw_attriters.design2 = function(current_task_data) {
  # raw responses
  tmp.dt.attriters = current_task_data[toupper(Finished) == "FALSE"]

  # GET CCC!!!
  tmp.dt.group = tmp.dt.attriters[`CCC_Signal_Page Submit` > 0]
  tmp.dt.CCC = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'CCC')
  
  # GET CCT!!!
  tmp.dt.group = tmp.dt.attriters[`CCT_signal_Page Submit` > 0]
  tmp.dt.CCT = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'CCT')
 
  # GET CTT!!!
  tmp.dt.group = tmp.dt.attriters[`CTT_signal_Page Submit` > 0]
  tmp.dt.CTT = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'CTT') 
  
  # GET TTT!!!
  tmp.dt.group = tmp.dt.attriters[`TTT_signal_Page Submit` > 0]
  tmp.dt.TTT = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'TTT') 
  
  rm(tmp.dt.group, tmp.dt.attriters) # clear up some memory
  tmp.dt.groups = rbind(tmp.dt.CCC, tmp.dt.CCT, tmp.dt.CTT, tmp.dt.TTT)
  tmp.dt.groups
}



convert_raw_to_correct_ans.design2 = function(current_task_data, allQ.design2){
  #raw response 
  tmp.dt.finished = current_task_data[toupper(Finished) == "TRUE", ]  
  
  # GET CCC!!!
  tmp.dt.group = tmp.dt.finished[CCC_TQ1 != "",] 
  # get useful columns out
  tmp.dt.CCC = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'CCC',
                          time_spent_S1 = tmp.dt.group$`Timer_CCC_S1_Page Submit`,
                          time_spent_S2 = tmp.dt.group$`Timer_CCC_S2_Page Submit`,
                          time_spent_S3 = tmp.dt.group$`Timer_CCC_S3_Page Submit`,
                          TQ1 = tmp.dt.group$CCC_TQ1,
                          TQ2 = tmp.dt.group$CCC_TQ2,
                          TQ3 = tmp.dt.group$CCC_TQ3,
                          TQ4 = tmp.dt.group$CCC_TQ4,
                          TQ5 = tmp.dt.group$CCC_TQ5,
                          TQ6 = tmp.dt.group$CCC_TQ6,
                          TQ7 = tmp.dt.group$CCC_TQ7,
                          TQ8 = tmp.dt.group$CCC_TQ8,
                          SQ1 = tmp.dt.group$CCC_SQ1,
                          TQ9 = tmp.dt.group$CCC_TQ9,
                          TQ10 = tmp.dt.group$CCC_TQ10,
                          TQ11 = tmp.dt.group$CCC_TQ11,
                          TQ12 = tmp.dt.group$CCC_TQ12,
                          TQ13 = tmp.dt.group$CCC_TQ13,
                          TQ14 = tmp.dt.group$CCC_TQ14,
                          TQ15 = tmp.dt.group$CCC_TQ15,
                          TQ16 = tmp.dt.group$CCC_TQ16,
                          TQ17 = tmp.dt.group$CCC_TQ17,
                          TQ18 = tmp.dt.group$CCC_TQ18,
                          TQ19 = tmp.dt.group$CCC_TQ19,
                          TQ20 = tmp.dt.group$CCC_TQ20,
                          TQ21 = tmp.dt.group$CCC_TQ21,
                          TQ22 = tmp.dt.group$CCC_TQ22,
                          TQ23 = tmp.dt.group$CCC_TQ23,
                          TQ24 = tmp.dt.group$CCC_TQ24,
                          SQ2 = tmp.dt.group$CCC_SQ2,
                          TQ25 = tmp.dt.group$CCC_TQ25,
                          TQ26 = tmp.dt.group$CCC_TQ26,
                          TQ27 = tmp.dt.group$CCC_TQ27,
                          TQ28 = tmp.dt.group$CCC_TQ28,
                          TQ29 = tmp.dt.group$CCC_TQ29,
                          TQ30 = tmp.dt.group$CCC_TQ30,                        
                          TQ31 = tmp.dt.group$CCC_TQ31,
                          TQ32 = tmp.dt.group$CCC_TQ32,
                          TQ33 = tmp.dt.group$CCC_TQ33,
                          TQ34 = tmp.dt.group$CCC_TQ34,
                          TQ35 = tmp.dt.group$CCC_TQ35,
                          TQ36 = tmp.dt.group$CCC_TQ36,
                          TQ37 = tmp.dt.group$CCC_TQ37,
                          TQ38 = tmp.dt.group$CCC_TQ38,
                          TQ39 = tmp.dt.group$CCC_TQ39,
                          TQ40 = tmp.dt.group$CCC_TQ40,
                          SQ3 = tmp.dt.group$CCC_SQ3,
                          TQ41 = tmp.dt.group$CCC_TQ41,
                          TQ42 = tmp.dt.group$CCC_TQ42,
                          TQ43 = tmp.dt.group$CCC_TQ43,
                          TQ44 = tmp.dt.group$CCC_TQ44,
                          TQ45 = tmp.dt.group$CCC_TQ45,                        
                          TQ46 = tmp.dt.group$CCC_TQ46,                        
                          TQ47 = tmp.dt.group$CCC_TQ47,                         
                          TQ48 = tmp.dt.group$CCC_TQ48)
  # GET CCT!!!
  tmp.dt.group = tmp.dt.finished[CCT_TQ1 != "",] 
  # get useful columns out
  tmp.dt.CCT = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'CCT',
                          time_spent_S1 = tmp.dt.group$`Timer_CCT_S1_Page Submit`,
                          time_spent_S2 = tmp.dt.group$`Timer_CCT_S2_Page Submit`,
                          time_spent_S3 = tmp.dt.group$`Timer_CCT_S3_Page Submit`,
                          TQ1 = tmp.dt.group$CCT_TQ1,
                          TQ2 = tmp.dt.group$CCT_TQ2,
                          TQ3 = tmp.dt.group$CCT_TQ3,
                          TQ4 = tmp.dt.group$CCT_TQ4,
                          TQ5 = tmp.dt.group$CCT_TQ5,
                          TQ6 = tmp.dt.group$CCT_TQ6,
                          TQ7 = tmp.dt.group$CCT_TQ7,
                          TQ8 = tmp.dt.group$CCT_TQ8,
                          SQ1 = tmp.dt.group$CCT_SQ1,
                          TQ9 = tmp.dt.group$CCT_TQ9,
                          TQ10 = tmp.dt.group$CCT_TQ10,
                          TQ11 = tmp.dt.group$CCT_TQ11,
                          TQ12 = tmp.dt.group$CCT_TQ12,
                          TQ13 = tmp.dt.group$CCT_TQ13,
                          TQ14 = tmp.dt.group$CCT_TQ14,
                          TQ15 = tmp.dt.group$CCT_TQ15,
                          TQ16 = tmp.dt.group$CCT_TQ16,
                          TQ17 = tmp.dt.group$CCT_TQ17,
                          TQ18 = tmp.dt.group$CCT_TQ18,
                          TQ19 = tmp.dt.group$CCT_TQ19,
                          TQ20 = tmp.dt.group$CCT_TQ20,
                          TQ21 = tmp.dt.group$CCT_TQ21,
                          TQ22 = tmp.dt.group$CCT_TQ22,
                          TQ23 = tmp.dt.group$CCT_TQ23,
                          TQ24 = tmp.dt.group$CCT_TQ24,
                          SQ2 = tmp.dt.group$CCT_SQ2,
                          TQ25 = tmp.dt.group$CCT_TQ25,
                          TQ26 = tmp.dt.group$CCT_TQ26,
                          TQ27 = tmp.dt.group$CCT_TQ27,
                          TQ28 = tmp.dt.group$CCT_TQ28,
                          TQ29 = tmp.dt.group$CCT_TQ29,
                          TQ30 = tmp.dt.group$CCT_TQ30,                        
                          TQ31 = tmp.dt.group$CCT_TQ31,
                          TQ32 = tmp.dt.group$CCT_TQ32,
                          TQ33 = tmp.dt.group$CCT_TQ33,
                          TQ34 = tmp.dt.group$CCT_TQ34,
                          TQ35 = tmp.dt.group$CCT_TQ35,
                          TQ36 = tmp.dt.group$CCT_TQ36,
                          TQ37 = tmp.dt.group$CCT_TQ37,
                          TQ38 = tmp.dt.group$CCT_TQ38,
                          TQ39 = tmp.dt.group$CCT_TQ39,
                          TQ40 = tmp.dt.group$CCT_TQ40,
                          SQ3 = tmp.dt.group$CCT_SQ3,
                          TQ41 = tmp.dt.group$CCT_TQ41,
                          TQ42 = tmp.dt.group$CCT_TQ42,
                          TQ43 = tmp.dt.group$CCT_TQ43,
                          TQ44 = tmp.dt.group$CCT_TQ44,
                          TQ45 = tmp.dt.group$CCT_TQ45,                        
                          TQ46 = tmp.dt.group$CCT_TQ46,                        
                          TQ47 = tmp.dt.group$CCT_TQ47,                         
                          TQ48 = tmp.dt.group$CCT_TQ48)
  # GET CTT!!!
  tmp.dt.group = tmp.dt.finished[CTT_TQ1 != "",] 
  # get useful columns out
  tmp.dt.CTT = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'CTT',
                          time_spent_S1 = tmp.dt.group$`Timer_CTT_S1_Page Submit`,
                          time_spent_S2 = tmp.dt.group$`Timer_CTT_S2_Page Submit`,
                          time_spent_S3 = tmp.dt.group$`Timer_CTT_S3_Page Submit`,
                          TQ1 = tmp.dt.group$CTT_TQ1,
                          TQ2 = tmp.dt.group$CTT_TQ2,
                          TQ3 = tmp.dt.group$CTT_TQ3,
                          TQ4 = tmp.dt.group$CTT_TQ4,
                          TQ5 = tmp.dt.group$CTT_TQ5,
                          TQ6 = tmp.dt.group$CTT_TQ6,
                          TQ7 = tmp.dt.group$CTT_TQ7,
                          TQ8 = tmp.dt.group$CTT_TQ8,
                          SQ1 = tmp.dt.group$CTT_SQ1,
                          TQ9 = tmp.dt.group$CTT_TQ9,
                          TQ10 = tmp.dt.group$CTT_TQ10,
                          TQ11 = tmp.dt.group$CTT_TQ11,
                          TQ12 = tmp.dt.group$CTT_TQ12,
                          TQ13 = tmp.dt.group$CTT_TQ13,
                          TQ14 = tmp.dt.group$CTT_TQ14,
                          TQ15 = tmp.dt.group$CTT_TQ15,
                          TQ16 = tmp.dt.group$CTT_TQ16,
                          TQ17 = tmp.dt.group$CTT_TQ17,
                          TQ18 = tmp.dt.group$CTT_TQ18,
                          TQ19 = tmp.dt.group$CTT_TQ19,
                          TQ20 = tmp.dt.group$CTT_TQ20,
                          TQ21 = tmp.dt.group$CTT_TQ21,
                          TQ22 = tmp.dt.group$CTT_TQ22,
                          TQ23 = tmp.dt.group$CTT_TQ23,
                          TQ24 = tmp.dt.group$CTT_TQ24,
                          SQ2 = tmp.dt.group$CTT_SQ2,
                          TQ25 = tmp.dt.group$CTT_TQ25,
                          TQ26 = tmp.dt.group$CTT_TQ26,
                          TQ27 = tmp.dt.group$CTT_TQ27,
                          TQ28 = tmp.dt.group$CTT_TQ28,
                          TQ29 = tmp.dt.group$CTT_TQ29,
                          TQ30 = tmp.dt.group$CTT_TQ30,                        
                          TQ31 = tmp.dt.group$CTT_TQ31,
                          TQ32 = tmp.dt.group$CTT_TQ32,
                          TQ33 = tmp.dt.group$CTT_TQ33,
                          TQ34 = tmp.dt.group$CTT_TQ34,
                          TQ35 = tmp.dt.group$CTT_TQ35,
                          TQ36 = tmp.dt.group$CTT_TQ36,
                          TQ37 = tmp.dt.group$CTT_TQ37,
                          TQ38 = tmp.dt.group$CTT_TQ38,
                          TQ39 = tmp.dt.group$CTT_TQ39,
                          TQ40 = tmp.dt.group$CTT_TQ40,
                          SQ3 = tmp.dt.group$CTT_SQ3,
                          TQ41 = tmp.dt.group$CTT_TQ41,
                          TQ42 = tmp.dt.group$CTT_TQ42,
                          TQ43 = tmp.dt.group$CTT_TQ43,
                          TQ44 = tmp.dt.group$CTT_TQ44,
                          TQ45 = tmp.dt.group$CTT_TQ45,                        
                          TQ46 = tmp.dt.group$CTT_TQ46,                        
                          TQ47 = tmp.dt.group$CTT_TQ47,                         
                          TQ48 = tmp.dt.group$CTT_TQ48)
  # GET TTT!!!
  tmp.dt.group = tmp.dt.finished[TTT_TQ1 != "",] 
  # get useful columns out
  tmp.dt.TTT = data.table(worker_id = (tmp.dt.group$worker_id),
                          CQ1 = tmp.dt.group$CQ1,
                          CQ2_1 = tmp.dt.group$CQ2_1,
                          CQ2_2 = tmp.dt.group$CQ2_2,
                          CQ2_3 = tmp.dt.group$CQ2_3,
                          CQ2_4 = tmp.dt.group$CQ2_4,
                          CQ3 = tmp.dt.group$CQ3,
                          CQ4 = tmp.dt.group$CQ4,
                          CQ5 = tmp.dt.group$CQ5,
                          group = 'TTT',
                          time_spent_S1 = tmp.dt.group$`Timer_TTT_S1_Page Submit`,
                          time_spent_S2 = tmp.dt.group$`Timer_TTT_S2_Page Submit`,
                          time_spent_S3 = tmp.dt.group$`Timer_TTT_S3_Page Submit`,
                          TQ1 = tmp.dt.group$TTT_TQ1,
                          TQ2 = tmp.dt.group$TTT_TQ2,
                          TQ3 = tmp.dt.group$TTT_TQ3,
                          TQ4 = tmp.dt.group$TTT_TQ4,
                          TQ5 = tmp.dt.group$TTT_TQ5,
                          TQ6 = tmp.dt.group$TTT_TQ6,
                          TQ7 = tmp.dt.group$TTT_TQ7,
                          TQ8 = tmp.dt.group$TTT_TQ8,
                          SQ1 = tmp.dt.group$TTT_SQ1,
                          TQ9 = tmp.dt.group$TTT_TQ9,
                          TQ10 = tmp.dt.group$TTT_TQ10,
                          TQ11 = tmp.dt.group$TTT_TQ11,
                          TQ12 = tmp.dt.group$TTT_TQ12,
                          TQ13 = tmp.dt.group$TTT_TQ13,
                          TQ14 = tmp.dt.group$TTT_TQ14,
                          TQ15 = tmp.dt.group$TTT_TQ15,
                          TQ16 = tmp.dt.group$TTT_TQ16,
                          TQ17 = tmp.dt.group$TTT_TQ17,
                          TQ18 = tmp.dt.group$TTT_TQ18,
                          TQ19 = tmp.dt.group$TTT_TQ19,
                          TQ20 = tmp.dt.group$TTT_TQ20,
                          TQ21 = tmp.dt.group$TTT_TQ21,
                          TQ22 = tmp.dt.group$TTT_TQ22,
                          TQ23 = tmp.dt.group$TTT_TQ23,
                          TQ24 = tmp.dt.group$TTT_TQ24,
                          SQ2 = tmp.dt.group$TTT_SQ2,
                          TQ25 = tmp.dt.group$TTT_TQ25,
                          TQ26 = tmp.dt.group$TTT_TQ26,
                          TQ27 = tmp.dt.group$TTT_TQ27,
                          TQ28 = tmp.dt.group$TTT_TQ28,
                          TQ29 = tmp.dt.group$TTT_TQ29,
                          TQ30 = tmp.dt.group$TTT_TQ30,                        
                          TQ31 = tmp.dt.group$TTT_TQ31,
                          TQ32 = tmp.dt.group$TTT_TQ32,
                          TQ33 = tmp.dt.group$TTT_TQ33,
                          TQ34 = tmp.dt.group$TTT_TQ34,
                          TQ35 = tmp.dt.group$TTT_TQ35,
                          TQ36 = tmp.dt.group$TTT_TQ36,
                          TQ37 = tmp.dt.group$TTT_TQ37,
                          TQ38 = tmp.dt.group$TTT_TQ38,
                          TQ39 = tmp.dt.group$TTT_TQ39,
                          TQ40 = tmp.dt.group$TTT_TQ40,
                          SQ3 = tmp.dt.group$TTT_SQ3,
                          TQ41 = tmp.dt.group$TTT_TQ41,
                          TQ42 = tmp.dt.group$TTT_TQ42,
                          TQ43 = tmp.dt.group$TTT_TQ43,
                          TQ44 = tmp.dt.group$TTT_TQ44,
                          TQ45 = tmp.dt.group$TTT_TQ45,                        
                          TQ46 = tmp.dt.group$TTT_TQ46,                        
                          TQ47 = tmp.dt.group$TTT_TQ47,                         
                          TQ48 = tmp.dt.group$TTT_TQ48)
  
  rm(tmp.dt.group, tmp.dt.finished) # clear up some memory
  tmp.dt.groups = rbind(tmp.dt.CCC, tmp.dt.CCT, tmp.dt.CTT, tmp.dt.TTT)
  
  # correct response scaffold
  tmp.dt.correct = data.table(worker_id = tmp.dt.groups$worker_id,
                              CQ1 = tmp.dt.groups$CQ1,
                              CQ2_1 = tmp.dt.groups$CQ2_1,
                              CQ2_2 = tmp.dt.groups$CQ2_2,
                              CQ2_3 = tmp.dt.groups$CQ2_3,
                              CQ2_4 = tmp.dt.groups$CQ2_4,
                              CQ3 = tmp.dt.groups$CQ3,
                              CQ4 = tmp.dt.groups$CQ4,
                              CQ5 = tmp.dt.groups$CQ5,
                              group = tmp.dt.groups$group,
                              time_spent_S1 = tmp.dt.groups$time_spent_S1,
                              time_spent_S2 = tmp.dt.groups$time_spent_S2,
                              time_spent_S3 = tmp.dt.groups$time_spent_S3)
  
  
  # correct responses fill in
  for (i in 1:length(allQ.design2)) {
    question.name = allQ.design2[i]
    answer = ans.design2[i]
    column.correct = as.numeric(tmp.dt.groups[,get(question.name)] == answer)
    new_col = data.table("dum_name" = column.correct)
    names(new_col)[names(new_col) == "dum_name"] <- question.name
    tmp.dt.correct = cbind(tmp.dt.correct, new_col)
  }
  tmp.dt.correct
}



# evaluate worker performance for those who completed the task
# screener questions included in evaluation
# return new data table
evaluate_worker_perf.design2 = function(current_task_data, allQ.design2) {
  # read in correct responses
  tmp.dt.perf = convert_raw_to_correct_ans.design2(current_task_data, allQ.design2)
  # const accuracy col
  tmp.dt.perf[, 
              accuracy:= sum(c(TQ1,TQ2,TQ3,TQ4,TQ5,TQ6,TQ7,TQ8, 
                               SQ1,
                               TQ9,TQ10,TQ11,TQ12,TQ13,TQ14,TQ15,TQ16,
                               TQ17,TQ18,TQ19,TQ20,TQ21,TQ22,TQ23,TQ24,
                               SQ2,
                               TQ25,TQ26,TQ27,TQ28,TQ29,TQ30,TQ31,TQ32,
                               TQ33,TQ34,TQ35,TQ36,TQ37,TQ38,TQ39,TQ40,
                               SQ3,
                               TQ41,TQ42,TQ43,TQ44,TQ45,TQ46,TQ47,TQ48))/length(allQ.design2),
              by = worker_id]
  
  tmp.dt.perf[, 
              accuracy_S1 := sum(c(TQ1,TQ2,TQ3,TQ4,TQ5,TQ6,TQ7,TQ8,
                                   SQ1,
                                   TQ9,TQ10,TQ11,TQ12,TQ13,TQ14,TQ15,TQ16))/17,
              by = worker_id]  
  
              
  tmp.dt.perf[, 
              accuracy_S2 := sum(c(TQ17,TQ18,TQ19,TQ20,TQ21,TQ22,TQ23,TQ24,
                                   SQ2,
                                   TQ25,TQ26,TQ27,TQ28,TQ29,TQ30,TQ31,TQ32))/17,
              by = worker_id]   

  
  tmp.dt.perf[, 
              accuracy_S3 := sum(c(TQ33,TQ34,TQ35,TQ36,TQ37,TQ38,TQ39,TQ40,
                                   SQ3,
                                   TQ41,TQ42,TQ43,TQ44,TQ45,TQ46,TQ47,TQ48))/17,
              by = worker_id] 
  
  tmp.dt.perf[, 
              time_spent := as.numeric(time_spent_S1)+as.numeric(time_spent_S2)+as.numeric(time_spent_S3),
              by = worker_id] 
              
  # const screener col
  tmp.dt.perf[,screener:= as.numeric(SQ1==1 & SQ2==1 & SQ3==1)]
  tmp.dt.perf[,screener_S1:= as.numeric(SQ1==1)]
  tmp.dt.perf[,screener_S2:= as.numeric(SQ2==1)]
  tmp.dt.perf[,screener_S3:= as.numeric(SQ3==1)]
  dt.perf = tmp.dt.perf[,c("worker_id", 
                           "CQ1", "CQ2_1", "CQ2_2", "CQ2_3", "CQ2_4", "CQ3", "CQ4", "CQ5", 
                           "group",
                           "time_spent","time_spent_S1", "time_spent_S2", "time_spent_S3", 
                           "accuracy",  "accuracy_S1", "accuracy_S2","accuracy_S3",
                           "screener", "screener_S1", "screener_S2", "screener_S3")]
  dt.perf
}



# provide stats summaries of accuracies over all workers given a csv
summarize_worker_perf.design2 = function(current_task_data, allQ.design2) {
  all_workers.dt = evaluate_worker_perf.design2(current_task_data,allQ.design2)
  
  tmp.group.CCC = all_workers.dt[group == "CCC", .(
    group = "CCC",
    mean = mean(accuracy),
    SD = sd(accuracy),
    min = min(accuracy),
    max = max(accuracy),
    mean_S1 = mean(accuracy_S1),
    mean_S2 = mean(accuracy_S2),
    mean_S3 = mean(accuracy_S3))]
  
  tmp.group.CCT = all_workers.dt[group == "CCT", .(
    group = "CCT",
    mean = mean(accuracy),
    SD = sd(accuracy),
    min = min(accuracy),
    max = max(accuracy),
    mean_S1 = mean(accuracy_S1),
    mean_S2 = mean(accuracy_S2),
    mean_S3 = mean(accuracy_S3))]
  
  tmp.group.CTT = all_workers.dt[group == "CTT", .(
    group = "CTT",
    mean = mean(accuracy),
    SD = sd(accuracy),
    min = min(accuracy),
    max = max(accuracy),
    mean_S1 = mean(accuracy_S1),
    mean_S2 = mean(accuracy_S2),
    mean_S3 = mean(accuracy_S3))]
  
  tmp.group.TTT = all_workers.dt[group == "TTT", .(
    group = "TTT",
    mean = mean(accuracy),
    SD = sd(accuracy),
    min = min(accuracy),
    max = max(accuracy),
    mean_S1 = mean(accuracy_S1),
    mean_S2 = mean(accuracy_S2),
    mean_S3 = mean(accuracy_S3))]
  
  rbind(tmp.group.CCC,tmp.group.CCT,tmp.group.CTT,tmp.group.TTT)
}



# evaluate accuracy per question
# of a particular question, how many people got it right?
evaluate_question_perf.design2 = function(current_task_data, allQ.design2) {
  tmp.dt.questions = convert_raw_to_correct_ans.design2(current_task_data, allQ.design2)
  tmp.dt.questions = tmp.dt.questions[, c("TQ1","TQ2","TQ3","TQ4","TQ5","TQ6","TQ7","TQ8",
                                          "SQ1",
                                          "TQ9","TQ10","TQ11","TQ12","TQ13","TQ14","TQ15","TQ16",
                                          
                                          "TQ17","TQ18","TQ19","TQ20","TQ21","TQ22","TQ23","TQ24",
                                          "SQ2",
                                          "TQ25","TQ26","TQ27","TQ28","TQ29","TQ30","TQ31","TQ32",
                                          
                                          "TQ33","SQ2","TQ34","TQ35","TQ36","TQ37","TQ38","TQ39","TQ40",
                                          "SQ3",
                                          "TQ41","TQ42","TQ43","TQ44","TQ45","TQ46","TQ47","TQ48")]
  tmp.dt.questions$dum = 1
  aggregate(. ~ dum,data = tmp.dt.questions, FUN = mean)
}



# provide stats summaries of accuracies over all questions given a csv
# screener questions not included
summarize_question_accuracy.design2 = function(current_task_data, allQ.design2) {
  # read in correct responses
  tmp.dt.questions = convert_raw_to_correct_ans.design2(current_task_data, allQ.design2)
  n = length(allQ) - 2 # minus 2 screener questions
  tmp.dt.questions[, .(
    mean = mean(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                  TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                  TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                  TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                  TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                  TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                  TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                  TQ43, TQ44, TQ45, TQ46, TQ47, TQ48)),
    sd = sd(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
              TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
              TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
              TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
              TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
              TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
              TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
              TQ43, TQ44, TQ45, TQ46, TQ47, TQ48)),
    se = sd(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
              TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
              TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
              TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
              TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
              TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
              TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
              TQ43, TQ44, TQ45, TQ46, TQ47, TQ48))/sqrt(n),
    min = min(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                TQ43, TQ44, TQ45, TQ46, TQ47, TQ48)),
    max = max(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                TQ43, TQ44, TQ45, TQ46, TQ47, TQ48))
  )]
}