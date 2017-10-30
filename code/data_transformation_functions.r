library(data.table)
library(magrittr)

ans = c(
  "Boston Bull", 
  "Shih Tzu",
  "Shih Tzu",
  "Cocker Spaniel",
  "Irish wolfhound",
  "Golden retriever",
  "Cocker Spaniel",
  "Yorkshire Terrier",
  "Saluki",
  "Shih Tzu",
  "Yorkshire Terrier",
  "Bloodhound",
  "Shih Tzu",
  "Irish wolfhound",
  "Saluki",
  "Yorkshire Terrier",
  "Not A Dog",
  "Cocker Spaniel",
  "Golden retriever",
  "Golden retriever",
  "Irish wolfhound",
  "Yorkshire Terrier",
  "Bloodhound",
  "Cocker Spaniel",
  "Saluki",
  "Boston Bull",
  "Saluki",
  "Cocker Spaniel",
  "Irish wolfhound",
  "Golden retriever",
  "Yorkshire Terrier",
  "Saluki",
  "Cocker Spaniel",
  "Boston Bull",
  "Not A Dog",
  "Yorkshire Terrier",
  "Boston Bull",
  "Golden retriever",
  "Bloodhound",
  "Golden retriever",
  "Bloodhound",
  "Saluki",
  "Boston Bull",
  "Shih Tzu",
  "Bloodhound",
  "Boston Bull",
  "Irish wolfhound",
  "Shih Tzu",
  "Irish wolfhound",
  "Bloodhound"        
)

allQ = c("TQ1", "TQ2", "TQ3", "TQ4", "TQ5", "TQ6",
         "TQ7", "TQ8", "TQ9", "TQ10", "TQ11", "TQ12",
         "TQ13", "TQ14", "TQ15", "TQ16", "SQ1","TQ17", "TQ18",
         "TQ19", "TQ20", "TQ21", "TQ22", "TQ23", "TQ24",
         "TQ25", "TQ26", "TQ27", "TQ28", "TQ29", "TQ30",
         "TQ31", "TQ32", "TQ33", "SQ2","TQ34", "TQ35", "TQ36",
         "TQ37", "TQ38", "TQ39", "TQ40", "TQ41", "TQ42",
         "TQ43", "TQ44", "TQ45", "TQ46", "TQ47", "TQ48")

# --------------------------------------------------------------------------------------------------
# get list of turks to assign qualification

# funtion (existing_csv, current_task_data)
# read in existing_csv as existing_dt
# read in current task data as dt
# check accuracies of current task data 
# construct temp_accuracies_dt
# [worder_id, date of task, task description, complete 1/0 ,accuracy, payment_threshold, pay 1/0]
# append temp_accuracies_dt to existing_dt
# overwrite exiting_dt to existing_cv
# return temp_accuracies_dt, existing_dt


# get dataframe of worker ids and their task status
construct_frame_worderIDs_task_status = function(current_task_data, allQ, payment_accuracy_threshold, task_name) {
  # for those who completed the task
  dt_completer_status = evaluate_worker_perf(current_task_data, allQ)[,c("worker_id",
                                                               "accuracy",
                                                               "screener")]
  dt_completer_status[, complete_task := 1][, payment_threshold:=payment_threshold]
  dt_completer_status[, pay_or_not := as.numeric(accuracy >= payment_threshold & screener == 1 & complete_task == 1)]
  # for those who did not complete the task
  not_completer_id = current_task_data[Finished == "FALSE", ]$worker_id
  dt_not_completer_status = data.table(worker_id = not_completer_id)
  dt_not_completer_status[, accuracy := NA][, screener := NA][, complete_task := 0][,payment_threshold:=payment_threshold][,pay_or_not :=0]
  dt_status = rbind(dt_completer_status, dt_not_completer_status)
  dt_status[, task_name := task_name]
  dt_status[, task_date := substr(current_task_data[1]$StartDate, 1, 10)]
  dt_status
}
# TEST function
# dum = construct_frame_worderIDs_task_status(current_task_data = current_task_data, allQ = allQ, payment_accuracy_threshold = 0.25, task_name = "pilot 0.25")




# DO NOT RUN THIS FUNCTION
# construct empty dataframe to append worker ids and task status
# path3 = "../MTurk_worderIDs/MTurk_worderIDsworker_task_status.csv"
DONT_USE_construct_frame_workerIDs_task_empty = function(custom_path) {
  tmp_frame = data.table(key= c("worker_id, StartDate, task_description, complete, 
                                screener, accuracy, payment_threshold, pay_or_not"))
  write.csv(x = tmp_frame, file = custom_path)
}
# DONT_USE_construct_frame_workerIDs_task_empty(path3)


# get MTurk workerid
get_MTurk_worker_id = function(csv_path) {
  current_MTurk_data = fread(csv_path)
  worker_id = current_MTurk_data$WorkerId
  worker_id
}

# get current task data from qualtric
get_current_task_data = function(csv_path) {
  current_task_data = fread(csv_path)[-c(1,2),] # strip off first two rows of non-reposnse information
  current_task_data = current_task_data[StartDate != "",] # stip off any empty rows at the end
}



# convert from raw ans to correct/wrong responses
# return new data table [worder_id, covariates, time_spent, TQ1correct, TQ2correct...]
convert_raw_to_correct_ans = function(current_task_data, allQ) {
  # raw response
  tmp.dt.finished = current_task_data[Finished == "TRUE", ]
  # correct response scaffold
  tmp.dt.correct = data.table(worker_id = tmp.dt.finished$worker_id,
                              CQ1 = tmp.dt.finished$CQ1,
                              CQ2_1 = tmp.dt.finished$CQ2_1,
                              CQ2_2 = tmp.dt.finished$CQ2_2,
                              CQ2_3 = tmp.dt.finished$CQ2_3,
                              CQ2_4 = tmp.dt.finished$CQ2_4,
                              CQ3 = tmp.dt.finished$CQ3,
                              CQ4 = tmp.dt.finished$CQ4,
                              CQ5 = tmp.dt.finished$CQ5,
                              time_spent = tmp.dt.finished$`Timer1_Page Submit`)
  # correct responses fill in
  for (i in 1:length(allQ)) {
    question.name = allQ[i]
    answer = ans[i]
    column.correct = as.numeric(tmp.dt.finished[,get(question.name)] == answer)
    new_col = data.table("dum_name" = column.correct)
    names(new_col)[names(new_col) == "dum_name"] <- question.name
    tmp.dt.correct = cbind(tmp.dt.correct, new_col)
  }
  tmp.dt.correct
}
# convert_raw_to_correct_ans(current_task_data, allQ)


# evaluate worker performance for those who completed the task
# screener questions included in evaluation
# return new data table
evaluate_worker_perf = function(current_task_data, allQ) {
  # read in correct responses
  tmp.dt.perf = convert_raw_to_correct_ans(current_task_data, allQ)
  # const accuracy col
  tmp.dt.perf[, 
             accuracy:= sum(c(TQ1,TQ2,TQ3,TQ4,TQ5,TQ6,
                                   TQ7,TQ8,TQ9,TQ10,TQ11,TQ12,
                                   TQ13,TQ14,TQ15,TQ16,SQ1,TQ17,TQ18,
                                   TQ19,TQ20,TQ21,TQ22,TQ23,TQ24,
                                   TQ25,TQ26,TQ27,TQ28,TQ29,TQ30,
                                   TQ31,TQ32,TQ33,SQ2,TQ34,TQ35,TQ36,
                                   TQ37,TQ38,TQ39,TQ40,TQ41,TQ42,
                                   TQ43,TQ44,TQ45,TQ46,TQ47,TQ48))/length(allQ),
             by = worker_id]
  # const screener col
  tmp.dt.perf[,screener:= as.numeric(SQ1==1 & SQ2==1)]
  dt.perf = tmp.dt.perf[,c("worker_id", "CQ1", "CQ2_1", "CQ2_2", "CQ2_3", "CQ2_4",
                           "CQ3", "CQ4", "CQ5", "time_spent", "accuracy", "screener")]
  dt.perf
}
### test function
# dum_i = 25
# dt.perf$dog_accuracy[dum_i] == mean(as.numeric(dt.perf[,11:60][dum_i]))
### Run fucntion
# evaluate_worker_perf(current_task_data, allQ)


# evaluate question accuracies
# screener questions not included
evaluate_question_accuracy = function(current_task_data, allQ) {
  # read in correct responses
  tmp.dt.questions = convert_raw_to_correct_ans(current_task_data, allQ)
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
# evaluate_question_accuracy(current_task_data, allQ)



# get list of worker ids from turks who viewed the task (regardless of task completion)
get_worderIDs_viewed_task = function(current_task_data) {
  available = current_task_data[worker_id != "", ]$worker_id
  available
}

# get list of worker ids from turks who completed the task
get_worderIDs_completed_task = function(current_task_data) {
  available = current_task_data[Finished == "TRUE", ]$worker_id
  available
}

# get list of worker ids from turks who did not completed the task
get_worderIDs_not_completed_task = function(current_task_data) {
  available = current_task_data[Finished == "FALSE", ]$worker_id
  available
}


#------------------------------------------------

# OLD CODE

