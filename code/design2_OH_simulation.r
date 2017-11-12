library(fGarch)

qualtric_data_path_0.25 = "../qualtric_data/20171028_qualtric_results_pilot_0.25.csv"
qualtric_data_path_0.10 = "../qualtric_data/20171028_qualtric_results_pilot_0.10.csv"
current_task_data_0.25 = get_current_task_data(qualtric_data_path_0.25)
current_task_data_0.10 = get_current_task_data(qualtric_data_path_0.10)
worker_perf_0.25 = evaluate_worker_perf(current_task_data_0.25, allQ)
worker_perf_0.10 = evaluate_worker_perf(current_task_data_0.10, allQ)
sample_accuracies = rbind(worker_perf_0.10, worker_perf_0.25)$accuracy

#--------------------------------------------------------------------------------------------#

Design = data.table(
           Group = c("A","B","C","D"), 
           N = c(100,100,100,100),
           Randomize = c("R", "R", "R", "R"),
           Round1 = c("C", "C", "C", "T"),
           Round2 = c("C", "C", "T", "T"),
           Round3 = c("C", "T", "T", "T"),
           TotalBonus = c(0, 0.15, 0.3, 0.45))

Design

# Function to simulate data for each group
generate_group_data = function(groupname, bonus_R1, bonus_R2, bonus_R3, ID_range) {
  data.table(WorkerID = rep(ID_range,3) ,
             Group = rep(groupname,300),
             Round = c(rep(1,100), rep(2,100), rep(3,100)),
             Bonus = c(rep(bonus_R1,100), rep(bonus_R2,100), rep(bonus_R3,100)),
             accuracy = sample(x = sample_accuracies, size = 300, replace = T),
             time_spent = sample(x = worker_perf_0.10$time_spent, size = 300, replace = T),
             CQ1 = sample(x = worker_perf_0.10$CQ1, size = 300, replace = T),
             CQ2 = sample(x = worker_perf_0.10$CQ2_3, size = 300, replace = T),
             CQ3 = sample(x = worker_perf_0.10$CQ3, size = 300, replace = T),
             CQ4 = sample(x = worker_perf_0.10$CQ4, size = 300, replace = T),
             CQ5 = sample(x = worker_perf_0.10$CQ5, size = 300, replace = T))
}

# simulate data for group A, B, C and D
A.table = generate_group_data(ID_range = 1:100,groupname = "A", bonus_R1 = 0, bonus_R2 = 0, bonus_R3 = 0)
B.table = generate_group_data(ID_range = 101:200,groupname = "B", bonus_R1 = 0, bonus_R2 = 0, bonus_R3 = 0.15)
C.table = generate_group_data(ID_range = 201:300,groupname = "C", bonus_R1 = 0, bonus_R2 = 0.15, bonus_R3 = 0.15)
D.table = generate_group_data(ID_range = 301:400,groupname = "D", bonus_R1 = 0.15, bonus_R2 = 0.15, bonus_R3 = 0.15)
all.table = rbind(A.table, B.table, C.table, D.table)
all.table = all.table[order(WorkerID),]

View(all.table)

write.csv(x=all.table, file = "OH.csv")
#--------------------------------------------------------------------------------------------#

m = lm(accuracy ~ Group * factor(Round), data = all.table)


pred_data = data.frame(
  Round = factor(c(2,3)),
  Group = factor(c('D','D'))
)

pred_data2 = data.frame(
  Round = factor(c(2,3)),
  Group = factor(c('A','A'))
)

predict(m , newdata = pred_data, se.fit = T) 
predict(m , newdata = pred_data2, se.fit = T)

