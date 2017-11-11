# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "pilot_data_transformation_functions.r")

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# PILOT, PAYMENT RATE = 0.25

# read in qualtric output csv
qualtric_data_path_0.25 = "../qualtric_data/20171028_qualtric_results_pilot_0.25.csv"
current_task_data_0.25 = get_current_task_data(qualtric_data_path_0.25)

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.25, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.25 = evaluate_worker_perf(current_task_data_0.25, allQ)
worker_perf_0.25

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.25, allQ)

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# PILOT, PAYMENT RATE = 0.10

# read in qualtric output csv
qualtric_data_path_0.10 = "../qualtric_data/20171028_qualtric_results_pilot_0.10.csv"
current_task_data_0.10 = get_current_task_data(qualtric_data_path_0.10)

# evaluate accuracy per question
# of a particular question, how many people got it right?

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.10, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.10 = evaluate_worker_perf(current_task_data_0.10, allQ)
worker_perf_0.10

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.10, allQ)

#---------------------------------------------------------------------#
# POOLING TWO CSV FILES FROM DIFFERENT TREATMENTS

# pool the data from different treatments together
worker_perf_0.25$treatment = 0.25
worker_perf_0.10$treatment = 0.10
regr_table = rbind(worker_perf_0.10, worker_perf_0.25)
# our covariates are CQ1, CQ2_3, CQ3
# converting some data type of some covariates
regr_table$CQ1 = as.factor(regr_table$CQ1)
regr_table$CQ2_3 = as.numeric(regr_table$CQ2_3)
regr_table$CQ3 = as.factor(regr_table$CQ3)

#---------------------------------------------------------------------#
# CHECK COVARIATE BALANCE

# Dog friends question
CQ1_1 = regr_table$CQ1 == "a lot less than half"
CQ1_2 = regr_table$CQ1 == "around half"
CQ1_3 = regr_table$CQ1 == "a lot more than half"
cov_regr_CQ1_1= lm(CQ1_1 ~ regr_table$treatment)
cov_regr_CQ1_2= lm(CQ1_2 ~ regr_table$treatment)
cov_regr_CQ1_3= lm(CQ1_3 ~ regr_table$treatment)

# Preference to work with images question
cov_regr_CQ2_3= lm(CQ2_3 ~ treatment, data = regr_table)

# Lived with or planned to own a dog
CQ3_1 = regr_table$CQ3 == "Yes"
CQ3_2 = regr_table$CQ3 == "No"
CQ3_3 = regr_table$CQ3 == "Maybe"
cov_regr_CQ3_1= lm(CQ3_1 ~ regr_table$treatment)
cov_regr_CQ3_2= lm(CQ3_2 ~ regr_table$treatment)
cov_regr_CQ3_3= lm(CQ3_3 ~ regr_table$treatment)


#---------------------------------------------------------------------#
# ESTIMATING ATE

# TWO-SAMPLE T-TEST
# test if variance of the two groups are unequal
car::leveneTest(worker_perf_0.10$accuracy,worker_perf_0.25$accuracy,center=median)
# 2 sample independent t-test
t.test(worker_perf_0.10$accuracy,
       worker_perf_0.25$accuracy,
       alternative = "two.sided", var.equal = TRUE)

# REGRESSION
regr1 = lm(accuracy ~ treatment, data = regr_table)
regr2 = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3, data = regr_table)
summary(regr1)
summary(regr2)
coeftest(regr1, vcov(regr1))
coeftest(regr2, vcov(regr2))

#---------------------------------------------------------------------#
# DISTRIBUTION OF ACCURACY

# pooling 0.10 & 0.25 treatments
hist(regr_table$accuracy)

hist(regr_table[treatment == 0.25,]$accuracy)
hist(regr_table[treatment == 0.10,]$accuracy)
# Notice that both histograms are very left skewed
# This calls the appropriateness of OLS asymptotics for ATE SE & p-val estimation into question
# Hopefully with a larger sample size, asymtotics will be more reliable
# Anyhow, this says that we should report p-val using randomization inference in addition to regression
# Just like many of the papers we read in class