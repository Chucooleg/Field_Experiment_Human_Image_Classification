# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 1, PAYMENT RATE = 0.10

# read in qualtric output csv
qualtric_data_path_0.10 = "../qualtric_data/20171111_qualtric_results_order1_0.10.csv"
current_task_data_0.10 = get_current_task_data(qualtric_data_path_0.10)

# evaluate accuracy per question
# of a particular question, how many people got it right?
question_perf_0.10 = evaluate_question_perf(current_task_data_0.10, allQ)
question_perf_0.10

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.10, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.10 = evaluate_worker_perf(current_task_data_0.10, allQ)
worker_perf_0.10

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.10, allQ)

#number of observations valid for regression
nrow(worker_perf_0.10)

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 1, PAYMENT RATE = 0.55

# read in qualtric output csv
qualtric_data_path_0.55 = "../qualtric_data/20171111_qualtric_results_order1_0.55.csv"
current_task_data_0.55 = get_current_task_data(qualtric_data_path_0.55)

# !!! REMOVE REPEATERS : turks who checked out the 0.10 task already
filter = !(current_task_data_0.55$worker_id %in% current_task_data_0.10$worker_id)
# get number of violaters
sum_spillover = sum(!filter)
# weed out the violaters 
current_task_data_0.55_weeded = current_task_data_0.55[filter, ]

# evaluate accuracy per question
# of a particular question, how many people got it right?
question_perf_0.55 = evaluate_question_perf(current_task_data_0.55_weeded, allQ)
question_perf_0.55

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.55_weeded, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.55 = evaluate_worker_perf(current_task_data_0.55_weeded, allQ)
worker_perf_0.55

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.55_weeded, allQ)

#number of observations valid for regression
nrow(worker_perf_0.55)

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 1, PAYMENT RATE = 0.40

# Please fill in (reuse above code block)

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 1, PAYMENT RATE = 0.25

# Please fill in (reuse above code block)

#---------------------------------------------------------------------#
# POOLING TWO CSV FILES FROM DIFFERENT TREATMENTS

# pool the data from different treatments together
worker_perf_0.10$treatment = 0.10
worker_perf_0.55$treatment = 0.55
#worker_perf_0.55$treatment = 0.40
#worker_perf_0.55$treatment = 0.25
regr_table = rbind(worker_perf_0.10, worker_perf_0.55) # TEMPORARY
# regr_table = rbind(worker_perf_0.10, worker_perf_0.25, worker_perf_0.40, worker_perf_0.55) # ONCE ALL FOUR POSTINGS ARE DONE


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
lmtest::coeftest(cov_regr_CQ1_1, vcov(cov_regr_CQ1_1))

cov_regr_CQ1_2= lm(CQ1_2 ~ regr_table$treatment)
lmtest::coeftest(cov_regr_CQ1_2, vcov(cov_regr_CQ1_2))

cov_regr_CQ1_3= lm(CQ1_3 ~ regr_table$treatment)
lmtest::coeftest(cov_regr_CQ1_3, vcov(cov_regr_CQ1_3))

# Preference to work with images question
cov_regr_CQ2_3= lm(CQ2_3 ~ treatment, data = regr_table)
lmtest::coeftest(cov_regr_CQ2_3, vcov(cov_regr_CQ2_3))

# Lived with or planned to own a dog
CQ3_1 = regr_table$CQ3 == "Yes"
CQ3_2 = regr_table$CQ3 == "No"
CQ3_3 = regr_table$CQ3 == "Maybe"

cov_regr_CQ3_1= lm(CQ3_1 ~ regr_table$treatment)
lmtest::coeftest(cov_regr_CQ3_1, vcov(cov_regr_CQ3_1))

cov_regr_CQ3_2= lm(CQ3_2 ~ regr_table$treatment)
lmtest::coeftest(cov_regr_CQ3_2, vcov(cov_regr_CQ3_2))

cov_regr_CQ3_3= lm(CQ3_3 ~ regr_table$treatment)
lmtest::coeftest(cov_regr_CQ3_3, vcov(cov_regr_CQ3_3))

#---------------------------------------------------------------------#
# ESTIMATING ATE

# TWO-SAMPLE T-TEST
# test if variance of the two groups are unequal
car::leveneTest(worker_perf_0.10$accuracy,worker_perf_0.55$accuracy,center=median)
# 2 sample independent t-test
t.test(worker_perf_0.10$accuracy,
       worker_perf_0.55$accuracy,
       alternative = "two.sided", var.equal = TRUE)

# REGRESSION
regr1 = lm(accuracy ~ treatment, data = regr_table)
regr2 = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3, data = regr_table)
summary(regr1)
summary(regr2)
lmtest::coeftest(regr1, vcov(regr1))
lmtest::coeftest(regr2, vcov(regr2))


# CAN REPEAT TWO-SAMPLE T-TEST FOR THE OTHER PRICES '
# (0.10 vs 0.25) (0.25 vs 0.40), (0.40 vs 0.55)



#---------------------------------------------------------------------#
# MISSING : RANDOMIZATION INFERENCE (for CACE -- compliers average causal effect)

# !!!PLEASE FILL IN

#---------------------------------------------------------------------#
# DISTRIBUTION OF ACCURACY

# pooling 0.10 & 0.25 treatments
hist(regr_table$accuracy)

hist(regr_table[treatment == 0.55,]$accuracy)
hist(regr_table[treatment == 0.10,]$accuracy)
#hist(regr_table[treatment == 0.25,]$accuracy)
#hist(regr_table[treatment == 0.40,]$accuracy)

# Notice that both histograms are very left skewed
# This calls the appropriateness of OLS asymptotics for ATE SE & p-val estimation into question
# Hopefully with a larger sample size, asymtotics will be more reliable
# Anyhow, this says that we should report p-val using randomization inference in addition to regression
# Just like many of the papers we read in class