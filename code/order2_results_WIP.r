# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")
source(file = "design1_data_analysis_functions.r")

#---------------------------------------------------------------------#
# NOTES FROM PROBLEM SHEETS AND ANY CAVEATS?

## Combining experiments:  See PS2, problem 2, part d, about pooling data from two different studies that have very different means and std errors.
##  This causes biased results if the data is just pooled. we should use Equation 3.10 in FE Ch.3, this is the right way.
## Discuss confidence intervals
## Do we need to worry about clustering in this case? I guess our experiments aren't separate clusters as we are getting samples from the same population (roughly)
##  probably a good idea to briefly discuss this in our final report. 
## Blocking? I guess we don't have it...
## Formulate results from all regressions in nice tables, like the ones seen in readings. 
## Dimensions of our experiment? Also, we should probably create those cool simple regression models with the X's Os and Ts
## Do we have Compliers and Never-takers? Are they enough to make a difference?

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 2, PAYMENT RATE = 0.40

# read in qualtric output csv
qualtric_data_path_0.40 = "../qualtric_data/20171118_qualtric_results_order2_0.40.csv"
current_task_data_0.40 = get_current_task_data(qualtric_data_path_0.40)

# evaluate accuracy per question
# of a particular question, how many people got it right?
question_perf_0.40 = evaluate_question_perf(current_task_data_0.40, allQ)
question_perf_0.40

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.40, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.40 = evaluate_worker_perf(current_task_data_0.40, allQ)
worker_perf_0.40

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.40, allQ)

#number of observations valid for regression
nrow(worker_perf_0.40)

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 2, PAYMENT RATE = 0.25

# read in qualtric output csv
qualtric_data_path_0.25 = "../qualtric_data/20171118_qualtric_results_order2_0.25.csv"
current_task_data_0.25 = get_current_task_data(qualtric_data_path_0.25)

# !!! REMOVE REPEATERS : turks who checked out the 0.10 task already
filter = !(current_task_data_0.25$worker_id %in% current_task_data_0.40$worker_id)
# get number of violaters
sum_spillover = sum(!filter)
# weed out the violaters 
current_task_data_0.25_weeded = current_task_data_0.25[filter, ]

# evaluate accuracy per question
# of a particular question, how many people got it right?
question_perf_0.25 = evaluate_question_perf(current_task_data_0.25_weeded, allQ)
question_perf_0.25

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.25_weeded, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.25 = evaluate_worker_perf(current_task_data_0.25_weeded, allQ)
worker_perf_0.25

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.25_weeded, allQ)

#number of observations valid for regression
nrow(worker_perf_0.25)

#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 2, PAYMENT RATE = 0.55

# Please fill in (reuse above code block)
# read in qualtric output csv
qualtric_data_path_0.55 = "../qualtric_data/20171119_qualtric_results_order2_0.55.csv"


current_task_data_0.55 = get_current_task_data(qualtric_data_path_0.55)

# evaluate accuracy per question
# of a particular question, how many people got it right?
question_perf_0.55 = evaluate_question_perf(current_task_data_0.55, allQ)
question_perf_0.55

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.55, allQ)




#evaluate accuracy per worker, return a table per worker
worker_perf_0.55 = evaluate_worker_perf(current_task_data_0.55, allQ)
worker_perf_0.55

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.55, allQ)

#number of observations valid for regression
nrow(worker_perf_0.55)


#---------------------------------------------------------------------#
# FOCUS ON A SINGLE CSV FILE CORRESPONDING TO A SINGLE TREATMENT
# ORDER 2, PAYMENT RATE = 0.10

# Please fill in (reuse above code block)
# read in qualtric output csv
qualtric_data_path_0.10 = "../qualtric_data/20171119_qualtric_results_order2_0.10.csv"
current_task_data_0.10 = get_current_task_data(qualtric_data_path_0.10)

# !!! REMOVE REPEATERS : turks who checked out the 0.40 task already
filter = !(current_task_data_0.10$worker_id %in% current_task_data_0.55$worker_id)
# get number of violaters
sum_spillover = sum(!filter)
# weed out the violaters 
current_task_data_0.10_weeded = current_task_data_0.10[filter, ]

# evaluate accuracy per question
# of a particular question, how many people got it right?
question_perf_0.10 = evaluate_question_perf(current_task_data_0.10_weeded, allQ)
question_perf_0.10

#stats summary of accuracies over all questions
summarize_question_accuracy(current_task_data_0.10_weeded, allQ)

#evaluate accuracy per worker, return a table per worker
worker_perf_0.10 = evaluate_worker_perf(current_task_data_0.10_weeded, allQ)
worker_perf_0.10

#stats summary of accuracies over all workers
summarize_worker_perf(current_task_data_0.10_weeded, allQ)

#number of observations valid for regression
nrow(worker_perf_0.10)

#---------------------------------------------------------------------#
# POOLING FOUR (0.10, 0.25, 0.40, 0.55) CSV FILES FROM DIFFERENT TREATMENTS

# pool the data from different treatments together
worker_perf_0.10$treatment = 0.10
worker_perf_0.55$treatment = 0.55
worker_perf_0.40$treatment = 0.40
worker_perf_0.25$treatment = 0.25
regr_table = rbind(worker_perf_0.10, worker_perf_0.25, worker_perf_0.40, worker_perf_0.55) # ONCE ALL FOUR POSTINGS ARE DONE


# our covariates are CQ1, CQ2_3, CQ3
# converting some data type of some covariates
regr_table$CQ1 = as.factor(regr_table$CQ1)
regr_table$CQ2_3 = as.numeric(regr_table$CQ2_3)
regr_table$CQ3 = as.factor(regr_table$CQ3)


# converting data types for Q4 and Q5 if we decide to use it
# remember that these are bad controls in Design1, never regress on them
# on the other hand, they are OK in Design2, can regress on then
# regr_table$CQ5 = regr_table$CQ5 == "Yes"
# regr_table$CQ4 = as.factor(regr_table$CQ4)
#---------------------------------------------------------------------#
# POOLING THREE (0.25, 0.40, 0.55) CSV FILES FROM DIFFERENT TREATMENTS

# pool the data from different treatments together
regr_table_exclude0.10 = rbind(worker_perf_0.25, worker_perf_0.40, worker_perf_0.55)

# our covariates are CQ1, CQ2_3, CQ3
# converting some data type of some covariates
regr_table_exclude0.10$CQ1 = as.factor(regr_table_exclude0.10$CQ1)
regr_table_exclude0.10$CQ2_3 = as.numeric(regr_table_exclude0.10$CQ2_3)
regr_table_exclude0.10$CQ3 = as.factor(regr_table_exclude0.10$CQ3)

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

# ATE by T-test
est.t.test(worker_perf_0.10$accuracy,worker_perf_0.55$accuracy, "two.sided")
est.t.test(worker_perf_0.10$accuracy,worker_perf_0.25$accuracy, "two.sided")
est.t.test(worker_perf_0.10$accuracy,worker_perf_0.40$accuracy, "two.sided")
est.t.test(worker_perf_0.25$accuracy,worker_perf_0.40$accuracy, "two.sided")
est.t.test(worker_perf_0.25$accuracy,worker_perf_0.55$accuracy, "two.sided")
est.t.test(worker_perf_0.40$accuracy,worker_perf_0.55$accuracy, "two.sided")

# ATE by REGRESSION
# REMEMBER TO REPORT
# -COEFFICIENT INTERPRETATION
# -STANDARD ERRORS
# -CONFIDENCE INTERVAL
# -F-TEST on the 3 covariates as a block (restricted only has treatment)(full has CQ1-CQ3 as well)
# ---we want to see if the covariates as a block has any explanatory power
est.regr.simple(regr_table) #ATE = 0.87 result is problematic 
est.regr.covars(regr_table) #ATE = 0.87 result is problematic 


est.regr.simple(regr_table_exclude0.10) # ATE = 0.23
est.regr.covars(regr_table_exclude0.10) # ATE = 0.20

#---------------------------------------------------------------------#
# RANDOMIZATION INFERENCE

n10 = nrow(worker_perf_0.10)
n25 = nrow(worker_perf_0.25)
n40 = nrow(worker_perf_0.40)
n55 = nrow(worker_perf_0.55)

rand_ass = function() sample(c(rep(0.1,n10),rep(0.25,n25), rep(0.4,n40), rep(0.55,n55)))

# INCLUDING ALL four postings
# treatment = rand_ass()
ate = est.ri.ate(regr_table,regr_table$treatment)
all_ate <- replicate(5000, est.ri.ate(regr_table, rand_ass()))
hist(all_ate)
# two-tailed p-val, is this correct?
mean(ate < all_ate & -ate > -all_ate)

# EXCLUDING $0.10 posting
# treatment = rand_ass()
ate_exclude_0.10 = est.ri.ate(regr_table_exclude0.10,regr_table_exclude0.10$treatment)
all_ate_exclude_0.10 <- replicate(5000, est.ri.ate(regr_table_exclude0.10, rand_ass()))
hist(all_ate_exclude_0.10)
# two-tailed p-val, is this correct?
mean(ate_exclude_0.10 < all_ate_exclude_0.10 & -ate_exclude_0.10 > -all_ate_exclude_0.10)

#---------------------------------------------------------------------#
# EDA & DISTRIBUTION OF ACCURACY

# pooling 0.10 & 0.25 treatments
hist(regr_table$accuracy)

hist(regr_table[treatment == 0.55,]$accuracy)
hist(regr_table[treatment == 0.10,]$accuracy)
hist(regr_table[treatment == 0.25,]$accuracy)
hist(regr_table[treatment == 0.40,]$accuracy)

# Notice that both histograms are very left skewed
# This calls the appropriateness of OLS asymptotics for ATE SE & p-val estimation into question
# Hopefully with a larger sample size, asymtotics will be more reliable
# Anyhow, this says that we should report p-val using randomization inference in addition to regression
# Just like many of the papers we read in class

#---------------------------------------------------------------------#
# HIGHER ORDER TERMS

# SECOND ORDER TERM -- may be best fitting our data for order 1, but order 2 may given different insights
regr.sqord = lm(accuracy ~ treatment + I(treatment^2) + CQ1 + CQ2_3 + CQ3, data = regr_table)
summary(regr.sqord)
lmtest::coeftest(regr, vcov(regr.sqord)) #the coefficient for "treatment" here is wrong", but SE is correct
# dev.off() --> if the following line gives an error
plot(allEffects(regr.sqord, default.levels=50))

#---------------------------------------------------------------------#