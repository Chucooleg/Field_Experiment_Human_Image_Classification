# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")

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
# read in qualtric output csv
qualtric_data_path_0.40 = "../qualtric_data/20171112_qualtric_results_order1_0.40.csv"


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
# ORDER 1, PAYMENT RATE = 0.25

# Please fill in (reuse above code block)
# read in qualtric output csv
qualtric_data_path_0.25 = "../qualtric_data/20171112_qualtric_results_order1_0.25.csv"
current_task_data_0.25 = get_current_task_data(qualtric_data_path_0.25)

# !!! REMOVE REPEATERS : turks who checked out the 0.40 task already
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
# POOLING TWO CSV FILES FROM DIFFERENT TREATMENTS

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
regr_table$CQ5 = regr_table$CQ5 == "Yes"
regr_table$CQ4 = as.factor(regr_table$CQ4)
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
est.t.test = function(d1, d2, alt){
  stacked_data = stack(list(d1=d1,d2=d2))
  ltest = car::leveneTest(values~ind,data = stacked_data, center = median)
  levene_p_val = ltest[1,3]
  t.test(d1, d2, alternative = alt, var.equal = levene_p_val >0.05)
}

# Remember to discuss confidence intervals, David likes those
est.t.test(worker_perf_0.10$accuracy,worker_perf_0.55$accuracy, "two.sided")
est.t.test(worker_perf_0.10$accuracy,worker_perf_0.25$accuracy, "two.sided")
est.t.test(worker_perf_0.10$accuracy,worker_perf_0.40$accuracy, "two.sided")
est.t.test(worker_perf_0.25$accuracy,worker_perf_0.40$accuracy, "two.sided")
est.t.test(worker_perf_0.25$accuracy,worker_perf_0.55$accuracy, "two.sided")
est.t.test(worker_perf_0.40$accuracy,worker_perf_0.55$accuracy, "two.sided")

# REGRESSION
est.regr.simple = function(r_table){
  regr = lm(accuracy ~ treatment, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}

est.regr.covars = function(r_table){
  regr = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}

est.regr.full = function(r_table){
  regr = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3 + CQ4 + CQ5, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}

est.regr.simple(regr_table)
est.regr.covars(regr_table)
est.regr.full(regr_table)

# test if variance of the two groups are unequal
# car::leveneTest(worker_perf_0.10$accuracy,worker_perf_0.55$accuracy,center=median)
# 
# sd = stack(list(d1=worker_perf_0.10$accuracy, d2=worker_perf_0.55$accuracy))
# ltest = car::leveneTest(values~ind,data = sd, center = median)
# # 2 sample independent t-test
# t.test(worker_perf_0.10$accuracy,
#        worker_perf_0.55$accuracy,
#        alternative = "two.sided", var.equal = TRUE)
# 
# regr1 = lm(accuracy ~ treatment, data = regr_table)
# regr2 = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3, data = regr_table)
# # don't know if it counts as fishing expedition, but adding screener to this also produces a statistically significant result. the screener variable is also highly stat significant. 
# regr3 = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3 + screener, data = regr_table)
# summary(regr1)
# summary(regr2)
# summary(regr3)
# lmtest::coeftest(regr1, vcov(regr1))
# lmtest::coeftest(regr2, vcov(regr2))
# lmtest::coeftest(regr3, vcov(regr3))

#---------------------------------------------------------------------#
# RANDOMIZATION INFERENCE

n10 = nrow(worker_perf_0.10)
n25 = nrow(worker_perf_0.25)
n40 = nrow(worker_perf_0.40)
n55 = nrow(worker_perf_0.55)

rand_ass = function() sample(c(rep(0.1,n10),rep(0.25,n25), rep(0.4,n40), rep(0.55,n55)))

est.ri.ate = function(d, treatment){
  d$treatment_new = treatment
  m1 = lm(accuracy ~ treatment_new, data =d)
  ate = lmtest::coeftest(m1, vcov(m1))[2]
  return(ate)
} 

# treatment = rand_ass()
ate = est.ri.ate(regr_table,regr_table$treatment)
all_ate <- replicate(5000, est.ri.ate(regr_table, rand_ass()))

# one-tailed p-val, is this correct?
mean(ate<all_ate)

# two-tailed p-val, is this correct?
mean(ate < all_ate & -ate > -all_ate)

# CACE -- compliers average causal effect - with randomization inference

# !!!PLEASE FILL IN

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
treatment_sq <- regr_table$treatment^2
treatment_cubed <- regr_table$treatment^3

est.regr.highord = function(r_table){
  regr = lm(accuracy ~ treatment + treatment_sq + treatment_cubed + CQ1 + CQ2_3 + CQ3 + CQ4 + CQ5, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}

est.regr.highord(regr_table)
#---------------------------------------------------------------------#
# REGRESSION TABLES

# !!!PLEASE FILL IN

#---------------------------------------------------------------------#