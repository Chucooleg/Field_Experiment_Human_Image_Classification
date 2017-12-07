# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/241_Final/Field_Experiment_Human_Image_Classification/code")
source(file = "design2_data_transformation_functions.r")
source(file = "design2_data_analysis_functions.r")

#---------------------------------------------------------------------#
# check attrition

attrition_by_group = lm(observed~group,data=by_HIT.table)
summary(attrition_by_group) #nothing significant

# Use interactions!
attrition_by_group_CQ = lm(observed~group*CQ1 + group*CQ2 + group*CQ3 + group*CQ4 + group*CQ5,data=by_HIT.table)
summary(attrition_by_group_CQ) #violations mostly related to CQ1

#---------------------------------------------------------------------#
# check covariate balance (among non-attriters)

# CQ1 "What portion of your friends own pets?"
table(by_HIT.table$CQ1)
cov_check_CQ1.1 = lm(CQ1=="a lot less than half"~group, data=by_HIT.table)
cov_check_CQ1.2 = lm(CQ1=="around half"~group, data=by_HIT.table)
cov_check_CQ1.3 =lm(CQ1=="a lot more than half"~group, data=by_HIT.table)
summary(cov_check_CQ1.1) #nothing significant
summary(cov_check_CQ1.2) #nothing significant
summary(cov_check_CQ1.3) #groupTTT coef has marginally significant p-val 0.0267, meaning Turkers in TTT group are more likely to answer "around half" or "a lot less than half"


# CQ2 "Please rank your preferences to work with the following media:"
# Some numeric rank of "Images"
hist(by_HIT.table$CQ2, breaks = 0:4)
cov_check_CQ2 = lm(CQ2~group, data=by_HIT.table)
summary(cov_check_CQ2) #nothing significant

# CQ3 "Have your ever lived with any dogs in your household? If not, have you ever planned to own a dog?"
cov_check_CQ3.1 = lm(CQ3=="Yes"~group, data=by_HIT.table)
cov_check_CQ3.2 = lm(CQ3=="No"~group, data=by_HIT.table)
cov_check_CQ3.3 = lm(CQ3=="Maybe"~group, data=by_HIT.table)
summary(cov_check_CQ3.1) #nothing significant
summary(cov_check_CQ3.2) #nothing significant
summary(cov_check_CQ3.3) #nothing significant

# CQ4 "On average, how many tasks on Amazon Mechanical Turk do you complete every week?"
# "21 to 30"   "41 or more" "31 to 40"   "11 to 20"   "0 to 10"  
cov_check_CQ4.1 = lm(CQ4=="0 to 10"~group, data=by_HIT.table)
cov_check_CQ4.2 = lm(CQ4=="11 to 20"~group, data=by_HIT.table)
cov_check_CQ4.3 = lm(CQ4=="21 to 30"~group, data=by_HIT.table)
cov_check_CQ4.4 = lm(CQ4=="31 to 40"~group, data=by_HIT.table)
cov_check_CQ4.5 = lm(CQ4=="41 or more"~group, data=by_HIT.table)
summary(cov_check_CQ4.1) #nothing significant
summary(cov_check_CQ4.2) #nothing significant
summary(cov_check_CQ4.3) #nothing significant
summary(cov_check_CQ4.4) #nothing significant
summary(cov_check_CQ4.5) #nothing significant

# CQ5  "Do you use Linkedin?"
# "Yes" "No" "Never heard of Linkedin"
cov_check_CQ5.1 = lm(CQ5=="Yes"~group, data=by_HIT.table)
cov_check_CQ5.2 = lm(CQ5=="No"~group, data=by_HIT.table)
cov_check_CQ5.3 = lm(CQ5=="Never heard of Linkedin"~group, data=by_HIT.table)
summary(cov_check_CQ5.1) #nothing significant
summary(cov_check_CQ5.2) #nothing significant
summary(cov_check_CQ5.3) #nothing significant


#---------------------------------------------------------------------#
# Brief EDA
# check output var distribution
hist(by_HIT.table$overall_accuracy)
hist(by_Session.table$round_accuracy)
# Notice that both histograms are very left skewed
# This calls the appropriateness of OLS asymptotics for ATE SE & p-val estimation into question
# Hopefully with a larger sample size, asymtotics will be more reliable
# Anyhow, this says that we should report p-val using randomization inference in addition to regression
# Just like many of the papers we read in class

#---------------------------------------------------------------------#
# Model Overall -- Comparing only by group 
# (looking at the entire HIT of 48 questions as a whole)

# Without covariates
# Any effect on Accuracy?
mod.design2.by_HIT = lm(overall_accuracy ~ group, data = by_HIT.table)
lmtest::coeftest(mod.design2.by_HIT, vcov=vcovHC(mod.design2.by_HIT))
# Any effect on Timespent?
mod.design2.by_HIT.total_timespent = lm(total_timespent ~ group, data = by_HIT.table)
lmtest::coeftest(mod.design2.by_HIT.total_timespent, vcov=vcovHC(mod.design2.by_HIT.total_timespent))
# Any effect on Whether screeners are passed?
mod.design2.by_HIT.all_screeners_passed = lm(all_screeners_passed ~ group, data = by_HIT.table)
lmtest::coeftest(mod.design2.by_HIT.all_screeners_passed, vcov=vcovHC(mod.design2.by_HIT.all_screeners_passed))


# With covariates
# Any effect on Accuracy?
mod.design2.by_HIT.cov = lm(overall_accuracy ~ group + CQ1 + CQ2 + CQ3 + CQ4 + CQ5, data = by_HIT.table)
lmtest::coeftest(mod.design2.by_HIT.cov, vcov=vcovHC(mod.design2.by_HIT.cov))
# Any effect on Timespent?
mod.design2.by_HIT.cov.total_timespent = lm(total_timespent ~ group + CQ1 + CQ2 + CQ3 + CQ4 + CQ5, data = by_HIT.table)
lmtest::coeftest(mod.design2.by_HIT.cov.total_timespent, vcov=vcovHC(mod.design2.by_HIT.cov.total_timespent))
# Any effect on Whether screeners are passed?
mod.design2.by_HIT.cov.all_screeners_passed = lm(all_screeners_passed ~ group + CQ1 + CQ2 + CQ3 + CQ4 + CQ5, data = by_HIT.table)
lmtest::coeftest(mod.design2.by_HIT.cov.all_screeners_passed, vcov=vcovHC(mod.design2.by_HIT.cov.all_screeners_passed))

#---------------------------------------------------------------------#
# Model Stepped-Wedge -- Comparing by group and round
# (looking at task 3 rounds of 16 questions)

# WITH COVARIATES
mod.design2.by_Session = lm(round_accuracy ~ group * round,data=by_Session.table)
coeftest(mod.design2.by_Session, vcov=vcovHC(mod.design2.by_Session))


# WITH COVARIATES (TO BE CONTINUED)
mod.design2.by_Session.cov = lm(round_accuracy ~ group * round + CQ1 + CQ2 + CQ3 + CQ4 + CQ5,data=by_Session.table)
coeftest(mod.design2.by_Session.cov, vcov=vcovHC(mod.design2.by_Session.cov))

"
Round       R1 R2 R3

Group CCC   C   C  C
Group CCT   C   C  T
Group CTT   C   T  T 
Group TTT   T   T  T
"

"
Reduced Model

t test of coefficients:

Estimate Std. Error t value Pr(>|t|)    
b_1  (Intercept)          0.4832592  0.0282108 17.1303   <2e-16 ***
b_2  groupCCT             0.0132190  0.0383125  0.3450   0.7302    
b_3  groupCTT            -0.0369008  0.0401086 -0.9200   0.3579    
b_4  groupTTT            -0.0073552  0.0391139 -0.1880   0.8509    
b_5  roundtwo            -0.0173578  0.0369009 -0.4704   0.6382    
b_6  roundthree           0.0144648  0.0355561  0.4068   0.6843    
b_7  groupCCT:roundtwo   -0.0049546  0.0545827 -0.0908   0.9277    
b_8  groupCTT:roundtwo    0.0163774  0.0536613  0.3052   0.7603    
b_9  groupTTT:roundtwo    0.0072475  0.0536400  0.1351   0.8926    
b_10 groupCCT:roundthree  0.0037908  0.0534056  0.0710   0.9434    
b_11 groupCTT:roundthree -0.0046609  0.0530039 -0.0879   0.9300    
b_12 groupTTT:roundthree  0.0094323  0.0524767  0.1797   0.8574 
---------------------------------------------------------------------
"

"
Full Model

Estimate Std. Error t value  Pr(>|t|)    
b_1  (Intercept)              0.4832592  0.0905885  5.3347 1.291e-07 ***
b_2  groupCCT                 0.0132190  0.0358540  0.3687  0.712470    
b_3  groupCTT                -0.0369008  0.0371396 -0.9936  0.320773    
b_4  groupTTT                -0.0073552  0.0340609 -0.2159  0.829096    
b_5  roundtwo                -0.0173578  0.0321583 -0.5398  0.589533    
b_6  roundthree               0.0144648  0.0318498  0.4542  0.649855    
b_7  CQ1around half           0.0809586  0.0289370  2.7978  0.005286 ** 
b_8  CQ1a lot more than half  0.1153769  0.0287837  4.0084 6.763e-05 ***
b_9  CQ2                      0.0237025  0.0097418  2.4331  0.015218 *  
b_10 CQ3Maybe                -0.1525802  0.0541354 -2.8185  0.004960 ** 
b_11 CQ3Yes                   0.1013461  0.0318965  3.1773  0.001551 ** 
b_12 CQ411 to 20             -0.0647353  0.0354255 -1.8274  0.068067 .  
b_13 CQ421 to 30             -0.0304403  0.0302873 -1.0051  0.315217    
b_14 CQ431 to 40              0.0075191  0.0329807  0.2280  0.819723    
b_15 CQ441 or more           -0.0315901  0.0271677 -1.1628  0.245310    
b_16 CQ5No                    0.1814065  0.0797526  2.2746  0.023228 *  
b_17 CQ5Yes                   0.1143499  0.0805132  1.4203  0.155973    
b_18 groupCCT:roundtwo       -0.0049546  0.0496381 -0.0998  0.920520    
b_19 groupCTT:roundtwo        0.0163774  0.0500366  0.3273  0.743532    
b_20 groupTTT:roundtwo        0.0072475  0.0466586  0.1553  0.876606    
b_21 groupCCT:roundthree      0.0037908  0.0496820  0.0763  0.939201    
b_22 groupCTT:roundthree     -0.0046609  0.0501303 -0.0930  0.925949    
b_23 groupTTT:roundthree      0.0094323  0.0455974  0.2069  0.836179  
"
#---------------------------------------------------------------------

# ALL the comparisons we want to make with the Stepped-Wedge Design

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               # effect of being in round two rather than round one in group CCC
               "CCC R2-R1"=c(0,0,0,0,1,0,0,0,0,0, 0, 0),
               # effect of being in round three rather than round one in group CCC
               "CCC R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 0, 0),
               # effect of being in round three rather than round two in group CCC
               "CCC R3-R2"=c(0,0,0,0,-1,1,0,0,0,0, 0, 0),
               
               # effect of being in round two rather than round one in group CCT
               "CCT R2-R1"=c(0,0,0,0,1,0,1,0,0,0, 0, 0),
               # effect of being in round three rather than round one in group CCT
               "CCT R3-R1"=c(0,0,0,0,0,1,0,0,0,1, 0, 0),
               # effect of being in round three rather than round two in group CCT
               "CCT R3-R2"=c(0,0,0,0,-1,1,-1,0,0,1, 0, 0),
               
               # effect of being in round two rather than round one in group CTT
               "CTT R2-R1"=c(0,0,0,0,1,0,0,1,0,0, 0, 0),
               # effect of being in round three rather than round one in group CTT
               "CTT R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 1, 0),
               # effect of being in round three rather than round two in group CTT
               "CTT R3-R2"=c(0,0,0,0,-1,1,0,-1,0,0, 1, 0),
               
               # effect of being in round two rather than round one in group TTT
               "TTT R2-R1"=c(0,0,0,0,1,0,0,0,1,0, 0, 0),
               # effect of being in round three rather than round one in group TTT
               "TTT R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 0, 1),
               # effect of being in round three rather than round two in group TTT
               "TTT R3-R2"=c(0,0,0,0,-1,1,0,0,-1,0, 0, 1),
               
               # Effect of receiving treatment in Round one 
               "T-C R1"=c(0,(-1/3),(-1/3),1,0,0,0,0,0,0, 0, 0),
               # Effect of receiving treatment in Round two (regardless of round one status)
               "T-C R2"=c(0,(-1/2),(1/2),(1/2),0,0,(-1/2),(1/2),(1/2),0, 0, 0),
               # Effect of receiving treatment in Round three (regardless of round one or two status)
               "T-C R3"=c(0,(-1/3),(-1/3),1,0,0,0,0,0,(-1/3), (-1/3), 1),
               
               # Effect of being in a group TTT that receive all three rounds 
               # of treatment rather than all control group CCC
               "TTT-CCC"=c(0,0,0,1,0,0,0,0,1/3,0, 0, 1/3),
               # Effect of being in any treatment groups rather than all control group CCC 
               "(T)-CCC"=c(0,1/3,1/3,1/3,0,0,1/9,1/9,1/9,1/9, 1/9, 1/9),
               
               # Effect on Round 3, of Receiving Treatment in Round 3 but not earlier : 
               # Difference-Difference Estimator
               "D_in_D R3"=c(0,0,0,0,0,0,-1,0,0,1, 0, 0),
               # Effect on Round 2, of Receiving Treatment in Round 2 but not earlier : 
               # Difference-Difference Estimator
               "D_in_D R2"=c(0,0,0,0,0,0,-1/2,1,0,0, 0, 0)
             )))


#---------------------------------------------------------------------

# FE code pp277

# 243 full rows
table(by_HIT.table[by_HIT.table$overall_accuracy >=0]$group)


# Given 243 subjects, with the following group distribution. 
# possible random allocations
choose(243,64) * choose(179,60) * choose(119,58)



# Important substantive assumptions that allow us to get an overall estimate of bonuses assigned to any session.Invoking the no anticipation assumption allow us to ignore treatments that are allocated after the current period. We also ignore treatments that were allocated two sessions prior, which is equivalent to assuming that bonuses' effects wask out entirely after two sessions/.

# We first redefine the potential outcomes as follow:
# Y_00 (untreated during preceding and current session)
# Y_01 (untreated during preceding session but treated during current session)
# Y_11 (treated during preceding and current session)

# And the truncated tables below shows the randomly assigned treatment condition and observed outcomes for 20 of the subjects.

head(assigned_treatment_condition_table,20)
head(observed_outcomes_table, 20)

# ATE estimate
# Note that we cannot naively compare average outcomes of treated sessions to the average outcomes of the untreated sessions. This approach is biased because it ignores the fact that the probability of assignment to treatment varies from session to session because individuals are much more likely to be assignedf to bonuses in the final week than in the first week. It is also prone to biase because it ignores lagged effects, treating Y_11 and Y_01 as though they were identical.
# Instead we begin by calculating the probability of begin assigned to each treatment during each session. These probabilities are displayed below.

design2.prob.table = data.table(Treatment_Condition = c("Pr(00)","Pr(01)","Pr(11)"),
                                Week_1 = c(0.75, 0.25, 0),
                                Week_2 = c(0.50, 0.25, 0.25),
                                Week_3 = c(0.25, 0.25, 0.50))
design2.prob.table

# Then, in order to obtain unbiased treatment effects, we apply inverse weights. Treated units are weighted by the inverse of the probability of being treated; control units are weighted by the inverse of the probability of being in control. According to the above probability table, calculation of the immediate effect is as follows:


# SPECIFY FORMULAsss HERE 
$$\hat{E}[Y_{01} - Y_{00}] = \frac{ \frac{\sum_{S1} Y_{01}}{0.25} + \frac{\sum_{S2} Y_{01}}{0.25} + \frac{\sum_{S3} Y_{01}}{0.25} }{\frac{64}{0.25}+\frac{60}{0.25} +\frac{60}{0.25}} - \frac{ \frac{\sum_{S1} Y_{00}}{0.75} + \frac{\sum_{S2} Y_{00}}{0.50} + \frac{\sum_{S3} Y_{00}}{0.25} }{\frac{179}{0.75}+\frac{119}{0.50} +\frac{61}{0.25}}$$

  
# sum S1 Y_01 
Y01_S1 = by_Session.table[round=="one" & group=="TTT",round_accuracy]
# sum S2 Y_01
Y01_S2 = by_Session.table[round=="two" & group=="CTT",round_accuracy]
# sum S3 Y_01
Y01_S3 = by_Session.table[round=="three" & group=="CTT",round_accuracy]

# sum S1 Y_00
Y00_S1 = by_Session.table[round=="one" & group!="TTT",round_accuracy]
# sum S2 Y_00
Y00_S2 = by_Session.table[round=="two" & (group=="CCT" | group=="CCC"),round_accuracy]
# sum S3 Y_00
Y00_S3 = by_Session.table[round=="three" & group=="CCC",round_accuracy]


E_Y01 = (sum(Y01_S1)/0.25 + sum(Y01_S2)/0.25 + sum(Y01_S3)/0.25) / 
  (length(Y01_S1)/0.25 + length(Y01_S2)/0.25  + length(Y01_S3)/0.25 )

E_Y00.1 = (sum(Y00_S1)/0.75 + sum(Y00_S2)/0.50+ sum(Y00_S3)/0.25) /
  (length(Y00_S1)/0.75 + length(Y00_S2)/0.50+ length(Y00_S3)/0.25)

E_Y01_Y00 = E_Y01 - E_Y00.1 

# In order to estimate effects for combined immediate and lag effect, we restrict our attention to the second and third weeks, because this type of treatment cannot occur in the first week.

# SPECIFY FORMULAsss HERE 
$$\hat{E}[Y_{11} - Y_{00}] = \frac{ \frac{\sum_{S2} Y_{11}}{0.25} + \frac{\sum_{S3} Y_{11}}{0.50} }{\frac{64}{0.25}+\frac{124}{0.50}} - \frac{ \frac{\sum_{S2} Y_{00}}{0.50} + \frac{\sum_{S3} Y_{00}}{0.25} + }{\frac{119}{0.50}+\frac{61}{0.25}}$$

# sum S1 Y_11
# non-existent
# sum S2 Y_11
Y11_S2 = by_Session.table[round=="two" & group=="TTT",round_accuracy]
# sum S3 Y_11
Y11_S3 = by_Session.table[round=="three" & (group=="TTT" | group=="CTT"),round_accuracy]

E_Y11 = (sum(Y11_S2)/0.25 + sum(Y11_S3)/0.50) /
  (length(Y11_S2)/0.25 + length(Y11_S3)/0.50)
  
E_Y00.2 = (sum(Y00_S2)/0.50 + sum(Y00_S3)/0.25) /
  (length(Y00_S2)/0.50 + length(Y00_S3)/0.25)

E_Y11_Y00 = E_Y11 - E_Y00.2


# Get standard errors
# Below, we generated a hypothetical schedule of outcomes for this experiment. 

# outcome table as is
head(Y00.table,20)
head(Y01.table,20)
head(Y11.table,20)

# Then, we fill in the implied schedule of potential outcomes under the assumption of constant treatment effects.
head(Y00.table.filled,20)
head(Y01.table.filled,20)
head(Y11.table.filled,20)



# WORK IN PROGRESS
# Sampling Distribution to generate CI

ATEs_0100.hat = c()
ATEs_1100.hat = c()
treatments = c(rep("CCC",61), rep("CCT",58), rep("CTT",60), rep("TTT",64))


# write function to wrap below
for (i in 1:1000) {
  treat = sample(x=treatments,size=length(treatments),replace = F)
  # create simulated schedule
  sim.outcomes = create_simulated_schedule(sim.outcomes.template, treat) 
  # estimate ATE from simulated schedule
  estimates = est.ATE_from_simoutcomes(sim.outcomes)
  ATEs_1100.hat = c(ATEs_1100.hat, estimates[[2]])
  ATEs_0100.hat = c(ATEs_0100.hat, estimates[[1]])
}

# list of simulated ATE estimates
ATEs_0100.hat
ATEs_1100.hat 
# histogram of simulated ATE estimates
hist(ATEs_0100.hat, breaks = 20)
hist(ATEs_1100.hat, breaks = 20)
# CIs of simulated ATE estimates
quantile(ATEs_0100.hat, c(0.025, 0.975))
quantile(ATEs_1100.hat, c(0.025, 0.975))
#---------------------------------------------------------------------
# NOTES below for figuring out general linear hypothesis
"
Group CCC Round 1 Accuracy:

b_1*1 + b_2*0 + b_3*0 + b_4*0 +
b_5*0 + b_6*0 +
b_7*0 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1 
"

"
Group CCC Round 2 Accuracy:

b_1*1 + b_2*0 + b_3*0 + b_4*0 +
b_5*1 + b_6*0 +
b_7*0 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1 + b_5 
"

"
Group CCC Round 3 Accuracy:

b_0*1 + b_2*0 + b_3*0 + b_4*0 +
b_5*0 + b_6*1 +
b_7*0 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1 + b_6 
"

"
b_5 : effect of being in round two rather than round one in group CCC
b_6 : effect of being in round three rather than round one in group CCC
b_6 - b_5 : effect of being in round three rather than round two in group CCC
"

# Model without covariates
summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "CCC R2-R1"=c(0,0,0,0,1,0,0,0,0,0, 0, 0),
               "CCC R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 0, 0),
               "CCC R3-R2"=c(0,0,0,0,-1,1,0,0,0,0, 0, 0)
             )))

# Model with covariates
summary(glht(mod.design2.by_Session.cov , vcov=vcovHC(mod.design2.by_Session.cov),
             linfct=rbind(
               "CCC R2-R1"=c(0,0,0,0,1,0,     0,0,0, 0, 0, 0, 0, 0, 0, 0, 0,      0, 0, 0, 0, 0, 0),
               "CCC R3-R1"=c(0,0,0,0,0,1,     0,0,0, 0, 0, 0, 0, 0, 0, 0, 0,      0, 0, 0, 0, 0, 0),
               "CCC R3-R2"=c(0,0,0,0,-1,1,    0,0,0, 0, 0, 0, 0, 0, 0, 0, 0,      0,0,0,0, 0, 0)
             )))
#---------------------------------------------------------------------

"
Group CCT Round 1 Accuracy:

b_1*1 + b_2*1 + b_3*0 + b_4*0 +
b_5*0 + b_6*0 +
b_7*0 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_2
"

"
Group CCT Round 2 Accuracy:

b_1*1 + b_2*1 + b_3*0 + b_4*0 +
b_5*1 + b_6*0 +
b_7*1 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_2 + b_5 + b_7
"

"
Group CCT Round 3 Accuracy:

b_1*1 + b_2*1 + b_3*0 + b_4*0 +
b_5*0 + b_6*1 +
b_7*0 + b_8*0 + b_9*0 + b_10*1 + b_11*0 + b_12*0

= b_1 + b_2 + b_6 + b_10
"

"
b_5 + b_7 : effect of being in round two rather than round one in group CCT
b_6 + b_10 : effect of being in round three rather than round one in group CCT 
b_6 - b_5 + b_10 - b_7 : effect of being in round three rather than round two in group CCT
"

# Model without covariates
summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "CCT R2-R1"=c(0,0,0,0,1,0,1,0,0,0, 0, 0),
               "CCT R3-R1"=c(0,0,0,0,0,1,0,0,0,1, 0, 0),
               "CCT R3-R2"=c(0,0,0,0,-1,1,-1,0,0,1, 0, 0)
             )))


# Model with covariates
summary(glht(mod.design2.by_Session.cov , vcov=vcovHC(mod.design2.by_Session.cov),
             linfct=rbind(
               "CCT R2-R1"=c(0,0,0,0,1,0,     0,0,0, 0, 0, 0, 0, 0, 0, 0, 0,     1,0,0,0, 0, 0),
               "CCT R3-R1"=c(0,0,0,0,0,1,     0,0,0, 0, 0, 0, 0, 0, 0, 0, 0,     0,0,0,1, 0, 0),
               "CCT R3-R2"=c(0,0,0,0,-1,1,    0,0,0, 0, 0, 0, 0, 0, 0, 0, 0,    -1,0,0,1, 0, 0)
             )))
#---------------------------------------------------------------------

"
Group CTT Round 1 Accuracy:

b_1*1 + b_2*0 + b_3*1 + b_4*0 +
b_5*0 + b_6*0 +
b_7*0 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_3
"

"
Group CTT Round 2 Accuracy:

b_1*1 + b_2*0 + b_3*1 + b_4*0 +
b_5*1 + b_6*0 +
b_7*0 + b_8*1 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_3 + b_5 + b_8
"

"
Group CTT Round 3 Accuracy:

b_1*1 + b_2*0 + b_3*1 + b_4*0 +
b_5*0 + b_6*1 +
b_7*0 + b_8*1 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_3 + b_6 + b_11
"

"
b_5 + b_8 : effect of being in round two rather than round one in group CTT
b_6 + b_11 : effect of being in round three rather than round one in group CTT 
b_6 - b_5 + b_11 - b_8 : effect of being in round three rather than round two in group CTT
"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "CTT R2-R1"=c(0,0,0,0,1,0,0,1,0,0, 0, 0),
               "CTT R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 1, 0),
               "CTT R3-R2"=c(0,0,0,0,-1,1,0,-1,0,0, 1, 0)
             )))

#---------------------------------------------------------------------

"
Group TTT Round 1 Accuracy:

b_1*1 + b_2*0 + b_3*0 + b_4*1 +
b_5*0 + b_6*0 +
b_7*0 + b_8*0 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_4
"

"
Group TTT Round 2 Accuracy:

b_1*1 + b_2*0 + b_3*1 + b_4*0 +
b_5*1 + b_6*0 +
b_7*0 + b_8*1 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_4 + b_5 + b_9
"

"
Group TTT Round 3 Accuracy:

b_1*1 + b_2*0 + b_3*1 + b_4*0 +
b_5*0 + b_6*1 +
b_7*0 + b_8*1 + b_9*0 + b_10*0 + b_11*0 + b_12*0

= b_1  + b_4 + b_6 + b_12
"

"
b_5 + b_9 : effect of being in round two rather than round one in group TTT
b_6 + b_12 : effect of being in round three rather than round one in group TTT 
b_6 - b_5 + b_12 - b_9 : effect of being in round three rather than round two in group TTT
"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "TTT R2-R1"=c(0,0,0,0,1,0,0,0,1,0, 0, 0),
               "TTT R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 0, 1),
               "TTT R3-R2"=c(0,0,0,0,-1,1,0,0,-1,0, 0, 1)
             )))

#---------------------------------------------------------------------
"
Effect of receiving treatment in Round one
(Effect of receiving bonus in round 1)

Round       R1 

Group CCC   C 
Group CCT   C 
Group CTT   C 
--------------
Group TTT   T 

= Group TTT Round 1 Accuracy - avg(Group CTT Round 1 Accuracy,  Group CCT Round 1 Accuracy, Group CCC Round 1 Accuracy)

= b_1 + b_4 - (b_1 + b_3 + b_1 + b_2 + b_1)/3
= b_4 - (b_3 + b_2)/3

= (-1/3)*b_2 + (-1/3)*b_3 + b_4

= (-1/3)*(-0.001014) + (-1/3)*(-0.043137) -0.028493
= -0.013776
"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "T-C R1"=c(0,(-1/3),(-1/3),1,0,0,0,0,0,0, 0, 0)
             )))

#---------------------------------------------------------------------

"
Effect of receiving treatment in Round two (regardless of round one status)
(Effect of receiving bonus in round 2, regardless of prior bonus status)

Round       R2 

Group CCC   C 
Group CCT   C 
--------------
Group CTT   T 
Group TTT   T 

=  avg(Group CTT Round 2 Accuracy,  Group TTT Round 2 Accuracy) - avg(Group CCC Round 2 Accuracy,  Group CCT Round 2 Accuracy)


= (b_1  + b_3 + b_5 + b_8 + b_1 + b_4 + b_5 + b_9)/2 - (b_1 + b_5 + b_1 + b_2 + b_5 + b_7)/2

= (b_1 + b_5) + (b_3 + b_4 + b_8 + b_9)/2 - (b_1 + b_5) - (b_2 + b_7)/2

= (-b_2 + b_3 + b_4 - b_7 + b_8 + b_9)/2

= (-1/2)*b_2 + (1/2)*b_3 + (1/2)*b_4 + (-1/2)*b_7 + (1/2)*b_8 + (1/2)*b_9

= (-1/2)*(-0.001014) + (1/2)*(-0.043137) + (1/2)*(-0.028493) + (-1/2)*(-0.004955) + (1/2)*(0.016377) + (1/2)*(0.007247)
= -0.0210185

"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "T-C R2"=c(0,(-1/2),(1/2),(1/2),0,0,(-1/2),(1/2),(1/2),0, 0, 0)
             )))

#---------------------------------------------------------------------

"
Effect of receiving treatment in Round three (regardless of round one or two status)
(Effect of receiving bonus in round 3, regardless of prior bonus status)

Round       R3 

Group CCC   C 
--------------
Group CCT   T 
Group CTT   T 
Group TTT   T 

=  avg(Group TTT Round 2 Accuracy) - avg(Group CCC Round 2 Accuracy,  Group CCT Round 2 Accuracy, Group CTT Round 2 Accuracy)


= (b_1 + b_4 + b_6 + b_12) - (b_1 + b_6 + b_1 + b_2 + b_6 + b_10 + b_1 + b_3 + b_6 + b_11)/3

= (b_1 + b_4 + b_6 + b_12) - (b_1 + b_6) - (b_2 + b_3 + b_10 + b_11)/3

= (b_4 + b_12) - (b_2 + b_3 + b_10 + b_11)/3

= (-1/3)*b_2 + (-1/3)*b_3 + b_4 + (-1/3)*b_10 + (-1/3)*b_11 + b_12

= (-1/3)*(-0.001014) + (-1/3)*(-0.043137) + (-0.028493) + (-1/3)*(0.003791) + (-1/3)*(-0.004661) + 0.009432
= -0.004054

"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "T-C R3"=c(0,(-1/3),(-1/3),1,0,0,0,0,0,(-1/3), (-1/3), 1)
             )))

#---------------------------------------------------------------------

"
Effect of being in a group TTT that receive all three rounds of treatment rather than all control group CCC
(Effect of receiving maximum bonus rather than no bonuses at all)

Round       R1 R2 R3

Group CCC   C   C  C
----------------------
Group TTT   T   T  T


= Average accuracy of group TTT - Average accuracy of group CCC

= [(b_1  + b_4) + (b_1  + b_4 + b_5 + b_9) + (b_1  + b_4 + b_6 + b_12)]/3 
- [(b_1) + (b_1 + b_5) + (b_1 + b_6)]/3

= b_1 + b_4 + (b_5 + b_6 + b_9 + b_12)/3 
- b_1 - (b_5 + b_6)/3

= b_4 + (b_9 + b_12)/3

= -0.028493 + (0.007247 + 0.009432)/3
= -0.02293333

coincide with overall results using by_HIT.table
lm(overall_accuracy ~ group, data = by_HIT.table)
# look for coeff for groupTTT
"

tmp.mod = lm(overall_accuracy ~ group, data = by_HIT.table)
lmtest::coeftest()

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "TTT-CCC"=c(0,0,0,1,0,0,0,0,1/3,0, 0, 1/3)
             )))

#---------------------------------------------------------------------

"
Effect of being in any treatment groups rather than all control group CCC 
(Effect of receiving any bonus rather than no bonuses at all)

Round       R1 R2 R3

Group CCC   C   C  C
--------------------
Group CCT   C   C  T
Group CTT   C   T  T 
Group TTT   T   T  T

= Average accuracy of group (TTT, CTT, CCT) - Average accuracy of group CCC

= [(b_1  + b_4) + (b_1  + b_4 + b_5 + b_9) + (b_1  + b_4 + b_6 + b_12)]/3/3 
+ [(b_1  + b_3) + (b_1  + b_3 + b_5 + b_8) + (b_1  + b_3 + b_6 + b_11)]/3/3 
+ [(b_1  + b_2) + (b_1  + b_2 + b_5 + b_7) + (b_1  + b_3 + b_6 + b_10)]/3/3
- [(b_1) + (b_1 + b_5) + (b_1 + b_6)]/3

= (3*b_1 + 3*b_4 + b_5 + b_6 + b_9 + b_12)/9
+ (3*b_1 + 3*b_3 + b_5 + b_6 + b_8 + b_11)/9
+ (3*b_1 + 3*b_2 + b_5 + b_6 + b_7 + b_10)/9
- b_1 - (b_5 + b_6)/3


= b_1 + (b_2 + b_3 + b_4 + b_5 + b_6)/3 + (b_7 + b_8 + b_9 + b_10 + b_11 + b_12)/9
- b_1 - (b_5 + b_6)/3

= (b_2 + b_3 + b_4)/3 + (b_7 + b_8 + b_9 + b_10 + b_11 + b_12)/9

= 1/3*b_2 + 1/3*b_3 + 1/3*b_4 + 1/9*b_7 + 1/9*b_8 + 1/9*b_9 + 1/9*b_10 + 1/9*b_11 + 1/9*b_12

= 1/3*(-0.001014) + 1/3*(-0.043137) + 1/3*(-0.028493) + 1/9*(-0.004955) + 1/9*(0.016377) + 1/9*(0.007247) + 1/9*(0.003791) + 1/9*(-0.004661) + 1/9*(0.009432)


= 1/3*b[2] + 1/3*b[3] + 1/3*b[4] + 1/9*b[5] + 1/9*b[8] + 1/9*b[9] + 1/9*b[10] + 1/9*b[11] + 1/9*b[12]
= -0.021189

# respecify model with group == CCC vs not CCC
= -0.024564 (slightly different because this is weighted -- more accurate)
"
group.CCC = as.numeric(by_Session.table$group != "CCC")
mod.design2.by_Session.CCC = lm(round_accuracy ~ group.CCC * round,data=by_Session.table)
lmtest::coeftest(mod.design2.by_Session.CCC, vcov=vcovHC(mod.design2.by_Session.CCC))


summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "(T)-CCC"=c(0,1/3,1/3,1/3,0,0,1/9,1/9,1/9,1/9, 1/9, 1/9)
             )))
#---------------------------------------------------------------------

"
Effect on Round 3, of Receiving Treatment in Round 3 but not earlier : Difference-Difference Estimator

Round         R2 R3

Group CCC     C | C
--------------------
Group CCT     C | T


= (Difference of being in R3 rather than R2 in group CCT) - (Difference of being in R3 rather than R2 in group CCC)
= (b_6 - b_5 + b_10 - b_7) - (b_6 - b_5)
= b_10 - b_7
"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "D_in_D R3"=c(0,0,0,0,0,0,-1,0,0,1, 0, 0)
             )))

#---------------------------------------------------------------------

"
Effect on Round 2, of Receiving Treatment in Round 2 but not earlier : Difference-Difference Estimator

Round         R1 R2

Group CCC     C | C
Group CCT     C | C
--------------------
Group CTT     C | T


= (Difference of being in R2 rather than R1 in group CTT) - [(Difference of being in R2 rather than R1 in group CCT) + (Difference of being in R2 rather than R1 in group CCC)]/2
= (b_5 + b_8) - (b_5 + b_7 + b_5)/2
= - (1/2)*b_7 + b_8
"

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "D_in_D R2"=c(0,0,0,0,0,0,-1/2,1,0,0, 0, 0)
             )))


#---------------------------------------------------------------------#





