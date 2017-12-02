# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

library(multcomp)

# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design2_data_transformation_functions.r")
#source(file = "design2_data_analysis_functions.r")

#---------------------------------------------------------------------#

# read in data for analysis
by_HIT.table = fread("../modeling_data/design2_by_HIT.csv")
by_Session.table = fread("../modeling_data/design2_by_Session.csv")

#View(by_HIT.table)
#View(by_Session.table)
#---------------------------------------------------------------------#
# Fix data type of imported csv 

by_Session.table$group = factor(by_Session.table$group, levels = c("CCC", "CCT", "CTT", "TTT"))
by_Session.table$round = factor(by_Session.table$round, levels = c("one", "two", "three"))
by_Session.table$CQ1 = factor(by_Session.table$CQ1, levels = c("a lot less than half", "around half", "a lot more than half"))
by_Session.table$CQ2 = as.numeric(by_Session.table$CQ2)
by_Session.table$CQ3 = factor(by_Session.table$CQ3, levels = c("No", "Maybe", "Yes"))
by_Session.table$CQ4 = factor(by_Session.table$CQ4, levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more"))
by_Session.table$CQ5 = factor(by_Session.table$CQ5, levels = c("Never heard of Linkedin", "No", "Yes"))
by_Session.table$round_screener = as.numeric(by_Session.table$round_screener)
by_Session.table$round_timespent = as.numeric(by_Session.table$round_timespent)
by_Session.table$round_accuracy = as.numeric(by_Session.table$round_accuracy)


by_HIT.table$group = factor(by_HIT.table$group, levels = c("CCC", "CCT", "CTT", "TTT"))
by_HIT.table$CQ1 = factor(by_HIT.table$CQ1, levels = c("a lot less than half", "around half", "a lot more than half"))
by_HIT.table$CQ2 = as.numeric(by_HIT.table$CQ2)
by_HIT.table$CQ3 = factor(by_HIT.table$CQ3, levels = c("No", "Maybe", "Yes"))
by_HIT.table$CQ4 = factor(by_HIT.table$CQ4, levels = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 or more"))
by_HIT.table$CQ5 = factor(by_HIT.table$CQ5, levels = c("Never heard of Linkedin", "No", "Yes"))

#---------------------------------------------------------------------#
# check attrition

attrition_by_group = lm(observed~group,data=by_HIT.table)
summary(attrition_by_group) #nothing significant

attrition_by_group_CQ1.1 = lm(observed~group,data=by_HIT.table[CQ1=="a lot less than half",])
summary(attrition_by_group_CQ1.1) # groupCCT 0.047434 *
attrition_by_group_CQ1.2 = lm(observed~group,data=by_HIT.table[CQ1=="around half",])
summary(attrition_by_group_CQ1.2) # nothing significant
attrition_by_group_CQ1.3 = lm(observed~group,data=by_HIT.table[CQ1=="a lot more than half",])
summary(attrition_by_group_CQ1.3) # groupCCT 0.00154 *

attrition_by_group_CQ2 = lm(observed~group*CQ2,data=by_HIT.table)
summary(attrition_by_group_CQ2) # nothing significant

attrition_by_group_CQ3.1 = lm(observed~group,data=by_HIT.table[CQ3=="Yes",])
summary(attrition_by_group_CQ3.1) # nothing significant
attrition_by_group_CQ3.2 = lm(observed~group,data=by_HIT.table[CQ3=="No",])
summary(attrition_by_group_CQ3.2) # nothing significant
attrition_by_group_CQ3.3 = lm(observed~group,data=by_HIT.table[CQ3=="Maybe",])
summary(attrition_by_group_CQ3.3) # nothing significant

attrition_by_group_CQ4.1 = lm(observed~group,data=by_HIT.table[CQ4=="0 to 10",])
summary(attrition_by_group_CQ4.1) # nothing significant
attrition_by_group_CQ4.2 = lm(observed~group,data=by_HIT.table[CQ4=="11 to 20",])
summary(attrition_by_group_CQ4.2) # nothing significant
attrition_by_group_CQ4.3 = lm(observed~group,data=by_HIT.table[CQ4=="21 to 30",])
summary(attrition_by_group_CQ4.3) # nothing significant
attrition_by_group_CQ4.4 = lm(observed~group,data=by_HIT.table[CQ4=="31 to 40",])
summary(attrition_by_group_CQ4.4) # nothing significant
attrition_by_group_CQ4.5 = lm(observed~group,data=by_HIT.table[CQ4=="41 or more",])
summary(attrition_by_group_CQ4.5) # nothing significant

attrition_by_group_CQ5.1 = lm(observed~group,data=by_HIT.table[CQ5=="Yes",])
summary(attrition_by_group_CQ5.1) # nothing significant
attrition_by_group_CQ5.2 = lm(observed~group,data=by_HIT.table[CQ5=="No",])
summary(attrition_by_group_CQ5.2) # nothing significant
attrition_by_group_CQ5.3 = lm(observed~group,data=by_HIT.table[CQ5=="Never heard of Linkedin",])
summary(attrition_by_group_CQ5.3) # nothing significant

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
# Can do some brief EDA

# !!! Please fill in 

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
summary(mod.design2.by_Session)

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
#---------------------------------------------------------------------

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

summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "CCC R2-R1"=c(0,0,0,0,1,0,0,0,0,0, 0, 0),
               "CCC R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 0, 0),
               "CCC R3-R2"=c(0,0,0,0,-1,1,0,0,0,0, 0, 0)
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


summary(glht(mod.design2.by_Session , vcov=vcovHC(mod.design2.by_Session),
             linfct=rbind(
               "CCT R2-R1"=c(0,0,0,0,1,0,1,0,0,0, 0, 0),
               "CCT R3-R1"=c(0,0,0,0,0,1,0,0,0,1, 0, 0),
               "CCT R3-R2"=c(0,0,0,0,-1,1,-1,0,0,1, 0, 0)
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
               "CTT R2-R1"=c(0,0,0,0,1,0,0,0,1,0, 0, 0),
               "CTT R3-R1"=c(0,0,0,0,0,1,0,0,0,0, 0, 1),
               "CTT R3-R2"=c(0,0,0,0,-1,1,0,0,-1,0, 0, 1)
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
               "TTT-CCC"=c(0,1/3,1/3,1/3,0,0,1/9,1/9,1/9,1/9, 1/9, 1/9)
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
# WITH COVARIATES (TO BE CONTINUED)
mod.design2.by_Session.cov = lm(round_accuracy ~ group * round + CQ1 + CQ2 + CQ3 + CQ4 + CQ5,data=by_Session.table)
lmtest::coeftest(mod.design2.cov, vcov=vcovHC(mod.design2.by_Session))




