# Use this code to evaluate worker performance based on pilot resul csvs
rm(list = ls())

# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/code")
source(file = "design2_data_transformation_functions.r")
#source(file = "design2_data_analysis_functions.r")

#---------------------------------------------------------------------#

# read in data for analysis
by_HIT.table = fread("../modeling_data/design2_by_HIT.csv")
by_Session.table = fread("../modeling_data/design2_by_Session.csv")

View(by_HIT.table)
View(by_Session.table)
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

# plot distribution (Brief EDA)

# specify model

unique(by_Session.table$CQ1)

lm(round_accuracy ~ group * round + CQ1 + CQ2 + CQ3 + CQ4 + CQ5,data=by_Session.table)

# Specify model?

