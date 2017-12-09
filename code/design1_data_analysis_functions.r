# load supporting functions
# setwd("/home/fred/Field_Experiment_Human_Image_Classification/code")
# setwd("D:/MIDS/W241_1_Experiments_Causality/project/Field_Experiment_Human_Image_Classification/code")
setwd("F:/001_Learn_UCB/241_Experiments_and_Causality/Field_Experiment_Human_Image_Classification/code")
source(file = "design1_data_transformation_functions.r")

library(effects)
#-------------------------------------------------------------------------------------------------

# TWO-SAMPLE T-TEST
est.t.test = function(d1, d2, alt){
  stacked_data = stack(list(d1=d1,d2=d2))
  ltest = car::leveneTest(values~ind,data = stacked_data, center = median)
  levene_p_val = ltest[1,3]
  t.test(d1, d2, alternative = alt, var.equal = levene_p_val >0.05)
}

# ATE by REGRESSION -- on treatment only
est.regr.simple = function(r_table){
  regr = lm(accuracy ~ treatment, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}
# ATE by REGRESSION -- on covariates only
est.regr.covars = function(r_table){
  regr = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}

# ATE by REGRESSION -- on covariates only
est.regr.covars_order = function(r_table){
  regr = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3 + order1, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}

# ATE by REGRESSION -- on covariates only
est.regr.covars_order_interact = function(r_table){
  regr = lm(accuracy ~ treatment + CQ1 + CQ2_3 + CQ3 + order1 + order1*treatment, data = r_table)
  lmtest::coeftest(regr, vcov(regr))
}
#-------------------------------------------------------------------------------------
# FOR RANDOMIZATION INFERENCE

est.ri.ate = function(d, treatment){
  d$treatment_new = treatment
  m1 = lm(accuracy ~ treatment_new, data =d)
  ate = lmtest::coeftest(m1, vcov(m1))[2]
  return(ate)
} 

