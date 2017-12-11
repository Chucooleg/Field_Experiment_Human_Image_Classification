source(file = "design1_data_transformation_functions.r")

library(effects)
library(foreign)
library(sandwich)
library(lmtest)
library(data.table)
library(multiwayvcov)
library(ggplot2)
library(cowplot)

library(kableExtra)


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
#-------------------------------------------------------------------------------------
# standard errors

get_ate_se_robustci = function (mod, coef_idx) {
  ate = mod$coefficients[coef_idx]
  robust.se = lmtest::coeftest(mod, vcov = vcovHC(mod))[coef_idx,2]
  robust.p = lmtest::coeftest(mod, vcov = vcovHC(mod))[coef_idx,4]
  df = summary(mod)$df[2]
  t.stat = qt(p = 0.975, df = df, lower.tail = TRUE)
  robust.ci = c(ate-t.stat*robust.se, ate+t.stat*robust.se )
  
  cat("estimated average causal effect = $", ate,
      "\nrobust standard error = $", robust.se,
      "\n95% confidence interval = ", robust.ci,
      "\np-value = ",robust.p)
}


# fix this
get_ate_se_clusteredci = function(mod, mod_name,coef_idx) {
  ate = as.numeric(mod$coefficients[coef_idx])
  clustered.vcov = cluster.vcov(mod, ~cluster )
  clustered.se = as.numeric(sqrt(diag(clustered.vcov)))[2]
  clustered.ci = ate + qnorm(p = c(0.05/2, 1-0.05/2))*clustered.se
  cat(mod_name,
      "\nATE estimate =", ate,
      "\nClustered standard errors =", clustered.se,
      "\n.95 CI with clustered SE = [", clustered.ci, "]")
}
