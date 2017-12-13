library(effects)
library(foreign)
library(sandwich)
library(lmtest)
library(data.table)
library(multiwayvcov)
library(ggplot2)
library(cowplot)
library(multcomp)

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

est.ri.ate = function(treatment_n){
  regr_table_d1$new_treat = treatment_n
  m1 = lm(accuracy ~ new_treat, data=regr_table_d1)
  ate = lmtest::coeftest(m1, vcov=multiwayvcov::cluster.vcov(m1, ~cluster))[2]
  return(ate)
} 

est.ri.ate.control = function(treatment_n){
  regr_table_d1$new_treat = treatment_n  
  m1 = lm(accuracy ~ new_treat + CQ1 + CQ2_3 + CQ3 + order1, data =regr_table_d1)
  ate = lmtest::coeftest(m1, vcov=multiwayvcov::cluster.vcov(m1, ~cluster))[2]
  return(ate)
} 

#-------------------------------------------------------------------------------------
# standard errors

get_ate_se_robustci = function (mod, coef_idx) {
  ate = mod$coefficients[coef_idx]
  robust.se = lmtest::coeftest(mod, vcov = vcovHC(mod, type = "HC1"))[coef_idx,2]
  robust.p = lmtest::coeftest(mod, vcov = vcovHC(mod, type = "HC1"))[coef_idx,4]
  df = summary(mod)$df[2]
  t.stat = qt(p = 0.975, df = df, lower.tail = TRUE)
  robust.ci = c(ate-t.stat*robust.se, ate+t.stat*robust.se )
  
  cat("estimated average causal effect = ", ate,
      "\nrobust standard error = ", robust.se,
      "\n95% confidence interval = ", robust.ci,
      "\np-value = ",robust.p)
}



get_ate_se_clusteredci = function(mod, mod_name,coef_idx) {
  ate = as.numeric(mod$coefficients[coef_idx])
  clustered.se = lmtest::coeftest(mod, vcov = multiwayvcov::cluster.vcov(mod, ~cluster))[2, 2]
  clustered.p = lmtest::coeftest(mod, vcov = multiwayvcov::cluster.vcov(mod, ~cluster))[2, 4]
  df = summary(mod)$df[2]
  t.stat = qt(p = 0.975, df = df, lower.tail = TRUE)
  clustered.ci = c(ate-t.stat*clustered.se, ate+t.stat*clustered.se)
  cat("\n",mod_name,
      "\nestimated average causal effect = ", ate,
      "\nClustered standard errors =", clustered.se,
      "\n.95 CI with clustered SE = [", clustered.ci, "]",
      "\np-value = ",clustered.p,"\n")
}


# Linear combo only, not prediction
get_ate_se_clusteredci_LR = function(mod, mod_name,coef_idx) {
  ate = as.numeric(mod$coefficients[coef_idx])
  clustered.se = suppressWarnings(lmtest::coeftest(mod, vcov = multiwayvcov::cluster.vcov(mod, ~cluster)))[2, 2]
  clustered.p = suppressWarnings(lmtest::coeftest(mod, vcov = multiwayvcov::cluster.vcov(mod, ~cluster)))[2, 4]
  df = summary(mod)$df[2]
  t.stat = qt(p = 0.975, df = df, lower.tail = TRUE)
  clustered.ci = c(ate-t.stat*clustered.se, ate+t.stat*clustered.se)
 # Profile.Lik.Int = suppressWarnings(as.numeric(confint(object = mod, level = 0.95, vcov = multiwayvcov::cluster.vcov(mod, ~cluster), type="vcov")[2,]))
  #Wald.ci = suppressWarnings(as.numeric(confint.default(object = mod, level = 0.95, vcov = multiwayvcov::cluster.vcov(mod, ~cluster), type="vcov")[2,]))
  cat("\n",mod_name,
      "\nestimated linear combination = ", ate,
      "\nClustered standard errors =", clustered.se,
      "\np-value = ",clustered.p,
      "\n.95 CI with clustered SE = [", clustered.ci, "]\n")
      #"\n.95 Wald CI = [",Wald.ci, "]",
      #"\n.95 CI with (Profile Likelihood Interval) = [", Profile.Lik.Int ,"]\n")
}





# NEED TO FIX or ABANDON
get_ate_se_robustci_LR = function (mod, coef_idx) {
  ate = mod$coefficients[coef_idx]
  robust.se = lmtest::coeftest(mod, vcov = vcovHC(mod))[coef_idx,2]
  robust.p = lmtest::coeftest(mod, vcov = vcovHC(mod))[coef_idx,4]
  df = summary(mod)$df[2]
  t.stat = qt(p = 0.975, df = df, lower.tail = TRUE)
  robust.ci = c(ate-t.stat*robust.se, ate+t.stat*robust.se )
  
  cat("estimated average causal effect = ", ate,
      "\nrobust standard error = ", robust.se,
      "\n95% confidence interval = ", robust.ci,
      "\np-value = ",robust.p)
}


# NEED TO FIX or ABANDON
get_ate_se_clusteredci_LR = function(mod, mod_name,coef_idx) {
  ate = as.numeric(mod$coefficients[coef_idx])
  clustered.se = lmtest::coeftest(mod, vcov = multiwayvcov::cluster.vcov(mod, ~cluster))[2, "Std. Error"]
  clustered.p = lmtest::coeftest(mod, vcov = multiwayvcov::cluster.vcov(mod, ~cluster))[2, "Pr(>|t|)"]
  df = summary(mod)$df[2]
  t.stat = qt(p = 0.975, df = df, lower.tail = TRUE)
  clustered.ci = c(ate-t.stat*clustered.se, ate+t.stat*clustered.se)
  cat("\n",mod_name,
      "estimated average causal effect = ", ate,
      "Clustered standard errors =", clustered.se,
      ".95 CI with clustered SE = [", clustered.ci, "]",
      "p-value = ",clustered.p,"\n")
}
