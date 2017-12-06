library(multcomp)
library(sandwich)
library(lmtest)
library(stargazer)
library(multiwayvcov)
library(reshape)


#---------------------------------------------------------------------#
# read in data for analysis
by_HIT.table = fread("../modeling_data/design2_by_HIT.csv")
by_Session.table = fread("../modeling_data/design2_by_Session.csv")
observed_outcomes_table = fread("../modeling_data/design2_observed_outcomes_table.csv")
assigned_treatment_condition_table = fread("../modeling_data/design2_assigned_treatment_condition_tablee.csv")

Y00.table= fread("../modeling_data/design2_Y00_table.csv")
Y01.table= fread("../modeling_data/design2_Y01_table.csv")
Y11.table= fread("../modeling_data/design2_Y11_table.csv")


Y00.table.filled= fread("../modeling_data/design2_Y00_table_full.csv")
Y01.table.filled= fread("../modeling_data/design2_Y01_table_full.csv")
Y11.table.filled= fread("../modeling_data/design2_Y11_table_full.csv")


Y.table.keyed= fread("../modeling_data/design2_Y_table_keyed.csv")
Y.table.keyed = Y.table.keyed[,c("worker_id","round_accuracy","outcome.key","round")]

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

#-----------------------------------------------------------------------#
# Sampling Distribution for CI

sim.outcomes.template = data.table(worker_id = by_Session.table$worker_id,
                                   group = rep(NA, 729),
                                   round = c(rep("one",243), rep("two",243),rep("three",243))
)

outcome_keys = data.table(group = c("CCC","CCC","CCC","CCT","CCT","CCT","CTT","CTT","CTT","TTT","TTT","TTT"),
                          round = rep(c("one","two","three"),4),
                          outcome.key = c("00","00","00","00","00","01","00","01","11","01","11","11"))


create_simulated_schedule = function (sim.outcomes.template, treat) {
  sim.outcomes = sim.outcomes.template
  sim.outcomes$group = rep(treat,3)
  sim.outcomes = merge(sim.outcomes,outcome_keys, by.x=c("group","round"),by.y=c("group","round"))
  sim.outcomes = merge(Y.table.keyed, sim.outcomes, 
                       by.x=c("round","outcome.key","worker_id"),
                       by.y=c("round","outcome.key","worker_id"))
  
  sim.outcomes  
}


est.ATE_from_simoutcomes = function(simulated_outcomes) {
  
  Y01_S1 = simulated_outcomes[round=="one" & group=="TTT",round_accuracy]
  Y01_S2 = simulated_outcomes[round=="two" & group=="CTT",round_accuracy]
  Y01_S3 = simulated_outcomes[round=="three" & group=="CTT",round_accuracy]
  Y00_S1 = simulated_outcomes[round=="one" & group!="TTT",round_accuracy]
  Y00_S2 = simulated_outcomes[round=="two" & (group=="CCT" | group=="CCC"),round_accuracy]
  Y00_S3 = simulated_outcomes[round=="three" & group=="CCC",round_accuracy]
  Y11_S2 = simulated_outcomese[round=="two" & group=="TTT",round_accuracy]
  Y11_S3 = simulated_outcomes[round=="three" & (group=="TTT" | group=="CTT"),round_accuracy] 
  
  E_Y01 = (sum(Y01_S1)/0.25 + sum(Y01_S2)/0.25 + sum(Y01_S3)/0.25) / 
    (length(Y01_S1)/0.25 + length(Y01_S2)/0.25  + length(Y01_S3)/0.25 )
  
  E_Y00.1 = (sum(Y00_S1)/0.75 + sum(Y00_S2)/0.50+ sum(Y00_S3)/0.25) /
    (length(Y00_S1)/0.75 + length(Y00_S2)/0.50+ length(Y00_S3)/0.25)
  
  E_Y01_Y00 = E_Y01 - E_Y00.1 
  
  E_Y11 = (sum(Y11_S2)/0.25 + sum(Y11_S3)/0.50) /
    (length(Y11_S2)/0.25 + length(Y11_S3)/0.50)
  
  E_Y00.2 = (sum(Y00_S2)/0.50 + sum(Y00_S3)/0.25) /
    (length(Y00_S2)/0.50 + length(Y00_S3)/0.25)
  
  E_Y11_Y00 = E_Y11 - E_Y00.2
}