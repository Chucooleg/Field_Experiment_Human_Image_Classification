library(data.table)
library(magrittr)

data = fread("F:/001_Learn_UCB/241_Experiments_and_Causality/final_project/Field_Experiment_Human_Image_Classification/qualtric_data/20171028_results_pilot_0.10.csv")

ans = c(
        "Boston Bull", 
        "Shih Tzu",
        "Shih Tzu",
        "Cocker Spaniel",
        "Irish wolfhound",
        "Golden retriever",
        "Cocker Spaniel",
        "Yorkshire Terrier",
        "Saluki",
        "Shih Tzu",
        "Yorkshire Terrier",
        "Bloodhound",
        "Shih Tzu",
        "Irish wolfhound",
        "Saluki",
        "Yorkshire Terrier",
        "Not A Dog",
        "Cocker Spaniel",
        "Golden retriever",
        "Golden retriever",
        "Irish wolfhound",
        "Yorkshire Terrier",
        "Bloodhound",
        "Cocker Spaniel",
        "Saluki",
        "Boston Bull",
        "Saluki",
        "Cocker Spaniel",
        "Irish wolfhound",
        "Golden retriever",
        "Yorkshire Terrier",
        "Saluki",
        "Cocker Spaniel",
        "Boston Bull",
        "Not A Dog",
        "Yorkshire Terrier",
        "Boston Bull",
        "Golden retriever",
        "Bloodhound",
        "Golden retriever",
        "Bloodhound",
        "Saluki",
        "Boston Bull",
        "Shih Tzu",
        "Bloodhound",
        "Boston Bull",
        "Irish wolfhound",
        "Shih Tzu",
        "Irish wolfhound",
        "Bloodhound"        
        )


end = 56 # check excel sheet for last row of valid responses
subject.row_range = c(3:end) # valid responses from row 3 to end
n = end - 3 + 1

# unchecked responses in breed names
dt.raw_responses = data[subject.row_range, 30:79]  #from TQ1 to TQ48

# check responses and store in new frame
dt.correct_responses = data.table(ID = data[subject.row_range,]$Q167)
for (i in 1:50) {
  col.name = names(dt.raw_responses)[i]
  answer = ans[i]
  newcol = as.numeric(dt.raw_responses[, get(col.name)] == answer)
  dt.correct_responses = cbind(dt.correct_responses, newcol)
}
column.names = names(dt.raw_responses) # "TQ1", "TQ2", ...
names(dt.correct_responses) = c("ID", column.names)

# check accuracy for each question
dt.question.accuracies = dt.correct_responses[, 2:ncol(dt.correct_responses)]%>%.[, lapply(.SD, mean, na.rm = TRUE)]

allQ = c("TQ1", "TQ2", "TQ3", "TQ4", "TQ5", "TQ6",
         "TQ7", "TQ8", "TQ9", "TQ10", "TQ11", "TQ12",
         "TQ13", "TQ14", "TQ15", "TQ16", "TQ17", "TQ18",
         "TQ19", "TQ20", "TQ21", "TQ22", "TQ23", "TQ24",
         "TQ25", "TQ26", "TQ27", "TQ28", "TQ29", "TQ30",
         "TQ31", "TQ32", "TQ33", "TQ34", "TQ35", "TQ36",
         "TQ37", "TQ38", "TQ39", "TQ40", "TQ41", "TQ42",
         "TQ43", "TQ44", "TQ45", "TQ46", "TQ47", "TQ48")

dt.question.accuracies[, .(
                           mean = mean(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                                     TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                                     TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                                     TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                                     TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                                     TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                                     TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                                     TQ43, TQ44, TQ45, TQ46, TQ47, TQ48)),
                           sd = sd(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                                         TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                                         TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                                         TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                                         TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                                         TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                                         TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                                         TQ43, TQ44, TQ45, TQ46, TQ47, TQ48)),
                           se = sd(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                                     TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                                     TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                                     TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                                     TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                                     TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                                     TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                                     TQ43, TQ44, TQ45, TQ46, TQ47, TQ48))/sqrt(n),
                           min = min(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                                     TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                                     TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                                     TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                                     TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                                     TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                                     TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                                     TQ43, TQ44, TQ45, TQ46, TQ47, TQ48)),
                           max = max(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                                     TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                                     TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                                     TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                                     TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                                     TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                                     TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                                     TQ43, TQ44, TQ45, TQ46, TQ47, TQ48))
                           )] 

# check accuracy for each subject, not includnig screeners
dt.subject.accuracies = dt.correct_responses[,.(accuracy = mean(c(TQ1, TQ2, TQ3, TQ4, TQ5, TQ6,
                                       TQ7, TQ8, TQ9, TQ10, TQ11, TQ12,
                                       TQ13, TQ14, TQ15, TQ16, TQ17, TQ18,
                                       TQ19, TQ20, TQ21, TQ22, TQ23, TQ24,
                                       TQ25, TQ26, TQ27, TQ28, TQ29, TQ30,
                                       TQ31, TQ32, TQ33, TQ34, TQ35, TQ36,
                                       TQ37, TQ38, TQ39, TQ40, TQ41, TQ42,
                                       TQ43, TQ44, TQ45, TQ46, TQ47, TQ48))), by = ID]

dt.subject.accuracies[, .(mean = mean(accuracy), sd = sd(accuracy), se = sd(accuracy)/sqrt(n) , min = min(accuracy), max = max(accuracy))]

