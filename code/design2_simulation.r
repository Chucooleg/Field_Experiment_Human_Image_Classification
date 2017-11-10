Design = data.table(
           Group = c("A","B","C","D"), 
           N = c(100,100,100,100),
           Randomize = c("R", "R", "R", "R"),
           Round1 = c("C", "C", "C", "T"),
           Round2 = c("C", "C", "T", "T"),
           Round3 = c("C", "T", "T", "T"),
           TotalBonus = c(0, 0.15, 0.3, 0.45))

Truth = data.table(
  Group = c("A","B","C","D"), 
  TotalBonus = c(0, 0.15, 0.3, 0.45),
  Mean = c(0.56, 0.67, 0.76, 0.83),
  SD = c(0.3, 0.3, 0.3, 0.3))


Design
Truth