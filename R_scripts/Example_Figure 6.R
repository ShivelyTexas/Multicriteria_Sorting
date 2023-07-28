
library (ggplot2) 
#
#   Figure 6 (right side)
#
#   Construct plot with decisions made by algorithm
#
rm(list=ls())
file <- "I_vectors_X_matrix.dat"
data_table <- read.table(file, header=FALSE)
colnames(data_table) <- c("GPA", "ALES", "I_actual", "I_decision_algorithm", "I_decision_algorithm_wrong")
data_table$I_actual_character <- sprintf("%s", data_table$I_actual)
data_table$I_decision_algorithm_character <- sprintf("%s", data_table$I_decision_algorithm)
data_table$I_decision_wrong_algorithm_character <- sprintf("%s", data_table$I_decision_algorithm_wrong)
#
#   Replace I=8 values with the wrong decisions that were made
#
I_decision_only_wrong <- vector(length=73)
I_decision_only_wrong[1:73] <- ''
for (i in 1:73)
{
  if (data_table$I_decision_algorithm_wrong[i] == 8)
  {
    I_decision_only_wrong[i] <- data_table$I_decision_algorithm[i]
  }
}
data_table$I_decision_only_wrong_character <- sprintf("%s", I_decision_only_wrong)
#
#   Create vector with blanks when I=8
#
I_decision_only_correct_or_DM <- data_table$I_decision_algorithm
for (i in 1:73)
{
  if (data_table$I_decision_algorithm_wrong[i] == 8)
  {
    I_decision_only_correct_or_DM[i] <- ''
  }
}
#
#   Replace I=9 with I=0
#
for (i in 1:73)
{
  if (data_table$I_decision_algorithm_wrong[i] == 9)
  {
    I_decision_only_correct_or_DM[i] <- 0
  }
}
I_decision_only_correct_or_DM
data_table$I_decision_only_correct_or_DM_character <- sprintf("%s", I_decision_only_correct_or_DM)
#
#   Construct plot
#
figure <- ggplot(data_table, aes(x=GPA,y=ALES)) + geom_text(aes(label = I_decision_only_correct_or_DM_character), 
                                                            size = 6)
figure <- figure + geom_text(aes(label = I_decision_only_wrong_character), color='black', size = 6)
figure <- figure + geom_point(aes(x=GPA[36],y=ALES[36]), color='black', shape=0, size = 6)
figure <- figure + geom_point(aes(x=GPA[40],y=ALES[40]), color='black', shape=0, size = 6)
figure <- figure + scale_x_continuous(limits=c(2,4), breaks=seq(2,4,0.5))
figure <- figure + theme(axis.title = element_text(size = 18)) + theme(axis.text = element_text(size = 16))
figure <- figure + xlab("GPA") + ylab("ALES")
figure <- figure + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
                   theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))
figure
#
#   Figure 6 (left side)
#
#   Construct plot with actual decisions
#
I_actual_regular <- data_table$I_actual
I_actual_regular[26] <- ''
I_actual_regular[40] <- ''
I_actual_regular[50] <- ''
I_actual_regular[63] <- ''
data_table$I_actual_character_regular <- sprintf("%s", I_actual_regular)
I_actual_blue <- vector(length=73)
I_actual_blue[1:73] <- ''
I_actual_blue[26] <- 1
I_actual_blue[40] <- 1
I_actual_blue[50] <- 1
I_actual_blue[63] <- 1
data_table$I_actual_character_blue <- sprintf("%s", I_actual_blue)
figure <- ggplot(data_table, aes(x=GPA,y=ALES)) + geom_text(aes(label = I_actual_character_regular), size = 6)
figure <- figure + geom_text(aes(label = I_actual_character_blue), color='black', size = 6) #fontface='bold', 
figure <- figure + geom_point(aes(x=GPA[26],y=ALES[26]), color='black', shape=0, size = 6)
figure <- figure + geom_point(aes(x=GPA[40],y=ALES[40]), color='black', shape=0, size = 6)
figure <- figure + geom_point(aes(x=GPA[50],y=ALES[50]), color='black', shape=0, size = 6)
figure <- figure + geom_point(aes(x=GPA[63],y=ALES[63]), color='black', shape=0, size = 6)
figure <- figure + theme(axis.title = element_text(size = 18)) + theme(axis.text = element_text(size = 16))
figure <- figure + scale_x_continuous(limits=c(2,4), breaks=seq(2,4,0.5))
figure <- figure + xlab("GPA") + ylab("ALES")
figure <- figure + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
                   theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))
figure
