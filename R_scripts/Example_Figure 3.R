
library(ggplot2)
library(latex2exp)
#
#   Plot (GPA, ALES) with the convex hull
#
rm(list=ls())
file <- "I_vectors_X_matrix.dat"
data_table <- read.table(file, header=FALSE)
colnames(data_table) <- c("GPA", "ALES", "I_actual", "I_decision_algorithm", "I_decision_algorithm_wrong")
head(data_table)
tail(data_table)
#
#   Create plot
#
GPA_black <- c(data_table$GPA[1:7], data_table$GPA[9:11], data_table$GPA[13:20],data_table$GPA[22:40],
               data_table$GPA[42:67], data_table$GPA[69:73])
ALES_black <- c(data_table$ALES[1:7], data_table$ALES[9:11], data_table$ALES[13:20],data_table$ALES[22:40],
                data_table$ALES[42:67], data_table$ALES[69:73])
GPA_red  <- c(data_table$GPA[12],  data_table$GPA[21],  data_table$GPA[41])
ALES_red <- c(data_table$ALES[12], data_table$ALES[21], data_table$ALES[41])
GPA_blue  <- c(data_table$GPA[8],  data_table$GPA[68])
ALES_blue <- c(data_table$ALES[8], data_table$ALES[68])
convex_hull_line1_x <- c(data_table$GPA[12],  data_table$GPA[57],  data_table$GPA[13],  data_table$GPA[16],
                         data_table$GPA[3],   data_table$GPA[54],  data_table$GPA[46])
convex_hull_line1_y <- c(data_table$ALES[12], data_table$ALES[57], data_table$ALES[13], data_table$ALES[16],
                         data_table$ALES[3],  data_table$ALES[54], data_table$ALES[46])
convex_hull_line2_x <- c(data_table$GPA[46],  data_table$GPA[36],  data_table$GPA[21])
convex_hull_line2_y <- c(data_table$ALES[46], data_table$ALES[36], data_table$ALES[21])
convex_hull_line3_x <- c(data_table$GPA[21],  data_table$GPA[41],  data_table$GPA[12])
convex_hull_line3_y <- c(data_table$ALES[21], data_table$ALES[41], data_table$ALES[12])
ggplot() +
  geom_point(aes(x=GPA_black,y=ALES_black), color="black", shape=19) +
  geom_point(aes(x=GPA_red,y=ALES_red), color="grey", shape=15, size=2) +
  geom_point(aes(x=GPA_blue,y=ALES_blue), color="black", shape=19) +
  geom_line(aes(x=convex_hull_line1_x,y=convex_hull_line1_y), color="black") +
  geom_line(aes(x=convex_hull_line2_x,y=convex_hull_line2_y), color="black") +
  geom_line(aes(x=convex_hull_line3_x,y=convex_hull_line3_y), color="black", lty=2) +
  scale_x_continuous(limits=c(2,4), breaks=seq(2,4,0.5)) +
  xlab(TeX("$\\x_1$")) + ylab(TeX("$\\x_2$")) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 16)) +
  theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))

