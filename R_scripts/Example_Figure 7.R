
library (ggplot2)
library(latex2exp)
#
#   Figure 7 (left side)
#
#   Plot prior distribution
#
rm(list=ls())
file <- "Prior_beta1_beta2_prob.dat"
data_table <- read.table(file, header=FALSE)
colnames(data_table) <- c("beta1", "beta2", "prob")
Prob <- data_table$prob
#
#   Read data for constrained uniform prior
#
file_constrained <- "00beta_constrained_grid.dat"
data_table_constrained <- read.table(file_constrained, header=FALSE)
colnames(data_table_constrained) <- c("beta1", "beta2")
#
#   Plot grid of (beta1,beta2) points with probabilities
#
bound_low <- 0.0; bound_up <- 9
ggplot() +
  geom_point(aes(x=data_table_constrained$beta1, y=data_table_constrained$beta2), color='black', shape=23) +
  geom_point(aes(x=data_table$beta1, y=data_table$beta2, size=Prob), shape=16, alpha=0.5) +
  scale_x_continuous(limits=c(bound_low,bound_up), breaks=seq(bound_low,bound_up,1)) +
  scale_y_continuous(limits=c(bound_low,bound_up), breaks=seq(bound_low,bound_up,1)) +
  theme(aspect.ratio=1) +
  xlab(TeX("$\\beta_1$")) + ylab(TeX("$\\beta_2$")) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 16)) +
  theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))
#
#   Figure 7 (right side)
#
#   Plot posterior distribution when algorithm is done
#
rm(list=ls())
file <- "Post_finish_beta1_beta2_prob.dat"
data_table <- read.table(file, header=FALSE)
colnames(data_table) <- c("beta1", "beta2", "prob_when_done")
file_constrained <- "00beta_constrained_grid.dat"
data_table_constrained <- read.table(file_constrained, header=FALSE)
colnames(data_table_constrained) <- c("beta1", "beta2")
Prob <- data_table$prob_when_done
bound_low <- 0.0; bound_up <- 9
#
#   Plot grid of (beta1,beta2) points with probabilities
#
ggplot() +
  geom_point(aes(x=data_table$beta1, y=data_table$beta2, size=Prob), shape=16, alpha=0.5) +
  scale_x_continuous(limits=c(bound_low,bound_up), breaks=seq(bound_low,bound_up,1)) +
  scale_y_continuous(limits=c(bound_low,bound_up), breaks=seq(bound_low,bound_up,1)) +
  theme(aspect.ratio=1) +
  xlab(TeX("$\\beta_1$")) + ylab(TeX("$\\beta_2$")) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 16)) +
  theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))
