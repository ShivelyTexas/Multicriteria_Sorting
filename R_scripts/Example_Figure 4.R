
library(ggplot2)
library(latex2exp)
library(mnormt)
library(plot3D)
#
#   Figure 4 (left side)
#
#   Plot region denoted R as a shaded region
#
rm(list=ls())
file_edge_constrained <- "00beta_edge_constrained_grid.dat"
data_table_edge_constrained <- read.table(file_edge_constrained, header=FALSE)
colnames(data_table_edge_constrained) <- c("beta1", "beta2")
n <- nrow(data_table_edge_constrained)
lower_bound <- 0
upper_bound_x <- 10.0
upper_bound_y <- 10.0
cut_low <- 0
cut_up <- data_table_edge_constrained$beta2[1]
point1_x <- cut_low
point1_y <- cut_up
point2_x <- cut_low
point2_y <- upper_bound_y
point3_x <- upper_bound_x
point3_y <- upper_bound_y
point4_x <- upper_bound_x
point4_y <- cut_low
point5_x <- data_table_edge_constrained$beta1[n]
point5_y <- cut_low
shade <- rbind(c(point1_x,point1_y), c(point2_x,point2_y), c(point3_x,point3_y), c(point4_x,point4_y), 
               c(point5_x,point5_y),
               subset(data_table_edge_constrained, cut_low <= beta1 & beta1 <= 8.34))  #cut_up))
ggplot(data_table_edge_constrained) + 
  geom_polygon(data = shade, aes(beta1, beta2), fill="grey") + 
  geom_line(aes(x=beta1, y=beta2)) +
  scale_y_continuous(expand = c(0, 0), limits=c(lower_bound,upper_bound_y)) +
  scale_x_continuous(expand = c(0, 0), limits=c(lower_bound,upper_bound_x)) +
  xlab(TeX("$\\beta_1$")) + ylab(TeX("$\\beta_2$")) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 16)) +
  theme(aspect.ratio=1) +
  theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))
#
#   Figure 4 (right side)
#
#   Plot constrained bivariate normal distribution
#
rm(list=ls())
beta_1 <- seq(0, 5, 0.25) 
beta_2 <- seq(0, 5, 0.25)
mu    <- c(0, 0)
Sigma <- matrix(c(1.487, -0.111, -0.111, 1.181), nrow = 2)
f     <- function(beta_1, beta_2) dmnorm(cbind(beta_1, beta_2), mu, Sigma)
pi     <- outer(beta_1, beta_2, f)

persp(beta_1, beta_2, pi, theta = -30, phi = 25, 
      shade = 0.75, col = "green", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed", zlab="\n pi")
