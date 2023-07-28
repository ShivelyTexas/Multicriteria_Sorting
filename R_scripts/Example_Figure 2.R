
library(ggplot2)
library(latex2exp)
#
#   Figure 2
#
#   Plot density function for constrained prior on beta 
#
rm(list=ls())
dmean <- 0
stdev <- 0.8478
beta_U <- 2.62
beta_line_at_beta_U_x <- c(beta_U, beta_U)
beta_line_at_beta_U_y <- c(0, 0.1)
n_interval <- 1000
x <- vector(length=n_interval+1)
density_function <- vector(length=n_interval+1)
lower_bound <- dmean
upper_bound <- dmean + 4 * stdev
cut_low <- beta_U
cut_up <- upper_bound
x[1] = lower_bound
density_function[1] = dnorm(x[1], mean=dmean, sd = stdev)
width = (upper_bound - lower_bound) / n_interval
for(j in 1:n_interval)
{
  x[j+1] = x[j] + width
  density_function[j+1] = dnorm(x[j+1], mean=dmean, sd = stdev)
}
plotdata <- data.frame(x, density_function)
ggplot() + 
  geom_line(aes(x=plotdata$x[1:797], y=plotdata$density_function[1:797])) +
  geom_line(aes(x=plotdata$x[798:1001], y=plotdata$density_function[798:1001]), color="red", lty = 2) +
  geom_line(aes(x=beta_line_at_beta_U_x, y=beta_line_at_beta_U_y), lty=2) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.15*dnorm(dmean, mean=dmean, sd = stdev)), breaks=c(0,0.2)) +
  scale_x_continuous(expand = c(0, 0), limits=c(lower_bound-0.001,upper_bound+0.001)) +
  xlab(TeX("$\\beta_1$")) + ylab(TeX("$\\pi(beta_1)$")) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 16)) +
  theme(aspect.ratio=1) +
  theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) + theme(axis.line = element_line(colour = "black"))

