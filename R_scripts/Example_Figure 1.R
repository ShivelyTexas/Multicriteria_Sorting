
library(ggplot2)
#
#   Figure 1 (both sides - see diffent inputs below)
#
#   Plot vertical value distributions with shaded regions
#
#   Input parameter values and density blowup factor
#
rm(list=ls())
beta <-  2.0      #2.0 (for plot in left side of Figure 1)   2.62 (for plot in right side of Figure 1)
gamma <- 1.301    #1.301                                     1.644
stdev <- 1
x_place1 <- 0
x_place2 <- 0.4737
x_place3 <- 1
blowup_density <- 0.5
n_interval <- 1000
y <- vector(length=n_interval+1)
density_function <- vector(length=n_interval+1)
#
#   Initialize plot
#
figure <- ggplot() + 
  scale_x_continuous(expand = c(0, 0), limits=c(0, 1.3), breaks=seq(0, 1.3, 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-3,6), breaks=seq(-3,6,1)) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 16)) +
  xlab("x") + ylab("V(x)") +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
#
#   Construct first distribution
#
dmean <- beta * x_place1
upper_bound1 <- dmean + 3 * stdev
lower_bound1 <- dmean - 3 * stdev
#
#   Plot upper half of density function
#
y[1] = dmean
density_function[1] = x_place1 + blowup_density * dnorm(y[1], mean=dmean, sd = stdev)
width = (upper_bound1 - dmean) / n_interval
for(j in 1:n_interval)
{
  y[j+1] = y[j] + width
  density_function[j+1] = x_place1 + blowup_density * dnorm(y[j+1], mean=dmean, sd = stdev)
}
plotdata_upper1 <- data.frame(y, density_function)
figure <- figure + geom_line(aes(x=plotdata_upper1$density_function, y=plotdata_upper1$y))
#
#   Plot lower half of density function
#
y[1] = lower_bound1
density_function[1] = x_place1 + blowup_density * dnorm(y[1], mean=dmean, sd = stdev)
width = (dmean - lower_bound1) / n_interval
for(j in 1:n_interval)
{
  y[j+1] = y[j] + width
  density_function[j+1] = x_place1 + blowup_density * dnorm(y[j+1], mean=dmean, sd = stdev)
}
plotdata_lower1 <- data.frame(y, density_function)
figure <- figure + geom_line(aes(x=plotdata_lower1$density_function, y=plotdata_lower1$y))
#
#   Shade portion of density function from gamma to upper_bound2
#
if (gamma >= dmean) {
  cut_low <- gamma
  cut_up <- upper_bound1
  shade <- rbind(c(cut_low,x_place1), 
                 subset(plotdata_upper1, cut_low <= y & y <= cut_up),
                 c(cut_up,x_place1 + blowup_density * dnorm(cut_up, mean=dmean, sd=stdev)), 
                 c(cut_up,x_place1))
} else {
  cut_low <- gamma
  cut_up <- upper_bound1
  shade <- rbind(c(cut_low,x_place1), subset(plotdata_lower1, cut_low <= y & y <= dmean), 
                 subset(plotdata_upper1, dmean <= y & y <= cut_up),
                 c(cut_up,x_place1 + blowup_density * dnorm(cut_up, mean=dmean, sd=stdev)), 
                 c(cut_up,x_place1))
}
figure <- figure + geom_polygon(data = shade, aes(density_function, y), fill="grey") 
#
#   Plot line segment under density function
#
figure <- figure + geom_segment(aes(x=x_place1, y=lower_bound1, xend=x_place1, yend=upper_bound1), linetype=1)
#
#   Construct second distribution
#
dmean <- beta * x_place2
upper_bound2 <- dmean + 3 * stdev
lower_bound2 <- dmean - 3 * stdev
#
#   Plot upper half of density function
#
y[1] = dmean
density_function[1] = x_place2 + blowup_density * dnorm(y[1], mean=dmean, sd = stdev)
width = (upper_bound2 - dmean) / n_interval
for(j in 1:n_interval)
{
  y[j+1] = y[j] + width
  density_function[j+1] = x_place2 + blowup_density * dnorm(y[j+1], mean=dmean, sd = stdev)
}
plotdata_upper2 <- data.frame(y, density_function)
figure <- figure + geom_line(aes(x=plotdata_upper2$density_function, y=plotdata_upper2$y))
#
#   Plot lower half of density function
#
y[1] = lower_bound2
density_function[1] = x_place2 + blowup_density * dnorm(y[1], mean=dmean, sd = stdev)
width = (dmean - lower_bound2) / n_interval
for(j in 1:n_interval)
{
  y[j+1] = y[j] + width
  density_function[j+1] = x_place2 + blowup_density * dnorm(y[j+1], mean=dmean, sd = stdev)
}
plotdata_lower2 <- data.frame(y, density_function)
figure <- figure + geom_line(aes(x=plotdata_lower2$density_function, y=plotdata_lower2$y))
#
#   Shade portion of density function from gamma to upper_bound2
#
if (gamma >= dmean) {
  cut_low <- gamma
  cut_up <- upper_bound2
  shade <- rbind(c(cut_low,x_place2), 
                 subset(plotdata_upper2, cut_low <= y & y <= cut_up),
                 c(cut_up,x_place2 + blowup_density * dnorm(cut_up, mean=dmean, sd=stdev)), 
                 c(cut_up,x_place2))
} else {
  cut_low <- gamma
  cut_up <- upper_bound2
  shade <- rbind(c(cut_low,x_place2), subset(plotdata_lower2, cut_low <= y & y <= dmean), 
                 subset(plotdata_upper2, dmean <= y & y <= cut_up),
                 c(cut_up,x_place2 + blowup_density * dnorm(cut_up, mean=dmean, sd=stdev)), 
                 c(cut_up,x_place2))
}
figure <- figure + geom_polygon(data = shade, aes(density_function, y), fill="grey") 
#
#   Plot line segment under density function
#
figure <- figure + geom_segment(aes(x=x_place2, y=lower_bound2, xend=x_place2, yend=upper_bound2), linetype=1)
#
#   Construct third distribution
#
dmean <- beta * x_place3
upper_bound3 <- dmean + 3 * stdev
lower_bound3 <- dmean - 3 * stdev
#
#   Plot upper half of density function
#
y[1] = dmean
density_function[1] = x_place3 + blowup_density * dnorm(y[1], mean=dmean, sd = stdev)
width = (upper_bound3 - dmean) / n_interval
for(j in 1:n_interval)
{
  y[j+1] = y[j] + width
  density_function[j+1] = x_place3 + blowup_density * dnorm(y[j+1], mean=dmean, sd = stdev)
}
plotdata_upper3 <- data.frame(y, density_function)
figure <- figure + geom_line(aes(x=plotdata_upper3$density_function, y=plotdata_upper3$y))
#
#   Plot lower half of density function
#
y[1] = lower_bound3
density_function[1] = x_place3 + blowup_density * dnorm(y[1], mean=dmean, sd = stdev)
width = (dmean - lower_bound3) / n_interval
for(j in 1:n_interval)
{
  y[j+1] = y[j] + width
  density_function[j+1] = x_place3 + blowup_density * dnorm(y[j+1], mean=dmean, sd = stdev)
}
plotdata_lower3 <- data.frame(y, density_function)
figure <- figure + geom_line(aes(x=plotdata_lower3$density_function, y=plotdata_lower3$y))
#
#   Shade portion of density function from gamma to upper_bound3
#
if (gamma >= dmean) {
  cut_low <- gamma
  cut_up <- upper_bound3
  shade <- rbind(c(cut_low,x_place3), 
                 subset(plotdata_upper3, cut_low <= y & y <= cut_up),
                 c(cut_up,x_place3 + blowup_density * dnorm(cut_up, mean=dmean, sd=stdev)), 
                 c(cut_up,x_place3))
} else {
  cut_low <- gamma
  cut_up <- upper_bound3
  shade <- rbind(c(cut_low,x_place3), subset(plotdata_lower3, cut_low <= y & y <= dmean), 
                 subset(plotdata_upper3, dmean <= y & y <= cut_up),
                 c(cut_up,x_place3 + blowup_density * dnorm(cut_up, mean=dmean, sd=stdev)), 
                 c(cut_up,x_place3))
}
figure <- figure + geom_polygon(data = shade, aes(density_function, y), fill="grey") 
#
#   Plot line segment under density function
#
figure <- figure + geom_segment(aes(x=x_place3, y=lower_bound3, xend=x_place3, yend=upper_bound3), linetype=1)
#
#   Plot V(x) = beta * x and horizontal line at gamma
#
x <- c(x_place1, x_place2, x_place3, x_place3+0.3)
y <- beta * x
plotdata_V <- data.frame(x, y)
figure <- figure + 
  geom_line(aes(x=plotdata_V$x, y=plotdata_V$y)) +
  geom_hline(yintercept=gamma, linetype=2)

figure

