library(tidyverse)
library(gridExtra)
library(ggthemes)
ggplot2::theme_set(theme_tufte())
setwd("~/Documents/dev/linespots/linespots-analysis/other/")


# Weighting function

d = tibble("x" = seq(0, 1, 0.001))
d$google = (1 / (1 + exp((-12 * d$x) + 12)))
d$linear = d$x
d$flat = rep(1, 1001)

points = tibble("x" = c(0.75, 0.85))
points$y = (1 / (1 + exp((-12 * points$x) + 12)))

pdf("google-weighting.pdf")
ggplot(d, aes(x, google)) + geom_line() +
  ggtitle("Google Weighting Function", expression(paste("w(x)= ", frac(1, exp(-12 * x + 12))))) +
  xlab("Relative Commit Age") + ylab("Weight")
dev.off()

pdf("linear-weighting.pdf")
ggplot(d, aes(x, linear)) + geom_line() + ggtitle("Linear Weighting Function", "w(x) = x") +
  xlab("Relative Commit Age") + ylab("Weight")
dev.off()

pdf("flat-weighting.pdf")
ggplot(d, aes(x, flat)) + geom_line() + ggtitle("Flat Weighting Function", "w(x) = 1") +
  xlab("Relative Commit Age") + ylab("Weight")
dev.off()

points = tibble("x" = c(0.85, 0.9))
points$y = (1 / (1 + exp((-12 * points$x) + 12)))
pdf("time-weighting.pdf")
ggplot(d, aes(x, google)) + geom_line() +
  ggtitle("Google Weighting Function", expression(paste("w(x)= ", frac(1, exp(-12 * x)+12)))) +
  xlab("Relative Commit Age") + ylab("Weight") +
  geom_point(data=points, aes(x, y), size=4)+
  geom_text(x=0.77, y=(1 / (1 + exp((-12 * 0.85) + 12))), label="A", size = 10) +
  geom_text(x=0.82, y=(1 / (1 + exp((-12 * 0.9) + 12))), label="B", size = 10)
dev.off()

points = tibble("x" = c(0.8, 0.9))
points$y = (1 / (1 + exp((-12 * points$x) + 12)))
pdf("index-weighting.pdf")
ggplot(d, aes(x, google)) + geom_line() +
  ggtitle("Google Weighting Function", expression(paste("w(x)= ", frac(1, exp(-12 * x)+12)))) +
  xlab("Relative Commit Age") + ylab("Weight") +
  geom_point(data=points, aes(x, y), size=4)+
  geom_text(x=0.72, y=(1 / (1 + exp((-12 * 0.8) + 12))), label="A", size = 10) +
  geom_text(x=0.82, y=(1 / (1 + exp((-12 * 0.9) + 12))), label="B", size = 10)
dev.off()


points = tibble("x" = c(0.8, 0.9))
points$y = (1 / (1 + exp((-12 * points$x) + 12)))
pdf("index-weighting.pdf")
ggplot(d, aes(x, google)) + geom_line() +
  ggtitle("Google Weighting Function", expression(paste("w(x)= ", frac(1, exp(-12 * x)+12)))) +
  xlab("Relative Commit Age") + ylab("Weight") +
  geom_point(data=points, aes(x, y), size=4)+
  geom_text(x=0.72, y=(1 / (1 + exp((-12 * 0.8) + 12))), label="A", size = 10) +
  geom_text(x=0.82, y=(1 / (1 + exp((-12 * 0.9) + 12))), label="B", size = 10)
dev.off()


d = tibble("x" = seq(0, 1, 0.001))
d$Optimal = d$x^(1/8)
d$Random = d$x

pdf("aucec.pdf")
ggplot(data = d, aes(x=x)) +
  geom_line(aes(y = Optimal,linetype = "Optimal", color= "Optimal"), size=1) +
  geom_line(aes(y = Random, linetype = "Random", color= "Random"), size=1) +
  scale_linetype_manual("",values=c("Optimal"=1,"Random"=4)) +
  scale_color_manual("", values=c("Optimal"="grey22", "Random"="grey60"))+
  ggtitle("Cost-Effectiveness Curve", "Optimal and Random Case") +
  xlab("LOC") + ylab("Faults")
dev.off()