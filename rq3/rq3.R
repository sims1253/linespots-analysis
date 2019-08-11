library(tidyverse)
library(ggthemes)
library(gridExtra)
ggplot2::theme_set(theme_tufte())
bayesplot::color_scheme_set("darkgray")
options(mc.cores = parallel::detectCores())

SEED = 140919 # The day I move in with my gf

setwd("~/Documents/dev/linespots/linespots-analysis/rq3")

d = read_delim('../data.csv',
               delim = ",",
               locale = locale(decimal_mark = "."),
               col_names = TRUE,
               col_types = cols(
                 AUCECDENSITY = col_double(),
                 AUCECEXAM = col_double(),
                 Algorithm = col_factor(),
                 Choice = col_factor(),
                 Commits = col_double(),
                 Depth = col_double(),
                 Domain = col_factor(),
                 EInspect10Density = col_double(),
                 EInspect10EXAM = col_double(),
                 EInspect25Density = col_double(),
                 EInspect25EXAM = col_double(),
                 EXAM = col_double(),
                 EXAM25 = col_double(),
                 EXAM33 = col_double(),
                 EXAM50 = col_double(),
                 EXAM75 = col_double(),
                 EXAM95 = col_double(),
                 EXAMF = col_double(),
                 FixCount = col_double(),
                 FixRanks = col_guess(),
                 FullExam = col_guess(),
                 Future = col_double(),
                 ID = col_factor(),
                 LOC = col_double(),
                 Language = col_factor(),
                 MRD = col_double(),
                 Origin = col_double(),
                 Project = col_factor(),
                 SRD = col_double(),
                 Source = col_factor(),
                 Time = col_factor(),
                 WCP = col_double(),
                 WCW = col_double(),
                 Weighting = col_factor(),
                 fn = col_double(),
                 fp = col_double(),
                 hdMaxDensity = col_double(),
                 hdMaxEXAM = col_double(),
                 hdMaxLOCDensity = col_double(),
                 hdMaxLOCEXAM = col_double(),
                 tn = col_double(),
                 tp = col_double()
               )
)

d = subset(d, d$FixCount != 0)

# Standardizing
d$Commits = (d$Commits - mean(d$Commits)) / sd(d$Commits)
d$LOC = (d$LOC - mean(d$LOC)) / sd(d$LOC)
d$Origin = (d$Origin - mean(d$Origin)) / sd(d$Origin)


# RQ: What kind of weighting function produces the best results for Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

#There are some interesting things to look at for EXAM25, EXAMF, hdMaxEXAM, hdMaxLOCEXAM and the confusion matrix

ls.df$precision = ls.df$tp / (ls.df$tp + ls.df$fp)
ls.df$recall = ls.df$tp / (ls.df$tp + ls.df$fn)
precision.intervals = mean(ls.df$precision) + (seq(-4, 4, 2) * sd(ls.df$precision))
precision.intervals
recall.intervals = mean(ls.df$recall) + (seq(-4, 4, 2) * sd(ls.df$recall))
recall.intervals
pdf("rq3-precision.pdf")

p1 = ggplot(ls.df, aes(x = precision)) +
  geom_density(color = "grey22")
foo = ggplot_build(p1)$data[[1]]
p1 = p1 +
  geom_area(
    data = subset(foo, x >= precision.intervals[2] & x <= precision.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(precision)), linetype = "solid", color = "grey32") +
  ggtitle("Density of Precision at 5% LOC cut-off", "with median, 2 sd interval")

p2 = ggplot(ls.df, aes(x = recall)) +
  geom_density(color = "grey22")
foo = ggplot_build(p2)$data[[1]]
p2 = p2 +
  geom_area(
    data = subset(foo, x >= recall.intervals[2] & x <= recall.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(recall)), linetype = "solid", color = "grey32") +
  ggtitle("Density of Recall at 5% LOC cut-off", "with median, 2 sd interval")

grid.arrange(p1, p2, ncol=1)
dev.off()



pdf("rq3-exam25-hdm.pdf")
ggplot(ls.df, aes(EXAM25, hdMaxLOCEXAM)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="grey22") +
  scale_y_log10() +
  scale_x_log10() +
  ggtitle("Correlation of EXAM 25 and hdMaxLOCEXAM", "log scale, with y=X line")
dev.off()



hdm.intervals = mean(ls.df$hdMaxLOCEXAM) + (seq(-4, 4, 2) * sd(ls.df$hdMaxLOCEXAM))
hdm.intervals
# pdf("rq3-hdm-density.pdf")
p1 = ggplot(ls.df, aes(x = hdMaxLOCEXAM)) +
  geom_density(color = "grey22") +
  xlim(0, hdm.intervals[4])
foo = ggplot_build(p1)$data[[1]]
p1 = p1 +
  geom_area(
    data = subset(foo, x >= hdm.intervals[2] & x <= hdm.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(hdMaxLOCEXAM)), linetype = "solid", color = "grey32") +
  ggtitle("Density of hdMaxLOCEXAM", "with median, 2 sd interval")

p2 = ggplot(ls.df, aes(x = hdMaxLOCEXAM)) +
  geom_density(color = "grey22") +
  xlim(0, 0.01)
foo = ggplot_build(p2)$data[[1]]
p2 = p2 +
  geom_area(
    data = subset(foo, x >= hdm.intervals[2] & x <= hdm.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(hdMaxLOCEXAM)), linetype = "solid", color="grey22") +
  ggtitle("Density of hdMaxLOCEXAM", "with median, zoomed x-axis")

grid.arrange(p1, p2, ncol=1)
dev.off()



ex25.intervals = mean(ls.df$EXAM25) + (seq(-4, 4, 2) * sd(ls.df$EXAM25))
ex25.intervals
pdf("rq3-exam25-density.pdf")
p = ggplot(ls.df, aes(x = EXAM25)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= ex25.intervals[2] & x <= ex25.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(EXAM25)), linetype = "solid", color = "grey32") +
  ggtitle("Density of EXAM25", "with median, 2 sd interval")
p
dev.off()



exf.intervals = mean(ls.df$EXAMF) + (seq(-4, 4, 2) * sd(ls.df$EXAMF))
exf.intervals
pdf("rq3-examf-density.pdf")
p1 = ggplot(ls.df, aes(x = EXAMF)) +
  geom_density(color = "grey22") +
  xlim(0, exf.intervals[5])
foo = ggplot_build(p1)$data[[1]]
p1 = p1 +
  geom_area(
    data = subset(foo, x >= exf.intervals[2] & x <= exf.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(EXAMF)), linetype = "solid", color = "grey32") +
  ggtitle("Density of EXAMF", "with median, 4 sd x-axis")

p2 = ggplot(ls.df, aes(x = EXAMF)) +
  geom_density(color = "grey22") +
  xlim(0, 0.0025)
foo = ggplot_build(p2)$data[[1]]
p2 =  p2 +
  geom_area(
    data = subset(foo, x >= exf.intervals[2] & x <= exf.intervals[4]), aes(x = x, y = y), fill = "grey", color="grey22") +
  geom_vline(aes(xintercept = median(EXAMF)), linetype = "solid", color = "grey32") +
  ggtitle("Density of EXAMF", "with median, zoomed x-axis")
  

grid.arrange(p1, p2, ncol=1)
dev.off()