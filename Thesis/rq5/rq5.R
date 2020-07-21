library(tidyverse)
library(ggthemes)
library(gridExtra)
ggplot2::theme_set(theme_tufte())
bayesplot::color_scheme_set("darkgray")
options(mc.cores = parallel::detectCores())

SEED = 140919 # The day I move in with my gf

setwd("~/Documents/dev/linespots/linespots-analysis/rq5")

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


# RQ: 

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

# MRD, SRD, WCP, WCW
wcp.intervals = mean(ls.df$WCP) + (seq(-4, 4, 2) * sd(ls.df$WCP))
wcp.intervals
wcw.intervals = mean(ls.df$WCW) + (seq(-4, 4, 2) * sd(ls.df$WCW))
wcw.intervals
pdf("rq5-wcp-wcw.pdf")
p = ggplot(ls.df, aes(x = WCP)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p1 = p +
  geom_area(
    data = subset(foo, x >= wcp.intervals[2] & x <= wcp.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(WCP)), linetype = "solid", color = "grey32") +
  ggtitle("Density of WCP", "with median, 2 sd interval")

p = ggplot(ls.df, aes(x = WCW)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p2 = p +
  geom_area(
    data = subset(foo, x >= wcw.intervals[2] & x <= wcw.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(WCW)), linetype = "solid", color = "grey32") +
  ggtitle("Density of WCW", "with median, 2 sd interval")

grid.arrange(p1, p2, ncol=1)
dev.off()

pdf("rq5-mrd.pdf")
mrd.intervals = mean(ls.df$MRD) + (seq(-4, 4, 2) * sd(ls.df$MRD))
mrd.intervals
p = ggplot(ls.df, aes(x = MRD)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= mrd.intervals[2] & x <= mrd.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(MRD)), linetype = "solid", color = "grey32") +
  ggtitle("Density of MRD", "with median, 2 sd interval")
p
dev.off()

