library(tidyverse)
library(ggthemes)
library(gridExtra)
ggplot2::theme_set(theme_tufte())

setwd("~/Documents/dev/linespots/linespots-analysis")

d = read_delim('data.csv',
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

exam.intervals = mean(d$EXAM) + (seq(-4, 4, 2) * sd(d$EXAM))
exam.intervals
pdf("exp-exam.pdf")
p = ggplot(d, aes(x = EXAM)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= exam.intervals[2] & x <= exam.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(EXAM)), linetype = "solid", color = "grey32") +
  ggtitle("Density of EXAM", "with median, 2 sd interval")
p
dev.off()

aucecexam.intervals = mean(d$AUCECEXAM) + (seq(-4, 4, 2) * sd(d$AUCECEXAM))
aucecexam.intervals
pdf("exp-aucecexam.pdf")
p = ggplot(d, aes(x = AUCECEXAM)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= aucecexam.intervals[2] & x <= aucecexam.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(AUCECEXAM)), linetype = "solid", color = "grey32") +
  ggtitle("Density of AUCECEXAM", "with median, 2 sd interval")
p
dev.off()

aucecdens.intervals = mean(d$AUCECDENSITY) + (seq(-4, 4, 2) * sd(d$AUCECDENSITY))
aucecdens.intervals
pdf("exp-aucecdens.pdf")
p = ggplot(d, aes(x = AUCECDENSITY)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= aucecdens.intervals[2] & x <= aucecdens.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(AUCECDENSITY)), linetype = "solid", color = "grey32") +
  ggtitle("Density of AUCECDENSITY", "with median, 2 sd interval")
p
dev.off()

d = subset(d, d$Algorithm == "Linespots")

pdf("exp-box-lang.pdf")
p1 = ggplot(d, aes(x=Language, y=EXAM)) + geom_boxplot()
p2 = ggplot(d, aes(x=Language, y=AUCECEXAM)) + geom_boxplot()
grid.arrange(p1, p2, ncol=1)
dev.off()

pdf("exp-box-domain.pdf")
p1 = ggplot(d, aes(x=Domain, y=EXAM)) + geom_boxplot()
p2 = ggplot(d, aes(x=Domain, y=AUCECEXAM)) + geom_boxplot()
grid.arrange(p1, p2, ncol=1)
dev.off()

fix.intervals = mean(d$FixCount) + (seq(-4, 4, 2) * sd(d$FixCount))
fix.intervals
pdf("exp-fix.pdf")
p = ggplot(d, aes(x = FixCount)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= fix.intervals[2] & x <= fix.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(FixCount)), linetype = "solid", color = "grey32") +
  ggtitle("Density of FixCount", "with median, 2 sd interval")
p
dev.off()

loc.intervals = mean(d$LOC) + (seq(-4, 4, 2) * sd(d$LOC))
loc.intervals
pdf("exp-loc.pdf")
p = ggplot(d, aes(x = LOC)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= loc.intervals[2] & x <= loc.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(LOC)), linetype = "solid", color = "grey32") +
  ggtitle("Density of LOC", "with median, 2 sd interval")
p
dev.off()

commit.intervals = mean(d$Commits) + (seq(-4, 4, 2) * sd(d$Commits))
commit.intervals
pdf("exp-commit.pdf")
p = ggplot(d, aes(x = Commits)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= commit.intervals[2] & x <= commit.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(Commits)), linetype = "solid", color = "grey32") +
  ggtitle("Density of Commits", "with median, 2 sd interval")
p
dev.off()

origin.intervals = mean(d$Origin) + (seq(-4, 4, 2) * sd(d$Origin))
origin.intervals
pdf("exp-origin.pdf")
p = ggplot(d, aes(x = Origin)) +
  geom_density(color = "grey22")
foo = ggplot_build(p)$data[[1]]
p = p +
  geom_area(
    data = subset(foo, x >= origin.intervals[2] & x <= origin.intervals[4]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = median(Origin)), linetype = "solid", color = "grey32") +
  ggtitle("Density of Origin", "with median, 2 sd interval")
p
dev.off()
