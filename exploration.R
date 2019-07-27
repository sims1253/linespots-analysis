library(tidyverse)
library(dplyr)

#setwd("~/Documents/dev/linespots/linespots-analysis/")

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

# Standardizing
d$Commits = (d$Commits - mean(d$Commits)) / sd(d$Commits)
d$LOC = (d$LOC - mean(d$LOC)) / sd(d$LOC)
d$Origin = (d$Origin - mean(d$Origin)) / sd(d$Origin)

# There are results from one run, where no fix was found in the pseudo future.
# There is no value in that data so we remove it.
zero_entries = subset(d, d$AUCECDENSITY == 0)
zero_entries$FixCount
d = subset(d, d$FixCount != 0)

# Create data frames for only the Linespots and Bugspots data.
ls.df = subset(d, d$Algorithm == "Linespots")
bs.df = subset(d, d$Algorithm == "Bugspots")

summary(ls.df$AUCECEXAM)
summary(ls.df$EXAM)
summary(bs)
# Let's look at the outcomes first. AUCEC and EXAM namely.
# The AUCEC can take on values between 0 and 1, with higher being better.
# A random process should produce an AUCEC of 0.5
hist(ls.df$AUCECEXAM)
plot(density(ls.df$AUCECEXAM))
# The hist and density plots show that the data is distributed normal-ish
# with two small bumps close to 0.75 and 0.8 so maybe 2 gaussians overlayed.
# With the limitation between 0 and 1, a beta likelihood should
# be the right fit here.

# The EXAM score can also take on values between 0 and 1, with lower being better.
hist(ls.df$EXAM)
plot(density(ls.df$EXAM))
# Again it looks like there are two gaussians one with a peak at 0.2 and one at 0.3
# Again, with the limitation between 0 and 1, a beta likelihood
# should be the right choice.