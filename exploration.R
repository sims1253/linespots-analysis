library(tidyverse)

d = read_delim('data.csv',
               delim = ",",
               locale = locale(decimal_mark = "."),
               col_names = TRUE,
               col_types = cols(
                 AUCEC = col_double(),
                 Algorithm = col_factor(),
                 Choice = col_factor(),
                 Commits = col_double(),
                 Depth = col_double(),
                 Domain = col_factor(),
                 EInspect10 = col_double(),
                 EInspect25 = col_double(),
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
                 Origin = col_double(),
                 Project = col_factor(),
                 Source = col_factor(),
                 Time = col_factor(),
                 Weighting = col_factor(),
                 fn = col_double(),
                 fp = col_double(),
                 hdMax = col_double(),
                 hdMaxLOC = col_double(),
                 tn = col_double(),
                 tp = col_double()
               )
)

# Unneccesarry for reproduction
d$hdMaxLOC = d$hdMaxLOC / d$LOC


# Standardizing
d$Commits = (d$Commits - mean(d$Commits)) / sd(d$Commits)
d$LOC = (d$LOC - mean(d$LOC)) / sd(d$LOC)
d$Origin = (d$Origin - mean(d$Origin)) / sd(d$Origin)

# Create data frames for only the Linespots and Bugspots data.
ls.df = subset(d, d$Algorithm == "Linespots")
bs.df = subset(d, d$Algorithm == "Bugspots")

# Let's look at the outcomes first. AUCEC and EXAM namely.
# The AUCEC can take on values between 0 and 1, with higher being better.
# A random process should produce an AUCEC of 0.5
hist(ls.df$AUCEC)
plot(density(ls.df$AUCEC))
# The hist and density plots show that the data is distributed normal-ish
# with two small bumps close to 0.75 and 0.85 so maybe 2 gaussians overlayed.
# With the limitation between 0 and 1, a beta likelihood should
# be the right fit here.

# The EXAM score can also take on values between 0 and 1, with lower being better.
hist(ls.df$EXAM)
plot(density(ls.df$EXAM))
# Again it looks like there are two gaussians one with a peak at 0.15 and one at 0.25
# Again, with the limitation between 0 and 1, a beta likelihood
# should be the right choice.