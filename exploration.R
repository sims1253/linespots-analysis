library(tidyverse)

d = read_delim('combined.csv',
               delim = ",",
               locale = locale(decimal_mark = "."),
               col_names = TRUE,
               col_types = cols(
                 AUCEC = col_double(),
                 Algorithm = col_character(),
                 Commits = col_double(),
                 Depth = col_double(),
                 Domain = col_character(),
                 EXAM = col_double(),
                 FixCount = col_double(),
                 Fixed = col_double(),
                 Future = col_double(),
                 LOC = col_double(),
                 Missed = col_double(),
                 Origin = col_double(),
                 Project = col_character(),
                 Source = col_character(),
                 Choice = col_character(),
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

# Create data frames for only the Linespots and Bugspots data.
ls.df = subset(d, d$Algorithm == "Linespots")
bs.df = subset(d, d$Algorithm == "Bugspots")

# Let's look at the outcomes first. AUCEC and EXAM namely.
# The AUCEC can take on values between 0 and 1, with higher being better.
# A random process should produce an AUCEC of 0.5
hist(ls.df$AUCEC)
plot(density(ls.df$AUCEC))
# The hist and density plots show that the data is distributed normal-ish
# with two small bumps close to 0.1 and 0.4
# With the limitation between 0 and 1, a beta likelihood should
# be the right fit here.

# The EXAM score can also take on values between 0 and 1, with lower being better.
hist(ls.df$EXAM)
plot(density(ls.df$EXAM))
# The density plot looks like a multimodal normal distribution, while the
# histogram looks a little like a poisson.
# Again, with the limitation between 0 and 1, a beta likelihood
# should be the right choice.