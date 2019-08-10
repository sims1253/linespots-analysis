library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

SEED = 140919 # The day I move in with my gf

setwd("~/Documents/dev/linespots/linespots-analysis/rq4")
load(file="m1.RData")
load(file="m2.RData")
load(file="m3.RData")
load(file="m4.RData")
load(file="m5.RData")
load(file="m6.RData")

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


# RQ:  What is the prediction performance of Linespots??

# Lets look at some boxplots first
boxplot(EXAM ~ Algorithm, data = d)
# Linespots seems to produce lower (better) EXAM scores than Bugspots.

boxplot(AUCECEXAM ~ Algorithm, data = d)
# Linespots also seems to produce lower (worse) AUCEC sores than Bugspots.
# The difference seems smaller even when taking scaling of the plots
# into account. This is interesting, as the first boxplot indicated
# that linespots performs better while the second indicates that linespots
# performs worse.

# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
# The proposed predictors were LOC, Project, Origin, Language
m1.1 = brm(
  formula = EXAM ~ 1 + Algorithm,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m1.2 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)
m1.3 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m1.4 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project) + Origin,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m1.5 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project) + Origin + (1|Language),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

m1.6 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + Origin + (1|Project) + (1|Language) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.9999, max_treedepth=15),
  seed = SEED
)

loo1.1 = loo(m1.1)
loo1.2 = loo(m1.2)
loo1.3 = loo(m1.3)
loo1.4 = loo(m1.4)
loo1.5 = loo(m1.5)
loo1.6 = loo(m1.6)

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5, loo1.6)
save(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, file="m1.RData")
summary(m1.3)
summary(m1.5)


# We repeat the same process for the AUCEC output.
# The proposed predictors were: LOC, Project, Origin, Language
m2.1 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.2 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)
m2.3 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.4 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project) + Origin,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.5 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project) + Origin + (1|Language),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99, max_treedepth=15),
  seed = SEED
)

m2.6 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + Origin + (1|Project) + (1|Language) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5)
loo2.6 = loo(m2.6)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6)
summary(m2.3)
summary(m2.4)
summary(m2.5)


save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, file="m2.RData")


##########################################################
# Final models
# We can use the same priors as for rq1 and 2

# based on m1.3
m3.1 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.9999, max_treedepth=15),
  seed = SEED
)
summary(m3.1)
loo3.1 = loo(m3.1)

m3.2 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project) + (1|Language),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.9999, max_treedepth=15),
  seed = SEED
)
summary(m3.2)
loo3.2 = loo(m3.2)


loo_compare(loo3.1, loo3.2)
save(m3.1, m3.2, file="m3.RData")

###################################################################

# based on 2.3
m4.1 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.9999, max_treedepth = 15),
  seed = SEED
)
summary(m4.1)
loo4.1 = loo(m4.1)

m4.2 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project) + (1|Language),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.9999, max_treedepth = 15),
  seed = SEED
)
summary(m4.2)
loo4.2 = loo(m4.2)

loo_compare(loo4.1, loo4.2)

save(m4.1, m4.2, file="m4.RData")




##################################################################################

# Small trial for Exam25
# That would give LOC, Algorithm, Project, Language and maybe Domain as predictors.
# The mcmc_area plot also shows Origin and commit to have noticable effects.

m5.1 = brm(
  formula = EXAM25 ~ 1 + Algorithm + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo5.1 = loo(m5.1)

m5.2 = brm(
  formula = EXAM25 ~ 1 + Algorithm + LOC + (1|Project) + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo5.2 = loo(m5.2)

m5.3 = brm(
  formula = EXAM25 ~ 1 + Algorithm + LOC + (1|Project) + (1|Language) + (1|Domain) + Origin + Commits,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo5.3 = loo(m5.3)

loo_compare(loo5.1, loo5.2, loo5.3)
save(m5.1, m5.2, m5.3, file="m5.RData")


##################################################################################

# Small trial for EInstect25EXAM
# That would give Algorithm, Domain, Language, Project, LOC, Weighting as predictors.
# The mcmc_areas also makes Origin and commit seem usefull though.

m6.1 = brm(
  formula = EInspect25EXAM ~ 1 + Weighting + LOC + (1|Project),
  data = ls.df,
  family=poisson(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo6.1 = loo(m6.1)

m6.2 = brm(
  formula = EInspect25EXAM ~ 1 + Weighting + (1|Domain) + (1|Project) + (1|Language) + LOC,
  data = ls.df,
  family=poisson(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo6.2 = loo(m6.2)

m6.3 = brm(
  formula = EInspect25EXAM ~ 1 + Weighting + LOC + (1|Project) + (1|Language) + (1|Domain) + Origin + Commits,
  data = ls.df,
  family=poisson(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo6.3 = loo(m6.3)

loo_compare(loo6.1, loo6.2, loo6.3)
save(m6.1, m6.2, m6.3, file="m6.RData")
