library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

SEED = 4082 # I asked my girlfriend for a number

#setwd('.../linespots-analysis/rq4')
#load(file="m1.RData")
#load(file="m2.RData")
#load(file="m3.RData")

d = read_delim('../data.csv',
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
                 EXAM = col_double(),
                 EXAM25 = col_double(),
                 EXAM33 = col_double(),
                 EXAM50 = col_double(),
                 FixCount = col_double(),
                 Fixed = col_double(),
                 FullExam = col_guess(),
                 Future = col_double(),
                 ID = col_factor(),
                 LOC = col_double(),
                 Missed = col_double(),
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

# Standardizing
d$Commits = (d$Commits - mean(d$Commits)) / sd(d$Commits)
d$LOC = (d$LOC - mean(d$LOC)) / sd(d$LOC)
d$Origin = (d$Origin - mean(d$Origin)) / sd(d$Origin)


# RQ:  What is the prediction performance of Linespots??

# Lets look at some boxplots first
boxplot(EXAM ~ Algorithm, data = d)
# Linespots seems to produce lower (better) EXAM scores than Bugspots.

boxplot(AUCEC ~ Algorithm, data = d, outline=FALSE)
# Linespots also seems to produce lower (worse) AUCEC sores than Bugspots.
# The difference seems smaller even when taking scaling of the plots
# into account. This is interesting, as the first boxplot indicated
# that linespots performs better while the second indicates that linespots
# performs worse.

# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
# The proposed predictors were Algorithm, LOC, Commits, Choice, Source
m1.1 = brm(
  formula = EXAM ~ 1 + Algorithm,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.2 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)
m1.3 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + Commits,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.4 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + Commits + (1|Choice),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.5 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + Commits + (1|Choice) + (1|Source),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

# My intuition tells me that an intercept per project might work well so
# I try some models including that even though projpred didn't propose it
m1.6 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.7 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project) + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.8 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project) + LOC + Commits,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

# Do the same for the domain
m1.9 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.10 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Domain) + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.11 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Domain) + LOC + Commits,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.12 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Domain) + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.13 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Domain) + (1|Project) + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.14 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Domain) + (1|Project) + LOC + Commits,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

loo1.1 = loo(m1.1)
loo1.2 = loo(m1.2)
loo1.3 = loo(m1.3)
loo1.4 = loo(m1.4)
loo1.5 = loo(m1.5)
loo1.6 = loo(m1.6)
loo1.7 = loo(m1.7)
loo1.8 = loo(m1.8)
loo1.9 = loo(m1.9)
loo1.10 = loo(m1.10)
loo1.11 = loo(m1.11)
loo1.12 = loo(m1.12)
loo1.13 = loo(m1.13)
loo1.14 = loo(m1.14)

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5, loo1.6, loo1.7, loo1.8, loo1.9, loo1.10, loo1.11, loo1.12, loo1.13, loo1.14)
loo_compare(loo1.6, loo1.7, loo1.8)

# save(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, m1.10, m1.11, m1.12, m1.13, m1.14, file="m1.RData")

# As m1.14 and m1.13 are a lot more complex than m1.8 and m1.7, we ignore them
# based on the similar loo performance as the simpler models.
# As m1.7 is simpler than m1.8 we also ignore m1.8 as it performs virtually the
# same as m1.7. We end up with m1.6 and m1.7 that perform similar, which would give
# m1.6 the benefit as it is the simpler model. But we will investigate both,
# As a better prior choice might change the difference.

summary(m1.6)
summary(m1.7)
# m1.7 actually seems to sample better than m1.6 but that might change
# with better priors.


stanplot(m1.6, type="hist")
stanplot(m1.6, type="dens_overlay")
stanplot(m1.6, type="areas")
stanplot(m1.6, type="areas", pars="b_")
# This looks like Linespots produces significantly better (lower is better) EXAM scores than Bugspots here.
stanplot(m1.6, type="areas", pars="sd_")

stanplot(m1.7, type="hist")
stanplot(m1.7, type="dens_overlay")
stanplot(m1.7, type="areas")
stanplot(m1.7, type="areas", pars="b_")
# This looks like Linespots produces significantly better (lower is better) EXAM scores than Bugspots here.
stanplot(m1.7, type="areas", pars="sd_")
# They both seem to predict very similar differences bettwen Linespots and Bugspots.


# We repeat the same process for the AUCEC output.
# The proposed predictors were: Domain, Origin, Algorithm, LOC, Choice, Commits
m2.1 = brm(
  formula = AUCEC ~ 1 + Algorithm,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.2 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)
m2.3 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Domain) + Origin,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.4 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Domain) + Origin + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.5 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Domain) + Origin + LOC + (1|Choice),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.6 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Domain) + Origin + LOC + (1|Choice) + Commits,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

# My intuition tells me that an intercept per project might work well so
# I try some models including that even though projpred didn't propose it
m2.7 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.7 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.8 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain) + Origin,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.9 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain) + Origin + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.10 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain) + Origin + LOC + (1|Choice),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.11 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain) + Origin + LOC + (1|Choice) + Commits,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

# I added these last three models as they are similar to the successfull ones in rq1 and rq2
m2.12 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + Origin,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.13 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + Origin + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.14 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5)
loo2.6 = loo(m2.6)
loo2.7 = loo(m2.7)
loo2.8 = loo(m2.8)
loo2.9 = loo(m2.9)
loo2.10 = loo(m2.10)
loo2.11 = loo(m2.11)
loo2.12 = loo(m2.12)
loo2.13 = loo(m2.13)
loo2.14 = loo(m2.14)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6, loo2.7,
            loo2.8, loo2.9, loo2.10, loo2.11, loo2.12, loo2.13, loo2.14)
# m2.8 is the simplest model for all the models that perform equally well
# so we choose it.

#save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, m2.7, m2.8, m2.9, m2.10, m2.11, m2.12, m2.13, m2.14, file="m2.RData")

summary(m2.8)

stanplot(m2.8, type="hist")
stanplot(m2.8, type="dens_overlay")
stanplot(m2.8, type="areas")
stanplot(m2.8, type="areas", pars="b_")
# Looks like Linespots does produce better AUCEC scores than Bugspots
# but not significantly
stanplot(m2.8, type="areas", pars="sd_")


##########################################################
# Final models

# Prior Sensitivity analysis in psa_4.R
m3.1 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  sample_prior = TRUE,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

# Here we can use the same priors as for rq1 and rq2
m3.2 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project) + LOC,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  sample_prior = TRUE,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

# Prior Sensitivity analysis in psa_4.R
m3.3 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain) + Origin,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  sample_prior = TRUE,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

save(m3.1, m3.2, m3.3, file="m3.RData")
