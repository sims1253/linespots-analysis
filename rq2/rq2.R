library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

SEED = 4082 # I asked my girlfriend for a number

#setwd('.../linespots-analysis/rq2')
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
                 Time = col_factor(),
                 fn = col_double(),
                 fp = col_double(),
                 hdMax = col_double(),
                 hdMaxLOC = col_double(),
                 tn = col_double(),
                 tp = col_double()
               )
)

# Standardizing
d$Commits = scale(d$Commits)
d$LOC = scale(d$LOC)
d$Origin = scale(d$Origin)


# RQ: What kind of Time function produces the best results for Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

# Lets look at some boxplots first
boxplot(EXAM ~ Time, data = ls.df)
# This looks almost exactly the same for both

boxplot(AUCEC ~ Time, data = ls.df, outline=FALSE)
# Removing the outliers to get a closer look.
# Again, looks exactly the same for both.


# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
# The proposed predictors were: Origin, LOC, Choice, Source
m1.1 = brm(
  formula = EXAM ~ 1 + Time,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + Origin,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + Origin + LOC,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + Origin + LOC + (1|Choice),
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + Origin + LOC + (1|Choice) + (1|Source),
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + (1|Project),
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + (1|Project) + Origin,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + (1|Project) + LOC,
  data = ls.df,
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

m1.9 = brm(
  formula = EXAM ~ 1 + Time + (1|Project) + Origin + LOC,
  data = ls.df,
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
m1.10 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain),
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + (1|Domain) + (1|Project),
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + (1|Domain) + Origin,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + (1|Domain) + (1|Project) + Origin,
  data = ls.df,
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

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5, loo1.6, loo1.7, loo1.8, loo1.9, loo1.10, loo1.11, loo1.12, loo1.13)

# save(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, m1.10, m1.11, m1.12, m1.13, file="m1.RData")

summary(m1.7)

# As 9, 13 and 7 are almost identical in loo performance, we choose 7
# as it is the simplest model.


stanplot(m1.7, type="hist")
stanplot(m1.7, type="dens_overlay")
stanplot(m1.7, type="areas")
stanplot(m1.7, type="areas", pars="b_")
# This looks like time is significantly better than commit for producing lower EXAM scores.
stanplot(m1.7, type="areas", pars="sd_")

# We repeat the same process for the AUCEC output.
m2.1 = brm(
  formula = AUCEC ~ 1 + Time,
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + Origin,
  data = ls.df,
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
m2.3 = brm(
  formula = AUCEC ~ 1 + Time + Origin + LOC,
  data = ls.df,
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

m2.4 = brm(
  formula = AUCEC ~ 1 + Time + Origin + LOC + (1|Choice),
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + Origin + LOC + (1|Choice) + (1|Source),
  data = ls.df,
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
m2.6 = brm(
  formula = AUCEC ~ 1 + Time + (1|Project),
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + (1|Project) + Origin,
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + (1|Project) + LOC,
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + (1|Project) + Origin + LOC,
  data = ls.df,
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
m2.10 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain),
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + (1|Domain) + (1|Project),
  data = ls.df,
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

m2.12 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain) + Origin,
  data = ls.df,
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
  formula = AUCEC ~ 1 + Time + (1|Domain) + (1|Project) + Origin,
  data = ls.df,
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
loo2.9 = loo(m2.9, reloo = TRUE)
loo2.10 = loo(m2.10)
loo2.11 = loo(m2.11)
loo2.12 = loo(m2.12)
loo2.13 = loo(m2.13)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6, loo2.7, loo2.8, loo2.9, loo2.10, loo2.11, loo2.12, loo2.13)
#save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, m2.7, m2.8, m2.9, m2.10, m2.11, m2.12, m2.13, file="m2.RData")

summary(m2.7)
# While m2.9 is better than m2.7, the se_diff is more than half of the elpd_diff
# m2.7 is also the simpler model with one less predictor.
# Based on this, we chose m2.7 as the model to investigate further.


##########################################################
# Final models
# As the we are using a model of the same structure as rq1,
# we use the same priors as for rq1

m3.1 = brm(
  formula = EXAM ~ 1 + Time + (1|Project) + Origin,
  data = ls.df,
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
summary(m3.1)
plot(m3.1)

stanplot(m3.1, type="hist")
stanplot(m3.1, type="dens_overlay")
stanplot(m3.1, type="areas")
stanplot(m3.1, type="areas", pars="b_")
stanplot(m3.1, type="areas", pars="sd_")

np <- nuts_params(m3.1)
lp <- log_posterior(m3.1)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)
# From what I understand, the sampling seems to barely be ok.
# The stanplot shows, that the time based age gives significantly
# better EXAM scores (lower is better) than the commit based one.


############################################################
# And do the same for the AUCEC

m3.2 = brm(
  formula = AUCEC ~ 1 + Time + (1|Project) + Origin,
  data = ls.df,
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
summary(m3.2)
plot(m3.2)

stanplot(m3.2, type="hist")
stanplot(m3.2, type="dens_overlay")
stanplot(m3.2, type="areas")
stanplot(m3.2, type="areas", pars="b_")
stanplot(m3.2, type="areas", pars="sd_")

np <- nuts_params(m3.2)
lp <- log_posterior(m3.2)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)
# Again, from what I understand, the sampling seems to be barely ok.
# The stanplot shows, that the time based age version gives significantly
# better AUCEC scores (higher is better).

#save(m3.1, m3.2, file="m3.RData")
