library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

SEED = 2407 # I asked my girlfriend for a number

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


# RQ: What kind of Time function produces the best results for Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

# Lets look at some boxplots first
boxplot(EXAM ~ Time, data = ls.df)
# This looks almost exactly the same for both

boxplot(AUCEC ~ Time, data = ls.df)
# Again, looks exactly the same for both.


# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
# The proposed predictors were: LOC, Origin, Language, Project
m1.1 = brm(
  formula = EXAM ~ 1 + Time,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.2 = brm(
  formula = EXAM ~ 1 + Time + LOC,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)
m1.3 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.4 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.5 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

# My intuition tells me that an intercept per project might work well so
# I try some models including that even though projpred didn't propose it
m1.6 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.7 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain) + LOC,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.8 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain) + Origin,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.9 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain) + LOC + Origin,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.10 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain) + LOC + Origin + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m1.11 = brm(
  formula = EXAM ~ 1 + Time + (1|Domain) + LOC + Origin + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
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

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5, loo1.6, loo1.7, loo1.8, loo1.9, loo1.10, loo1.11)

#save(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, m1.10, m1.11, file="m1.RData")

summary(m1.5)

# As 5 and 11 are almost identical in loo performance, we choose 5
# as it is the simpler model.


stanplot(m1.5, type="hist")
stanplot(m1.5, type="dens_overlay")
stanplot(m1.5, type="areas")
stanplot(m1.5, type="areas", pars="b_")
# This looks like time is significantly better than commit for producing lower EXAM scores.
stanplot(m1.5, type="areas", pars="sd_")

# We repeat the same process for the AUCEC output.
m2.1 = brm(
  formula = AUCEC ~ 1 + Time,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(10, 10), class=phi)
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
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(10, 10), class=phi)
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
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.4 = brm(
  formula = AUCEC ~ 1 + Time + LOC + Origin + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.5 = brm(
  formula = AUCEC ~ 1 + Time + LOC + Origin + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

# My intuition tells me that an intercept per project might work well so
# I try some models including that even though projpred didn't propose it
m2.6 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.7 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain) + Origin,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.8 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain) + LOC,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.9 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain) + LOC + Origin,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.10 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain) + LOC + Origin + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  seed = SEED
)

m2.11 = brm(
  formula = AUCEC ~ 1 + Time + (1|Domain) + LOC + Origin + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(10, 10), class=phi)
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


loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6, loo2.7, loo2.8, loo2.9, loo2.10, loo2.11)
#save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, m2.7, m2.8, m2.9, m2.10, m2.11, file="m2.RData")

summary(m2.5)
# As 5 and 11 are almost identical in loo performance, we choose 5
# as it is the simpler model.

stanplot(m2.5, type="hist")
stanplot(m2.5, type="dens_overlay")
stanplot(m2.5, type="areas")
stanplot(m2.5, type="areas", pars="b_")
# This looks like time is significantly better than commit for producing higher AUCEC scores.
stanplot(m2.5, type="areas", pars="sd_")


##########################################################
# Final models
# As the we are using a model of the same structure as rq1,
# we use the same priors as for rq1

m3.1 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(10, 10), class=phi)
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999),
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
# From what I understand, the sampling seems to be good.
# The time based version is part of the intercept. Moving to the commit based version
# does not influence the EXAM from what I understand.


############################################################
# And do the same for the AUCEC

m3.2 = brm(
  formula = AUCEC ~ 1 + Time + Origin + LOC + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,10), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(10, 10), class=phi)
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
# Again, from what I understand, the sampling seems to be good.
# The time based version is part of the intercept. Moving to the commit based version
# does not influence the AUCEC from what I understand.

#save(m3.1, m3.2, file="m3.RData")
