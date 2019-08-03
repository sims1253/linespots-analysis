library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

SEED = 140919 # I asked my girlfriend for a number

setwd("~/Documents/dev/linespots/linespots-analysis/rq2")
load(file="m1.RData")
load(file="m2.RData")
load(file="m3.RData")
load(file="m4.RData")

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


# RQ: How does using the commit position instead of age for weighting, influence the performance of Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

# Lets look at some boxplots first
boxplot(EXAM ~ Time, data = ls.df)


boxplot(AUCECEXAM ~ Time, data = ls.df)


boxplot(EXAM25 ~ Time, data = ls.df)


# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
# The proposed predictors were:  LOC, Project, Origin
m1.1 = brm(
  formula = EXAM ~ 1 + Time,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + LOC,
  data = ls.df,
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
  formula = EXAM ~ 1 + Time + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m1.4 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

# Want to see if adding Language or Domain helps
m1.5 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Project) + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m1.6 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Project) + (1|Domain),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m1.7 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Project) + (1|Domain) + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)


loo1.1 = loo(m1.1)
loo1.2 = loo(m1.2)
loo1.3 = loo(m1.3)
loo1.4 = loo(m1.4)
loo1.5 = loo(m1.5)
loo1.6 = loo(m1.6)
loo1.7 = loo(m1.7)

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5, loo1.6, loo1.7)
# Using se_diff > 4*elpd_diff for a significantly better model, models 3 to 7 are
# essentially equally good.
# In that case, model 3 would be the preferred one as it has the fewest predictors.
# We can also keep model 7 for comparison.
# And while we see transitions exceeding tree depth, that is not a validity concern
# but only a efficiency concern.
summary(m1.3)
summary(m1.7)
# Model 7 seems to sample better. 3 has one rhat at 1.02

save(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, file="m1.RData")



# We repeat the same process for the AUCEC output.
# The proposed predictors were LOC, Project, Origin.
m2.1 = brm(
  formula = AUCECEXAM ~ 1 + Time,
  data = ls.df,
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
  formula = AUCECEXAM ~ 1 + Time + LOC,
  data = ls.df,
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
  formula = AUCECEXAM ~ 1 + Time + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.4 = brm(
  formula = AUCECEXAM ~ 1 + Time + Origin + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

# Test if Domain or Language improves the model

m2.5 = brm(
  formula = AUCECEXAM ~ 1 + Time + Origin + LOC + (1|Language) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.6 = brm(
  formula = AUCECEXAM ~ 1 + Time + Origin + LOC + (1|Domain) + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.7 = brm(
  formula = AUCECEXAM ~ 1 + Time + Origin + LOC + (1|Domain) + (1|Project) + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)


loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5)
loo2.6 = loo(m2.6)
loo2.7 = loo(m2.7)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6, loo2.7)
# Again models 3 to 7 have essentially the same loo performance.
# We again chose model 3 as it is the simplest one. We can keep model 7 as comparison.

summary(m2.3)
summary(m2.7)
# Again model 7 seems to sample better

save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, m2.7, file="m2.RData")

##########################################################
# Final models

# based on 1.3
m3.1 = brm(
  formula = EXAM ~ 1 + Time + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
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

# based on 1.7
m3.2 = brm(
  formula = EXAM ~ 1 + Time + LOC + Origin + (1|Project) + (1|Domain) + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
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
save(m3.1, m3.2, file="m3.RData")

loo_compare(loo(m3.1), loo(m3.2))

summary(m3.1)
summary(m3.2)
plot(m3.1)
plot(m3.2)

# Model 2 seems to sample a little better. Especially the Intercept.
# Regardless, both seem to 

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
# From what I understand, the sampling seems to work fine.

plot(hypothesis(m3.1, "Timelinear_Time_function=0"))
plot(hypothesis(m3.1, "Timeflat_Time_function=0"))


############################################################
# And do the same for the AUCEC

# Based on 2.3
m4.1 = brm(
  formula = AUCECEXAM ~ 1 + Time + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
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

# Based on 2.7
m4.2 = brm(
  formula = AUCECEXAM ~ 1 + Time + LOC + Origin + (1|Project) + (1|Domain) + (1|Language),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.5), class=sd),
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
save(m4.1, m4.2, file="m4.RData")

summary(m4.1)
summary(m4.2)
# Model 1 seems to sample a lot better but the Time coefficients are the same for both.
plot(m4.1)
plot(m4.2)

loo_compare(loo(m4.1), loo(m4.2))
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

plot(hypothesis(m3.2, "Timelinear_Time_function=0"))
plot(hypothesis(m3.2, "Timeflat_Time_function=0"))


save(m4.1, m4.2, file="m4.RData")
