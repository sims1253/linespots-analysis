library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

#setwd('.../linespots-analysis')

d = read_delim('../data.csv',
               delim = ",",
               locale = locale(decimal_mark = "."),
               col_names = TRUE,
               col_types = cols(
                 AUCEC = col_double(),
                 Algorithm = col_factor(),
                 Commits = col_double(),
                 Depth = col_double(),
                 Domain = col_factor(),
                 EXAM = col_double(),
                 FixCount = col_double(),
                 Fixed = col_double(),
                 Future = col_double(),
                 LOC = col_double(),
                 Missed = col_double(),
                 Origin = col_double(),
                 Project = col_factor(),
                 Source = col_factor(),
                 Choice = col_factor(),
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
m1.1 = brm(
  formula = EXAM ~ 1,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
)

m1.2 = brm(
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
)

m1.3 = brm(
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
)

m1.4 = brm(
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
)

m1.5 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project) + (1|Domain),
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
)

loo1.1 = loo(m1.1)
loo1.2 = loo(m1.2)
loo1.3 = loo(m1.3)
loo1.4 = loo(m1.4)
loo1.5 = loo(m1.5)

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5)

summary(m1.3)
summary(m1.5)

# Both m1.3 and m1.5 have trouble sampling but m1.5 also has divergent transitions.
# Performance is similar so we choose the simpler model, m1.3


stanplot(m1.3, type="hist")
stanplot(m1.3, type="dens_overlay")
stanplot(m1.3, type="areas")
stanplot(m1.3, type="areas", pars="b_")
# Looks like Linespots is performing significantly better.
stanplot(m1.3, type="areas", pars="sd_")

# We repeat the same process for the AUCEC output.
m2.1 = brm(
  formula = AUCEC ~ 1,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
)

m2.2 = brm(
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
)

m2.3 = brm(
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
)

m2.4 = brm(
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
)

m2.5 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Domain) + (1|Project),
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
)

loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5)

summary(m2.3)
summary(m2.5)

# Both m1.3 and m1.5 have trouble sampling but m1.5 also has divergent transitions.
# Performance is similar so we choose the simpler model, m1.3.


stanplot(m1.3, type="hist")
stanplot(m1.3, type="dens_overlay")
stanplot(m1.3, type="areas")
stanplot(m1.3, type="areas", pars="b_")
# Looks like Linespots is performing significantly worse here.
stanplot(m1.3, type="areas", pars="sd_")


# Sensitivity analysis
# TODO
m3.1 = brm(
  formula = AUCEC ~ 1 + Algorithm + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,5), class=Intercept), 
    prior(normal(0,5), class=b),
    prior(cauchy(0,5), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
)