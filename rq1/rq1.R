library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())

#setwd('.../linespots-analysis/rq1')

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


# RQ: What kind of weighting function produces the best results for Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

# Lets look at some boxplots first
boxplot(EXAM ~ Weighting, data = ls.df)
# This looks like all three weighting functions have very similar results
# with the linear one having a slightly lower median than the other two.

boxplot(AUCEC ~ Weighting, data = ls.df, outline=FALSE)
# Removing the outliers to get a closer look.
# All three weighting functions look to be virtually the same in terms
# of AUCEC here.


# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
m1.1 = brm(
  formula = EXAM ~ 1,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
)

m1.2 = brm(
  formula = EXAM ~ 1 + Weighting,
  data = ls.df,
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
  formula = EXAM ~ 1 + Weighting + (1|Project),
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
)

m1.4 = brm(
  formula = EXAM ~ 1 + Weighting + (1|Domain),
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
)

m1.5 = brm(
  formula = EXAM ~ 1 + Weighting + (1|Project) + (1|Domain),
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
)

loo1.1 = loo(m1.1)
loo1.2 = loo(m1.2)
loo1.3 = loo(m1.3)
loo1.4 = loo(m1.4)
loo1.5 = loo(m1.5)

loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5)

summary(m1.3)
summary(m1.5)

# While m1.3 is the simpler model, it has troubles sampling (Eff.Sample).
# That might be fixable with better priors and we generally prefer simpler
# models so m1.3 is our favourite.


stanplot(m1.3, type="hist")
stanplot(m1.3, type="dens_overlay")
stanplot(m1.3, type="areas")
stanplot(m1.3, type="areas", pars="b_")
# This looks like the google weighting function produces better EXAM scores (lower is better),
# and the difference seems to be significant.
stanplot(m1.3, type="areas", pars="sd_")


# We repeat the same process for the AUCEC output.
m2.1 = brm(
  formula = AUCEC ~ 1,
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
)

m2.2 = brm(
  formula = AUCEC ~ 1 + Weighting,
  data = ls.df,
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
  formula = AUCEC ~ 1 + Weighting + (1|Project),
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
)

m2.4 = brm(
  formula = AUCEC ~ 1 + Weighting + (1|Domain),
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
)

m2.5 = brm(
  formula = AUCEC ~ 1 + Weighting + (1|Domain) + (1|Project),
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
)

loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5, reloo = TRUE)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5)

summary(m2.3)
summary(m2.5)
# Both m2.3 and m2.5 have smapling problems (Eff.Sample) but based
# on loo performance and prefering the simpler model, m2.3 would be
# the winner here.

# Sensitivity analysis
# TODO
m3.1 = brm(
  formula = AUCEC ~ 1 + Weighting + (1|Project),
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
