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
    prior(normal(0,0.05), class=b),
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
    prior(normal(0,0.05), class=b),
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
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
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
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
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
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999),
  seed = SEED
)

m1.6 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + Origin + (1|Project) + (1|Language) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.01), class=b),
    prior(cauchy(0,0.01), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.9999),
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


# We repeat the same process for the AUCEC output.
# The proposed predictors were: LOC, Project, Origin, Language
m2.1 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm,
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
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
    prior(normal(0,0.05), class=b),
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
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
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
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
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
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.05), class=b),
    prior(cauchy(0,0.05), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

m2.6 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + Origin + (1|Project) + (1|Language) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.01), class=b),
    prior(cauchy(0,0.01), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999),
  seed = SEED
)

loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5)
loo2.6 = loo(m2.6)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6)
summary(m2.6)


save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, file="m2.RData")


##########################################################
# Final models
# We can use the same priors as for rq1 and 2

# based on m1.3
m3.1 = brm(
  formula = EXAM ~ 0 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(1,0.05), class=b),
    prior(cauchy(0,0.5), class=sd),
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

save(m3.1, file="m3.RData")

stanplot(m3.1, type="areas", pars="b_Algorithm")
post1 = posterior_samples(m3.1)
post1$diff_algo = inv_logit_scaled(post1$b_AlgorithmLinespots) - inv_logit_scaled(post1$b_AlgorithmBugspots)
plot(density(post1$diff_algo))
abline(v = quantile(post1$diff_algo, c(0.025, 0.975))[1])
abline(v = quantile(post1$diff_algo, c(0.025, 0.975))[2])
abline(v = median(post1$diff_algo), lty = "dotted")

###################################################################

# based on 2.3
m4.1 = brm(
  formula = AUCECEXAM ~ 0 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(1,0.05), class=b),
    prior(cauchy(0,0.5), class=sd),
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

save(m4.1, file="m4.RData")




#################################################################

# As a final test

m5.1 = brm(
  formula = EXAM25 ~ 0 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(1,0.05), class=b),
    prior(cauchy(0,0.5), class=sd),
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

stanplot(m5.1, type="areas", pars="b_Algorithm")
post5 = posterior_samples(m5.1)
post5$diff_algo = inv_logit_scaled(post5$b_AlgorithmLinespots) - inv_logit_scaled(post5$b_AlgorithmBugspots)
plot(density(post5$diff_algo, adjust = 0.1))
abline(v = quantile(post5$diff_algo, c(0.025, 0.975))[1])
abline(v = quantile(post5$diff_algo, c(0.025, 0.975))[2])
abline(v = median(post5$diff_algo), lty = "dotted")

save(m5.1, m5.2, m5.3, file="m5.RData")

m5.2 = brm(
  formula = EXAM25 ~ 0 + Algorithm  + Weighting + Time + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(1,0.05), class=b),
    prior(cauchy(0,0.5), class=sd),
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

m5.3 = brm(
  formula = EXAM25 ~ 0 + Algorithm  + Weighting + Time + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(1,0.05), class=b),
    prior(cauchy(0,0.5), class=sd),
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
