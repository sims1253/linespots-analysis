library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
library(extraDistr)
options(mc.cores = parallel::detectCores())

SEED = 140919 # The day I move in with my gf

setwd("~/Documents/dev/linespots/linespots-analysis/rq1")
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

# RQ: What kind of weighting function produces the best results for Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")



sensitivity1 <- function(intercept, parameters, var_intercepts) {
  set.seed(140919)
  N = 100
  
  b_intercept = rnorm(N, 0.5, intercept)
  b_weighting = rnorm(N, 0, parameters)
  b_loc = rnorm(N, 0, parameters)
  m_project = rhcauchy(N, var_intercepts)
  
  b_project = rep(0, N)
  for (i in 1:N) {
    b_project[i] = rnorm(1, m_project, intercept)
  }
  
  for (w in 0:1){
    plot(NULL, xlim = c(-1, 5), ylim = c(0, 1))
    for (i in 1:N) {
      curve(b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_project[i],
      add = TRUE)
    }  
  }
  
}

sensitivity1(10, 10, 10)
sensitivity1(1, 1, 1)
sensitivity1(0.1, 0.1, 0.1)
sensitivity1(0.2, 0.05, 0.05)



mg.1 = brm(
  formula = EXAM ~ 1 + Weighting,
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

mg.2 = brm(
  formula = EXAM ~ 1 + Weighting + LOC,
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)
mg.3 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + (1|Project),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.05), class=sd),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

mg.31 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + (1|Project),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.4), class=Intercept),
    prior(normal(0, 0.1), class=b),
    prior(cauchy(0, 0.1), class=sd),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

mg.32 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + (1|Project),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.05), class=Intercept),
    prior(normal(0, 0.1), class=b),
    prior(cauchy(0, 0.1), class=sd),
    prior(cauchy(0, 0.05), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

mg.4 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.05), class=sd),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99),
  seed = SEED
)

# Want to see if adding Language or Domain helps
mg.5 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Language),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.05), class=sd),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999),
  seed = SEED
)

mg.6 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Domain),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.05), class=sd),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999),
  seed = SEED
)

mg.7 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Domain) + (1|Language),
  data = ls.df,
  family=gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class=Intercept),
    prior(normal(0, 0.05), class=b),
    prior(cauchy(0, 0.05), class=sd),
    prior(cauchy(0, 0.1), class=sigma)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999),
  seed = SEED
)


loog.1 = loo(mg.1)
loog.2 = loo(mg.2)
loog.3 = loo(mg.3)
loog.4 = loo(mg.4)
loog.5 = loo(mg.5)
loog.6 = loo(mg.6)
loog.7 = loo(mg.7)
loog.31 = loo(mg.31) 
loog.32 = loo(mg.32)

loo_compare(loog.1, loog.2, loog.3, loog.4, loog.5, loog.6, loog.7)
loo_compare(loo1.1, loo1.2, loo1.3, loo1.4, loo1.5, loo1.6, loo1.7, loog.1, loog.2, loog.3, loog.31, loog.32, loog.4, loog.5, loog.6, loog.7)


pp_check(mg.32)
pp_check(m1.3)

stanplot(m1.3, type="areas", pars = "b_")
stanplot(mg.32, type="areas", pars = "b_")
