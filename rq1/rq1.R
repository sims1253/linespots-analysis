library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
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

# Lets look at some boxplots first
boxplot(EXAM ~ Weighting, data = ls.df)
# All three weighting functions seem to produce very similar EXAM scores.
# The flat one seems to have the lowest median slightly.

boxplot(AUCECEXAM ~ Weighting, data = ls.df)
# The same picture for the AUCEC scores. The flat weighting function seems
# to produce the highest ones just slightly.

# We start with the EXAM metric as output.
# Starting with the simplest model becoming more complex.
# The proposed predictors were:  LOC, Project, Origin
m1.1 = brm(
  formula = EXAM ~ 1 + Weighting,
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
  formula = EXAM ~ 1 + Weighting + LOC,
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
  formula = EXAM ~ 1 + Weighting + LOC + (1|Project),
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
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project),
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
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Language),
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
  control = list(adapt_delta=0.999),
  seed = SEED
)

m1.6 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Domain),
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
  control = list(adapt_delta=0.999),
  seed = SEED
)

m1.7 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Domain) + (1|Language),
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
  control = list(adapt_delta=0.999),
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

# Using se_diff > 2*elpd_diff for a significantly better model, models 3 to 7 are
# essentially equally good. Model 4 is right on the 2* mark.
# In this case, model 3 would be the preferred one as it has the fewest predictors.
# But as 3 has sampling problems, we will use the simplest model that also samples
# properly, which is model 7.
# And while we see transitions exceeding tree depth, that is not a validity concern
# but only a efficiency concern.

save(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, file="m1.RData")



# We repeat the same process for the AUCEC output.
# The proposed predictors were LOC, Project, Origin.
m2.1 = brm(
  formula = AUCECEXAM ~ 1 + Weighting,
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
  formula = AUCECEXAM ~ 1 + Weighting + LOC,
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
  formula = AUCECEXAM ~ 1 + Weighting + LOC + (1|Project),
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
  formula = AUCECEXAM ~ 1 + Weighting + Origin + LOC + (1|Project),
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
  formula = AUCECEXAM ~ 1 + Weighting + Origin + LOC + (1|Language) + (1|Project),
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
  formula = AUCECEXAM ~ 1 + Weighting + Origin + LOC + (1|Domain) + (1|Project),
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
  formula = AUCECEXAM ~ 1 + Weighting + Origin + LOC + (1|Domain) + (1|Project) + (1|Language),
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
# Here it seems like model 5 is best but models 4 and 3 have elpd_diff < 2* se_diff
# and model 6 is on the edge.
# We again chose model 3 as it is the simplest one. We can keep model 5 as comparison
# And analyze both.


# Again model 7 seems to sample better

save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, m2.7, file="m2.RData")

##########################################################
# Final models

# based on m1.3
m3.1 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + (1|Project),
  data = ls.df,
  family=Beta(),
  prior = c(
    prior(normal(0, 0.5), class=Intercept),
    prior(normal(0, 0.5), class=b),
    prior(cauchy(0, 0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

loo3.1 = loo(m3.1)
plot(m3.1)
summary(m3.1)
# The intercept doesn't sample well. We try the least complex model that has over 10% n_eff
# for all coefficients.

m3.2 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + (1|Project) + (1|Language),
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
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
loo3.3 = loo(m3.3)
loo3.2 = loo(m3.2)
plot(m3.2)
summary(m3.2)
# Samples a lot better than m3.1 and all diagnostics look fine.
np <- nuts_params(m3.2)
lp <- log_posterior(m3.2)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)

loo_compare(loo3.1, loo3.2, loo3.3)
# Both models seem to have the same performance in terms of loo.
# We will look at both initially to see the difference.

fixef(m3.1, summary=TRUE)[1:3,]
fixef(m3.2, summary=TRUE)[1:3,]
# While the estimated means are similar, the estimated errors differ. m3.2 has more uncertainty.
# Both however seem to consider the coefficiencts to be very similar.

stanplot(m3.1, type="areas", pars="^b_Weight")
stanplot(m3.2, type="areas", pars="^b_Weight")
# Visualization of the numbers before.

# We will analyze m3.2 as it has the better sampling behaviour. While it is more uncertain
# we should accept the uncertainty.
hypothesis(m3.2, "Weightinglinear_weighting_function=Weightinggoogle_weighting_function")
plot(hypothesis(m3.2, "Weightinglinear_weighting_function=Weightinggoogle_weighting_function"))
hypothesis(m3.2, "Weightingflat_weighting_function=Weightinggoogle_weighting_function")
plot(hypothesis(m3.2, "Weightingflat_weighting_function=Weightinggoogle_weighting_function"))
hypothesis(m3.2, "Weightingflat_weighting_function=Weightinglinear_weighting_function")
plot(hypothesis(m3.2, "Weightingflat_weighting_function=Weightinglinear_weighting_function"))
# Using hypothesis testing, it seems that there is no difference between any of the effects
# of the weighting functions.
# We will try to put numbers on it:

post.google = posterior_predict(m3.2, newdata = subset(ls.df, ls.df$Weighting == "google_weighting_function"), seed=SEED)
post.linear = posterior_predict(m3.2, newdata = subset(ls.df, ls.df$Weighting == "linear_weighting_function"), seed=SEED)
post.flat = posterior_predict(m3.2, newdata = subset(ls.df, ls.df$Weighting == "flat_weighting_function"), seed=SEED)

diff_gl = post.google - post.linear
diff_gf = post.google - post.flat
diff_lf = post.linear - post.flat

plot(density(diff_gl))
for(i in seq(-4,4,2)){
  abline(v=mean(diff_gl)+(i * sd(diff_gl)))
}
gl.intervals = mean(diff_gl)+(seq(-4,4,2) * sd(diff_gl))

plot(density(diff_gf))
for(i in seq(-4,4,2)){
  abline(v=mean(diff_gf)+(i * sd(diff_gf)))
}
gf.intervals = mean(diff_gf)+(seq(-4,4,2) * sd(diff_gf))

plot(density(diff_lf))
for(i in seq(-4,4,2)){
  abline(v=mean(diff_lf)+(i * sd(diff_lf)))
}
gf.intervals = mean(diff_lf)+(seq(-4,4,2) * sd(diff_lf))

# Based on these numbers, we argue that there is no difference in weighting function effect on EXAM
# score. This might depend on the depth as we only used one depth.

save(m3.1, m3.2, file="m3.RData")


############################################################
# And do the same for the AUCEC

# based on 2.3
m4.1 = brm(
  formula = AUCECEXAM ~ 1 + Weighting + LOC + (1|Project),
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
loo4.1 = loo(m4.1)
plot(m4.1)
summary(m4.1)
# Seems like m4.1 has problems sampling with low n_eff. We try the least complex model that has over 10% n_eff
# for all coefficients.

m4.2 = brm(
  formula = AUCECEXAM ~ 1 + Weighting + LOC + (1|Project) + (1|Language),
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
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
loo4.2 = loo(m4.2)
plot(m4.2)
summary(m4.2)
# Samples a lot better than m3.1 and all diagnostics look fine.
np <- nuts_params(m4.2)
lp <- log_posterior(m4.2)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)

loo_compare(loo4.1, loo4.2)
# Both models seem to have the same performance in terms of loo.
# We will look at both initially to see the difference.


fixef(m4.1, summary=TRUE)[1:3,]
fixef(m4.2, summary=TRUE)[1:3,]
# While the estimated means are similar, the estimated errors differ. m4.2 has more uncertainty.
# Both however seem to consider the coefficiencts to be very similar around the mean.

stanplot(m4.1, type="areas", pars="^b_Weight")
stanplot(m4.2, type="areas", pars="^b_Weight")
# Visualization of the numbers before. Again very obvious, that both models predict the different
# weighting functions to have the same effects.

# We will analyze m3.2 as it has the better sampling behaviour. While it is more uncertain
# we should accept the uncertainty.
hypothesis(m4.2, "Weightinglinear_weighting_function=Weightinggoogle_weighting_function")
plot(hypothesis(m4.2, "Weightinglinear_weighting_function=Weightinggoogle_weighting_function"))
hypothesis(m4.2, "Weightingflat_weighting_function=Weightinggoogle_weighting_function")
plot(hypothesis(m4.2, "Weightingflat_weighting_function=Weightinggoogle_weighting_function"))
hypothesis(m4.2, "Weightingflat_weighting_function=Weightinglinear_weighting_function")
plot(hypothesis(m4.2, "Weightingflat_weighting_function=Weightinglinear_weighting_function"))
# Using hypothesis testing, it seems that there is no difference between any of the effects
# of the weighting functions.
# We will try to put numbers on it:

post.google = posterior_predict(m4.2, newdata = subset(ls.df, ls.df$Weighting == "google_weighting_function"), seed=SEED)
post.linear = posterior_predict(m4.2, newdata = subset(ls.df, ls.df$Weighting == "linear_weighting_function"), seed=SEED)
post.flat = posterior_predict(m4.2, newdata = subset(ls.df, ls.df$Weighting == "flat_weighting_function"), seed=SEED)

diff_gl = post.google - post.linear
diff_gf = post.google - post.flat
diff_lf = post.linear - post.flat

plot(density(diff_gl))
for(i in seq(-4,4,2)){
  abline(v=mean(diff_gl)+(i * sd(diff_gl)))
}
gl.intervals = mean(diff_gl)+(seq(-4,4,2) * sd(diff_gl))

plot(density(diff_gf))
for(i in seq(-4,4,2)){
  abline(v=mean(diff_gf)+(i * sd(diff_gf)))
}
gf.intervals = mean(diff_gf)+(seq(-4,4,2) * sd(diff_gf))

plot(density(diff_lf))
for(i in seq(-4,4,2)){
  abline(v=mean(diff_lf)+(i * sd(diff_lf)))
}
gf.intervals = mean(diff_lf)+(seq(-4,4,2) * sd(diff_lf))

# Based on these numbers, we argue that there is no difference in weighting function effect on EXAM
# score. This might depend on the depth as we only used one depth.


save(m4.1, m4.2, file="m4.RData")
