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
# We can also keep model 5 and 7 for comparison as the highest ranked and most complex model.
# And while we see transitions exceeding tree depth, that is not a validity concern
# but only a efficiency concern.

# Model 7 seems to sample better

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
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
plot(m3.1)
summary(m3.1)
# Intercept and group intercept don't sample too well. The intercept is even unter 10%
# Other diagnostics look good from what I can tell.
np <- nuts_params(m3.1)
lp <- log_posterior(m3.1)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)

# based on m1.5
m3.2 = brm(
  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Language),
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
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
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

# based on m1.7
m3.3 = brm(
  formula = EXAM ~ 1 + Weighting +LOC + Origin + (1|Domain) + (1|Project) + (1|Language),
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
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
plot(m3.3)
summary(m3.3)
# Samples similarly good to m3.2 and all diagnostics seem fine.

np <- nuts_params(m3.3)
lp <- log_posterior(m3.3)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)


loo_compare(loo(m3.1), loo(m3.2), loo(m3.3))
# All three models seem to have identical explanatory power  in terms of loo.

stanplot(m3.1, type="areas", pars="b_Weight")
stanplot(m3.2, type="areas", pars="b_Weight")
stanplot(m3.3, type="areas", pars="b_Weight")
# Both weighting functions seem to have no significant effects but habe more propability mass
# on the negative side. This is hard to interpret due to the linking function though.

plot(hypothesis(m3.1, "Weightinglinear_weighting_function=0"))
plot(hypothesis(m3.1, "Weightingflat_weighting_function=0"))

plot(hypothesis(m3.2, "Weightinglinear_weighting_function=0"))
plot(hypothesis(m3.2, "Weightingflat_weighting_function=0"))

plot(hypothesis(m3.3, "Weightinglinear_weighting_function=0"))
plot(hypothesis(m3.3, "Weightingflat_weighting_function=0"))
# The posteriors for all weighting functions are very focused around 0 which further supports the idea
# that they have no impact.

# Lets calculate the difference between the google weighting function (as part of the intercept)
# and the other two weighting functions for all three models.
post1 = posterior_samples(m3.1)
lin.effect1 = inv_logit_scaled(post1$b_Intercept + post1$b_Weightinglinear_weighting_function) - inv_logit_scaled(post1$b_Intercept)
flat.effect1 = inv_logit_scaled(post1$b_Intercept + post1$b_Weightingflat_weighting_function) - inv_logit_scaled(post1$b_Intercept)
plot(density(lin.effect1, adjust = 0.1))
quants = quantile(lin.effect1, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(lin.effect1))
abline(v=median(lin.effect1), lty="dotted")

plot(density(flat.effect1, adjust = 0.1))
quants = quantile(flat.effect1, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(flat.effect1))
abline(v=median(flat.effect1), lty="dotted")

post2 = posterior_samples(m3.2)
lin.effect2 = inv_logit_scaled(post1$b_Intercept + post2$b_Weightinglinear_weighting_function) - inv_logit_scaled(post2$b_Intercept)
flat.effect2 = inv_logit_scaled(post1$b_Intercept + post2$b_Weightingflat_weighting_function) - inv_logit_scaled(post2$b_Intercept)
plot(density(lin.effect2, adjust = 0.1))
quants = quantile(lin.effect2, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(lin.effect2))
abline(v=median(lin.effect2), lty="dotted")

plot(density(flat.effect2, adjust = 0.1))
quants = quantile(flat.effect2, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(flat.effect2))
abline(v=median(flat.effect2), lty="dotted")

post3 = posterior_samples(m3.3)
lin.effect3 = inv_logit_scaled(post1$b_Intercept + post3$b_Weightinglinear_weighting_function) - inv_logit_scaled(post3$b_Intercept)
flat.effect3 = inv_logit_scaled(post1$b_Intercept + post3$b_Weightingflat_weighting_function) - inv_logit_scaled(post3$b_Intercept)
plot(density(lin.effect3, adjust = 0.1))
quants = quantile(lin.effect3, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(lin.effect3))
abline(v=median(lin.effect3), lty="dotted")

plot(density(flat.effect3, adjust = 0.1))
quants = quantile(flat.effect3, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(flat.effect3))
abline(v=median(flat.effect3), lty="dotted")
# All three models have large overlaps with 0 for both weighting functions so there seems to be
# no significant effect. Both the mean and median values are also not consistent positive or negative.
# Based on this we conclude, that the weighting function does not have a any consistend impact on the
# EXAM score.



############################################################
# And do the same for the AUCEC

# based on 2.3
m4.1 = brm(
  formula = AUCECEXAM ~ 1 + Weighting + LOC + (1|Project),
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
plot(m4.1)
summary(m4.1)
# Seems like m4.1 has problems sampling the Intercept.
np <- nuts_params(m4.1)
lp <- log_posterior(m4.1)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)
# Diagnostics look good otherwise.

# based on 2.7
m4.2 = brm(
  formula = AUCECEXAM ~ 1 + Weighting + Origin + LOC + (1|Domain) + (1|Project) + (1|Language),
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
plot(m4.2)
summary(m4.2)
# Model 4.2 seems to sample a lot better than 4.1.
np <- nuts_params(m4.2)
lp <- log_posterior(m4.2)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)
# Diagnostics look good.

loo_compare(loo(m4.1), loo(m4.2))
# The models seem to perform equally good.

stanplot(m4.1, type="areas", pars="b_Weight")
stanplot(m4.2, type="areas", pars="b_Weight")
# Again both weighting functions have high overlap with 0 with a little bit more weight on the positive
# side. Again hard to interpret though.
plot(hypothesis(m4.1, "Weightinglinear_weighting_function=0"))
plot(hypothesis(m4.1, "Weightingflat_weighting_function=0"))

plot(hypothesis(m4.2, "Weightinglinear_weighting_function=0"))
plot(hypothesis(m4.2, "Weightingflat_weighting_function=0"))
# The posteriors for all weighting functions are very focused around 0 which further supports the idea
# that they have no impact.

# Lets calculate the difference between the google weighting function (as part of the intercept)
# and the other two weighting functions for all three models again.
post1 = posterior_samples(m4.1)
lin.effect1 = inv_logit_scaled(post1$b_Intercept + post1$b_Weightinglinear_weighting_function) - inv_logit_scaled(post1$b_Intercept)
flat.effect1 = inv_logit_scaled(post1$b_Intercept + post1$b_Weightingflat_weighting_function) - inv_logit_scaled(post1$b_Intercept)
plot(density(lin.effect1, adjust = 0.1))
quants = quantile(lin.effect1, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(lin.effect1))
abline(v=median(lin.effect1), lty="dotted")

plot(density(flat.effect1, adjust = 0.1))
quants = quantile(flat.effect1, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(flat.effect1))
abline(v=median(flat.effect1), lty="dotted")

post2 = posterior_samples(m4.2)
lin.effect2 = inv_logit_scaled(post1$b_Intercept + post2$b_Weightinglinear_weighting_function) - inv_logit_scaled(post2$b_Intercept)
flat.effect2 = inv_logit_scaled(post1$b_Intercept + post2$b_Weightingflat_weighting_function) - inv_logit_scaled(post2$b_Intercept)
plot(density(lin.effect2, adjust = 0.1))
quants = quantile(lin.effect2, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(lin.effect2))
abline(v=median(lin.effect2), lty="dotted")

plot(density(flat.effect2, adjust = 0.1))
quants = quantile(flat.effect2, c(0.025, 0.975))
abline(v=quants[1])
abline(v=quants[2])
abline(v=mean(flat.effect2))
abline(v=median(flat.effect2), lty="dotted")

# ABoth models have large overlaps with 0 for both weighting functions so there seems to be
# no significant effect. Both the mean and median values are also not consistent positive or negative.
# Based on this we conclude, that the weighting function does not have a any consistend impact on the
# AUCEC.

save(m3.1, m3.2, m3.3, file="m3.RData")
save(m4.1, m4.2, file="m4.RData")
