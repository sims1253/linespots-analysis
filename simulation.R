library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
options(mc.cores = parallel::detectCores())
SEED = 140919

ls.mean = 0.6
bs.mean = 0.7
sd= 0.05

a = rnorm(1000, mean=ls.mean, sd=sd)
b = rnorm(1000, mean=bs.mean, sd=sd)
sim_data = tibble("AUCEC" = c(a, b), "Algorithm" = rep(c("Linespots", "Bugspots"), each=1000) )
boxplot(AUCEC ~ Algorithm, data=sim_data)

sim_model1 = brm(
  formula = AUCEC ~ 1 + Algorithm,
  data = sim_data,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,1), class=b),
    prior(gamma(0.01, 0.01), class=phi)
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)

sim_model2 = brm(
  formula = AUCEC ~ 1 + Algorithm,
  data = sim_data,
  family=Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.01, 0.01), class=phi)
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)

loo1 = loo(sim_model1)
loo1
loo2 = loo(sim_model2)
loo2
loo_compare(loo1, loo2)
summary(sim_model1)
summary(sim_model2)

mean(a) - mean(b)
intercept = posterior_samples(sim_model1)$b_Intercept
linespots = posterior_samples(sim_model1)$b_AlgorithmLinespots
linespots_effect = inv_logit_scaled(intercept + linespots) - inv_logit_scaled(intercept)
mean(linespots_effect)
sd(linespots_effect)
quants = quantile(linespots_effect, c(0.025, 0.975))
plot(density(linespots_effect, adjust = 0.1))
abline(v=quants[1])
abline(v=quants[2])

# So the mean effect of switching from bugspots to linespots is a decrease in AUCEC
# of 0.09797303 with a standard deviation of 0.002265158. An empirical posterior of
# the difference on the output scale with 95% intervals is plottet.