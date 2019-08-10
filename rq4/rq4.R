library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
library(gridExtra)
library(ggthemes)
ggplot2::theme_set(theme_tufte())
bayesplot::color_scheme_set("darkgray")
options(mc.cores = parallel::detectCores())

SEED = 140919 # The day I move in with my gf

setwd("~/Documents/dev/linespots/linespots-analysis/rq4")
load(file="m1.RData")
load(file="m2.RData")
load(file="m3.RData")
load(file="m4.RData")
load(file="m5.RData")
load(file="m6.RData")

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
pdf("rq4-box-exam.pdf")
ggplot(d, aes(x=Algorithm, y=EXAM)) + geom_boxplot()
dev.off()
# All three weighting functions seem to produce very similar EXAM scores.
# The flat one seems to have the lowest median slightly.
pdf("rq4-box-aucec.pdf")
ggplot(d, aes(x=Algorithm, y=AUCECEXAM)) + geom_boxplot()
dev.off()
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
    prior(normal(0,0.5), class=b),
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
    prior(normal(0,0.5), class=b),
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
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
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
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
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
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

m1.6 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + Origin + (1|Project) + (1|Language) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.9999, max_treedepth=15),
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
summary(m1.5)


# We repeat the same process for the AUCEC output.
# The proposed predictors were: LOC, Project, Origin, Language
m2.1 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm,
  data = d,
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
  formula = AUCECEXAM ~ 1 + Algorithm + LOC,
  data = d,
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
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
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
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
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
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.99, max_treedepth=15),
  seed = SEED
)

m2.6 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + Origin + (1|Project) + (1|Language) + (1|Domain),
  data = d,
  family=Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  cores = parallel::detectCores(),
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

loo2.1 = loo(m2.1)
loo2.2 = loo(m2.2)
loo2.3 = loo(m2.3)
loo2.4 = loo(m2.4)
loo2.5 = loo(m2.5)
loo2.6 = loo(m2.6)

loo_compare(loo2.1, loo2.2, loo2.3, loo2.4, loo2.5, loo2.6)
summary(m2.3)
summary(m2.4)
summary(m2.5)


save(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, file="m2.RData")


##########################################################
# Final models
# We can use the same priors as for rq1 and 2

# based on m1.3
m3.1 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
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
  control = list(adapt_delta=0.9999, max_treedepth=15),
  seed = SEED
)
summary(m3.1)
loo3.1 = loo(m3.1)

pdf("rq4-exam-m1-neff.pdf")
mcmc_neff(neff_ratio(m3.1))
dev.off()

pdf("rq4-exam-m1-rhat.pdf")
mcmc_rhat(rhat(m3.1))
dev.off()

pdf("rq4-exam-m1-trace.pdf")
mcmc_trace(m3.1)
dev.off()

np <- nuts_params(m3.1)
lp <- log_posterior(m3.1)
pdf("rq4-exam-m1-acc.pdf")
mcmc_nuts_acceptance(np, lp)
dev.off()

pdf("rq4-exam-m1-div.pdf")
mcmc_nuts_divergence(np, lp)
dev.off()

pdf("rq4-exam-m1-step.pdf")
mcmc_nuts_stepsize(np, lp)
dev.off()

pdf("rq4-exam-m1-tree.pdf")
mcmc_nuts_treedepth(np, lp)
dev.off()

pdf("rq4-exam-m1-energy.pdf")
mcmc_nuts_energy(np, lp)
dev.off()

m3.2 = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + (1|Project) + (1|Language),
  data = d,
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
  control = list(adapt_delta=0.9999, max_treedepth=15),
  seed = SEED
)
summary(m3.2)
loo3.2 = loo(m3.2)


pdf("rq4-exam-m2-neff.pdf")
mcmc_neff(neff_ratio(m3.2))
dev.off()

pdf("rq4-exam-m2-rhat.pdf")
mcmc_rhat(rhat(m3.2))
dev.off()

pdf("rq4-exam-m2-trace.pdf")
mcmc_trace(m3.2)
dev.off()

np <- nuts_params(m3.2)
lp <- log_posterior(m3.2)
pdf("rq4-exam-m2-acc.pdf")
mcmc_nuts_acceptance(np, lp)
dev.off()

pdf("rq4-exam-m2-div.pdf")
mcmc_nuts_divergence(np, lp)
dev.off()

pdf("rq4-exam-m2-step.pdf")
mcmc_nuts_stepsize(np, lp)
dev.off()

pdf("rq4-exam-m2-tree.pdf")
mcmc_nuts_treedepth(np, lp)
dev.off()

pdf("rq4-exam-m2-energy.pdf")
mcmc_nuts_energy(np, lp)
dev.off()


loo_compare(loo3.1, loo3.2)



fixef(m3.1, summary=TRUE)
fixef(m3.2, summary=TRUE)
# While the estimated means are similar, the estimated errors differ. m3.2 has more uncertainty.
# Both however seem to consider the coefficiencts to be very similar.

# We continue with the second model due to similar loo performance and better sampling than m3.1
stanplot(m3.2, type="areas", pars="^b_Algorithm")
pdf("rq4-exam-mcmc-area.pdf")
title = ggtitle("Posterior distribution",
                "with medians and 95% intervals")
mcmc_areas(as.matrix(m3.2),
           pars = c("b_AlgorithmBugspots"),
           prob = 0.95) + title
dev.off()
# Visualization of the numbers before.

# We will analyze m3.2 as it has the better sampling behaviour. While it is more uncertain
# we should accept the uncertainty.
hypothesis(m3.2, "AlgorithmBugspots=0")
pdf("rq4-exam-h1.pdf")
plot(hypothesis(m3.2, "Timecommit=0"))
dev.off()
# Using hypothesis testing, it seems that there is no difference between any of the effects
# of the weighting functions.
# We will try to put numbers on it:
post.ls = posterior_predict(m3.2, newdata = subset(d, d$Algorithm == "Linespots"), seed=SEED)
post.bs = posterior_predict(m3.2, newdata = subset(d, d$Algorithm == "Bugspots"), seed=SEED)
diff_lb = tibble("Effect" = as.vector(post.ls - post.bs))
lb.intervals = mean(diff_lb$Effect)+(seq(-4,4,2) * sd(diff_lb$Effect))
lb.intervals
pdf("rq4-exam-diff-lb.pdf")
p = ggplot(diff_lb, aes(x=Effect)) +
  geom_density(color="grey22")
foo = ggplot_build(p)$data[[1]]

p = p +
  geom_area(data = subset(foo, x >= lb.intervals[2] & x <= lb.intervals[4]), aes(x=x, y=y), fill="grey") +
  geom_area(data = subset(foo, x >= lb.intervals[1] & x <= lb.intervals[2]), aes(x=x, y=y), fill="lightgrey") +
  geom_area(data = subset(foo, x >= lb.intervals[4] & x <= lb.intervals[5]), aes(x=x, y=y), fill="lightgrey") + 
  geom_vline(aes(xintercept=median(Effect)), linetype="solid", color="grey32") +
  ggtitle("Linespots - Bugspots Marginal Effect on EXAM", "with median, 2 and 4 sd intervals")
p
dev.off()


pdf("rq4-exam-marginal.pdf")
marginal_effects(m3.2) # You have to manually press enter before running dev.off()
dev.off()




save(m3.1, m3.2, file="m3.RData")

###################################################################

# based on 2.3
m4.1 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
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
  control = list(adapt_delta=0.9999, max_treedepth = 15),
  seed = SEED
)
summary(m4.1)
loo4.1 = loo(m4.1)


pdf("rq4-aucec-m1-neff.pdf")
mcmc_neff(neff_ratio(m4.1))
dev.off()

pdf("rq4-aucec-m1-rhat.pdf")
mcmc_rhat(rhat(m4.1))
dev.off()

pdf("rq4-aucec-m1-trace.pdf")
mcmc_trace(m4.1)
dev.off()

np <- nuts_params(m4.1)
lp <- log_posterior(m4.1)
pdf("rq4-aucec-m1-acc.pdf")
mcmc_nuts_acceptance(np, lp)
dev.off()

pdf("rq4-aucec-m1-div.pdf")
mcmc_nuts_divergence(np, lp)
dev.off()

pdf("rq4-aucec-m1-step.pdf")
mcmc_nuts_stepsize(np, lp)
dev.off()

pdf("rq4-aucec-m1-tree.pdf")
mcmc_nuts_treedepth(np, lp)
dev.off()

pdf("rq4-aucec-m1-energy.pdf")
mcmc_nuts_energy(np, lp)
dev.off()

m4.2 = brm(
  formula = AUCECEXAM ~ 1 + Algorithm + LOC + (1|Project) + (1|Language),
  data = d,
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
  control = list(adapt_delta=0.9999, max_treedepth = 15),
  seed = SEED
)
summary(m4.2)
loo4.2 = loo(m4.2)


pdf("rq4-aucec-m2-neff.pdf")
mcmc_neff(neff_ratio(m4.2))
dev.off()

pdf("rq4-aucec-m2-rhat.pdf")
mcmc_rhat(rhat(m4.2))
dev.off()

pdf("rq4-aucec-m2-trace.pdf")
mcmc_trace(m4.2)
dev.off()

np <- nuts_params(m4.2)
lp <- log_posterior(m4.2)
pdf("rq4-aucec-m2-acc.pdf")
mcmc_nuts_acceptance(np, lp)
dev.off()

pdf("rq4-aucec-m2-div.pdf")
mcmc_nuts_divergence(np, lp)
dev.off()

pdf("rq4-aucec-m2-step.pdf")
mcmc_nuts_stepsize(np, lp)
dev.off()

pdf("rq4-aucec-m2-tree.pdf")
mcmc_nuts_treedepth(np, lp)
dev.off()

pdf("rq4-aucec-m2-energy.pdf")
mcmc_nuts_energy(np, lp)
dev.off()

loo_compare(loo4.1, loo4.2)


fixef(m4.1, summary=TRUE)
fixef(m4.2, summary=TRUE)
# While the estimated means are similar, the estimated errors differ. m4.2 has more uncertainty.
# Both however seem to consider the coefficiencts to be very similar.

# We continue with the second model due to similar loo performance and better sampling than m4.1
stanplot(m4.2, type="areas", pars="^b_Algorithm")
pdf("rq4-aucec-mcmc-area.pdf")
title = ggtitle("Posterior distribution",
                "with medians and 95% intervals")
mcmc_areas(as.matrix(m4.2),
           pars = c("b_AlgorithmBugspots"),
           prob = 0.95) + title
dev.off()
# Visualization of the numbers before.

# We will analyze m4.2 as it has the better sampling behaviour. While it is more uncertain
# we should accept the uncertainty.
hypothesis(m4.2, "AlgorithmBugspots=0")
pdf("rq4-aucec-h1.pdf")
plot(hypothesis(m4.2, "Timecommit=0"))
dev.off()
# Using hypothesis testing, it seems that there is no difference between any of the effects
# of the weighting functions.
# We will try to put numbers on it:
post.ls = posterior_predict(m4.2, newdata = subset(d, d$Algorithm == "Linespots"), seed=SEED)
post.bs = posterior_predict(m4.2, newdata = subset(d, d$Algorithm == "Bugspots"), seed=SEED)
diff_lb = tibble("Effect" = as.vector(post.ls - post.bs))
lb.intervals = mean(diff_lb$Effect)+(seq(-4,4,2) * sd(diff_lb$Effect))
lb.intervals
pdf("rq4-aucec-diff-lb.pdf")
p = ggplot(diff_lb, aes(x=Effect)) +
  geom_density(color="grey22")
foo = ggplot_build(p)$data[[1]]

p = p +
  geom_area(data = subset(foo, x >= lb.intervals[2] & x <= lb.intervals[4]), aes(x=x, y=y), fill="grey") +
  geom_area(data = subset(foo, x >= lb.intervals[1] & x <= lb.intervals[2]), aes(x=x, y=y), fill="lightgrey") +
  geom_area(data = subset(foo, x >= lb.intervals[4] & x <= lb.intervals[5]), aes(x=x, y=y), fill="lightgrey") + 
  geom_vline(aes(xintercept=median(Effect)), linetype="solid", color="grey32") +
  ggtitle("Linespots - Bugspots Marginal Effect on aucec", "with median, 2 and 4 sd intervals")
p
dev.off()


pdf("rq4-aucec-marginal.pdf")
marginal_effects(m4.2) # You have to manually press enter before running dev.off()
dev.off()


save(m4.1, m4.2, file="m4.RData")




##################################################################################

# Small trial for Exam25
# That would give LOC, Algorithm, Project, Language and maybe Domain as predictors.
# The mcmc_area plot also shows Origin and commit to have noticable effects.

m5.1 = brm(
  formula = EXAM25 ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
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
loo5.1 = loo(m5.1)

m5.2 = brm(
  formula = EXAM25 ~ 1 + Algorithm + LOC + (1|Project) + (1|Language),
  data = d,
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
loo5.2 = loo(m5.2)

m5.3 = brm(
  formula = EXAM25 ~ 1 + Algorithm + LOC + (1|Project) + (1|Language) + (1|Domain) + Origin + Commits,
  data = d,
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
loo5.3 = loo(m5.3)

loo_compare(loo5.1, loo5.2, loo5.3)
save(m5.1, m5.2, m5.3, file="m5.RData")


##################################################################################

# Small trial for EInstect25EXAM
# That would give Algorithm, Domain, Language, Project, LOC, Weighting as predictors.
# The mcmc_areas also makes Origin and commit seem usefull though.

m6.1 = brm(
  formula = EInspect25EXAM ~ 1 + Algorithm + LOC + (1|Project),
  data = d,
  family=poisson(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)
loo6.1 = loo(m6.1)

m6.1b = brm(
  bf(EInspect25EXAM ~ 1 + Algorithm + LOC + (1|Project), zi ~ (1|Project)),
  data = d,
  family=zero_inflated_poisson(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(cauchy(0,0.1), class=sd)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999),
  seed = SEED
)

m6.2 = brm(
  formula = EInspect25EXAM ~ 1 + Algorithm + (1|Domain) + (1|Project) + (1|Language) + LOC + Weighting,
  data = d,
  family=poisson(),
  prior = c(
    prior(normal(0,1), class=Intercept),
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
loo6.2 = loo(m6.2)

m6.3 = brm(
  bf(EInspect25EXAM ~ 1 + Algorithm + LOC + (1|Project) + (1|Language) + (1|Domain) + Origin + Commits + Weighting),
  data = d,
  family=poisson(),
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
loo6.3 = loo(m6.3)

loo_compare(loo6.1, loo6.2, loo6.3)
save(m6.1, m6.2, m6.3, file="m6.RData")
