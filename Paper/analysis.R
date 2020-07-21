library(ggdag)
library(brms)
library(tidyverse)
library(GGally)
library(bayesplot)
library(loo)
library(gridExtra)
library(ggthemes)

ggplot2::theme_set(theme_tufte())
bayesplot::color_scheme_set("darkgray")
options(mc.cores = parallel::detectCores())

SEED = 2020

setwd("~/Documents/linespots/linespots-analysis/Paper/data/")

d = read_delim(
  'full_evaluation.csv',
  delim = ",",
  locale = locale(decimal_mark = "."),
  col_names = TRUE,
  col_types = cols(
    AUCEC1 = col_double(),
    AUCEC100 = col_double(),
    AUCEC20 = col_double(),
    AUCEC5 = col_double(),
    AUROC = col_double(),
    Algorithm = col_factor(),
    Depth = col_double(),
    EInspect10 = col_double(),
    EInspect100 = col_double(),
    EInspect200 = col_double(),
    EInspect50 = col_double(),
    EInspectF = col_double(),
    EXAM = col_double(),
    FixCount = col_double(),
    Future = col_double(),
    LOC = col_double(),
    Origin = col_double(),
    Project = col_factor(),
    Runtime = col_double(),
    Time = col_factor(),
    Weight = col_factor(),
    comit_version = col_factor(),
    commits = col_double(),
    language = col_factor(),
    url = col_factor()
  )
)

foo = subset(d, d$FixCount == 0)
bar = subset(d, is.na(d$EXAM))
d = subset(d, d$FixCount != 0)
# Turns out one sample from junit did not contain any fixed faults in the pseudo
# future so we have to remove it.

# Standardizing
d$LOC = scale(d$LOC)
d$FixCount = scale(d$FixCount)
d$Origin = scale(d$Origin)

hist(d$Runtime)
hist(log(d$Runtime))

ggpairs(d, columns=c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16, 17,20,21))

################################################################################




Eval_dag <- dagify(
  Project ~ Language,
  LOC ~ Project,
  FixCount ~ Project,
  Runtime ~ Algorithm + LOC + Project + Language,
  EvaluationMetrics ~ Algorithm + Project + LOC + FixCount + Language,
  exposure = "Algorithm",
  outcome = "EvaluationMetrics",
  labels = c(
    "Project" = "Project",
    "Language" = "Language",
    "LOC" = "LOC",
    "FixCount" = "Fix\nCount",
    "Runtime" = "Runtime",
    "Algorithm" = "Algorithm",
    "EvaluationMetrics" = "Evaluation\nMetrics"
  )
)

ggdag(Eval_dag, text = FALSE, use_labels = "label") +
  theme_dag()
ggdag_paths(Eval_dag,
            text = FALSE,
            use_labels = "label",
            shadow = TRUE) +
  theme_dag()

ggdag_adjustment_set(Eval_dag,
                     text = FALSE,
                     use_labels = "label",
                     shadow = TRUE) +
  theme_dag()

ggdag_dseparated(
  Eval_dag,
  from = "Algorithm",
  to = "EvaluationMetrics",
  controlling_for = c("Project", "FixCount")
)

# So it seems that as long as we don't condition on Runtime, Algorithm and the
# Evaluation Metrics are d-separated.
################################################################################
# Lets try some 

m1.1t = brm(
  formula = AUROC ~ 1 + Algorithm,
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi)
  ),
  chains = 4,
  refresh = 0,
  sample_prior = "only",
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
pp_check(m1.1t)

m1.1 = brm(
  formula = AUROC ~ 1 + Algorithm,
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
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

m1.2t = brm(
  formula = AUROC ~ 1 + Algorithm + (1|Project),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi),
    prior(cauchy(0,0.01), class=sd)
  ),
  chains = 4,
  refresh = 0,
  sample_prior = "only",
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
pp_check(m1.2t, nsamples = 100)

m1.2 = brm(
  formula = AUROC ~ 1 + Algorithm + (1|Project),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi),
    prior(cauchy(0,0.01), class=sd)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

m1.3t = brm(
  formula = AUROC ~ 1 + Algorithm + (1|Project) + (1|language),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,0.1), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(0.1, 0.1), class=phi),
    prior(cauchy(0,0.01), class=sd)
  ),
  chains = 4,
  refresh = 0,
  sample_prior = "only",
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
pp_check(m1.3t, nsamples = 100)

m1.3 = brm(
  formula = AUROC ~ 1 + Algorithm + (1|Project) + (1|language),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,0.1), class=Intercept),
    prior(normal(0,0.1), class=b),
    prior(gamma(0.1, 0.1), class=phi),
    prior(cauchy(0,0.01), class=sd)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)


loo1 = loo(m1.1)
loo2 = loo(m1.2)
loo3 = loo(m1.3)
loo_compare(loo1, loo2, loo3)

mcmc_neff(neff_ratio(m1.2))
mcmc_rhat(rhat(m1.2))
mcmc_trace(m1.2)

np <- nuts_params(m1.2)
lp <- log_posterior(m1.2)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)


fixef(m1.2, summary=TRUE)
stanplot(m1.2, type="areas", pars="^b_Algorithm")

title = ggtitle("Posterior distribution",
                "with medians and 95% intervals")
mcmc_areas(as.matrix(m1.2),
           pars = c("b_AlgorithmBugspots"),
           prob = 0.95) + title
hypothesis(m1.2, "AlgorithmBugspots=0")
plot(hypothesis(m1.2, "AlgorithmBugspots=0"))
################################################################################

m2.1t = brm(formula = EInspect50 ~ 1 + Algorithm,
  data=d,
  family = poisson(),
  prior = c(
    prior(normal(0,0.1), class=Intercept),
    prior(normal(0,0.1), class=b)
  ),
  chains = 4,
  refresh = 0,
  sample_prior = "only",
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)
pp_check(m2.1t, nsamples = 100)

m2.2t = brm(
  formula = EInspect100 ~ 1 + Algorithm + (1|Project),
  data=d,
  family = poisson(),
  chains = 4,
  refresh = 0,
  sample_prior = "only",
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

m2.3t = brm(
  formula = EInspect100 ~ 1 + Algorithm + (1|Project) + (1|language),
  data=d,
  family = poisson(),
  chains = 4,
  refresh = 0,
  sample_prior = "only",
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

################################################################################

m3.1 = brm(
  formula = EXAM ~ 1 + Algorithm,
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,0.5), class=b),
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

m3.2 = brm(
  formula = EXAM ~ 1 + Algorithm + (1|Project),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(0,0.5), class=Intercept),
    prior(normal(0,0.5), class=b),
    prior(gamma(0.1, 0.1), class=phi),
    prior(cauchy(0,0.1), class=sd)
  ),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta=0.999, max_treedepth=15),
  seed = SEED
)

loo1 = loo(m2.1)
loo2 = loo(m2.2)
loo_compare(loo1, loo2)
summary(m2.2)

mcmc_neff(neff_ratio(m2.2))
mcmc_rhat(rhat(m2.2))
mcmc_trace(m2.2)

np <- nuts_params(m2.2)
lp <- log_posterior(m2.2)
mcmc_nuts_acceptance(np, lp)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)
mcmc_nuts_energy(np, lp)


fixef(m2.2, summary=TRUE)
stanplot(m2.2, type="areas", pars="^b_Algorithm")

title = ggtitle("Posterior distribution",
                "with medians and 95% intervals")
mcmc_areas(as.matrix(m2.2),
           pars = c("b_AlgorithmBugspots"),
           prob = 0.95) + title
hypothesis(m2.2, "AlgorithmBugspots=0")
plot(hypothesis(m2.2, "AlgorithmBugspots=0"))


Runtime_dag <- dagify(
  Project ~ Language,
  LOC ~ Project,
  FixCount ~ Project,
  Runtime ~ Algorithm + LOC + Project + Language,
  EvaluationMetrics ~ Algorithm + Project + LOC + FixCount + Language,
  exposure = "Algorithm",
  outcome = "Runtime",
  labels = c(
    "Project" = "Project",
    "Language" = "Language",
    "LOC" = "LOC",
    "FixCount" = "Fix\nCount",
    "Runtime" = "Runtime",
    "Algorithm" = "Algorithm",
    "EvaluationMetrics" = "Evaluation\nMetrics"
  )
  
  
)

ggdag(Runtime_dag, text = FALSE, use_labels = "label", layout="circle") +
  theme_dag()
ggdag_paths(Runtime_dag,
            text = FALSE,
            use_labels = "label",
            shadow = TRUE,
            layout = "circle") +
  theme_dag()

ggdag_paths(Runtime_dag,
            text = FALSE,
            use_labels = "label",
            shadow = TRUE,adjust_for = c("LOC", "Project", "Language"),
            layout="circle") +
  theme_dag()


dag_adjustment_sets(Runtime_dag)
ggdag_adjustment_set(Runtime_dag,
                     text = FALSE,
                     use_labels = "label",
                     shadow = TRUE) +
  theme_dag()

isAdjustmentSet(Runtime_dag, c("LOC", "Project", "Language"))

ggdag_dseparated(
  Runtime_dag,
  from = "Algorithm",
  to = "Runtime",
  controlling_for = c("Language", "Project", "FixCount", "LOC")
)



rt1 = brm(
  formula = Runtime | trunc(lb = 104) ~ 1 + LOC,
  data = d,
  family = gaussian(),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta = 0.9999, max_treedepth = 13),
  seed = SEED
)

# Same as before, as long as we don't condition on the evaluation metrics,
# Algorithm and Runtime are d-separated.
################################################################################
#
# Lets start with simulating some data


setwd("~/Documents/linespots/linespots-analysis/Paper/data/")

d = read_delim(
  'full_evaluation.csv',
  delim = ",",
  locale = locale(decimal_mark = "."),
  col_names = TRUE,
  col_types = cols(
    AUCEC1 = col_double(),
    AUCEC100 = col_double(),
    AUCEC20 = col_double(),
    AUCEC5 = col_double(),
    AUROC = col_double(),
    Algorithm = col_factor(),
    Depth = col_double(),
    EInspect10 = col_double(),
    EInspect100 = col_double(),
    EInspect200 = col_double(),
    EInspect50 = col_double(),
    EInspectF = col_double(),
    EXAM = col_double(),
    FixCount = col_double(),
    Future = col_double(),
    LOC = col_double(),
    Origin = col_double(),
    Project = col_factor(),
    Runtime = col_double(),
    Time = col_factor(),
    Weight = col_factor(),
    comit_version = col_factor(),
    commits = col_double(),
    language = col_factor(),
    url = col_factor()
  )
)

foo = subset(d, d$FixCount == 0)
d = subset(d, d$FixCount != 0)


# Standardizing
d$Commits = scale(d$commits)
d$LOC = scale(d$LOC)
d$Origin = scale(d$Origin)
d$Runtime = scale(d$Runtime)

ggplot(d, aes(x = Algorithm, y = Runtime)) + geom_boxplot()
ggplot(d, aes(x = Algorithm, y = AUCEC5)) + geom_boxplot()
ggplot(d, aes(x = Algorithm, y = EInspect200)) + geom_boxplot()
ggplot(d, aes(x = comit_version, y = EInspect200)) + geom_point()
ggplot(d, aes(x = comit_version, y = FixCount)) + geom_point()
ggplot(d, aes(x = Algorithm, y = Runtime)) + geom_boxplot()
ggplot(d, aes(x = Weight, y = AUCEC5)) + geom_boxplot()
ggplot(d, aes(x = Weight, y = EXAM)) + geom_boxplot()
ggplot(d, aes(x = Weight, y = EInspect100)) + geom_boxplot()
ggplot(d, aes(x = Weight, y = EInspectF)) + geom_boxplot()
ggplot(d, aes(x = Time, y = AUCEC5)) + geom_boxplot()
ggplot(d, aes(x = Time, y = EXAM)) + geom_boxplot()
ggplot(d, aes(x = Time, y = EInspect100)) + geom_boxplot()
ggplot(d, aes(x = Time, y = EInspectF)) + geom_boxplot()



# RQ: What kind of weighting function produces the best results for Linespots?

# For this question we are only interested in the behaviour of linespots.
ls.df = subset(d, d$Algorithm == "Linespots")

# Lets look at some boxplots first
pdf("rq1-box-exam.pdf")
ggplot(d, aes(x = Weight, y = AUROC)) + geom_boxplot()
dev.off()
# All three weighting functions seem to produce very similar EXAM scores.
# The flat one seems to have the lowest median slightly.
pdf("rq1-box-aucec.pdf")
ggplot(d, aes(x = Time, y = AUROC)) + geom_boxplot()
dev.off()



m1 = brm(
  formula = EInspect200 ~ 1 + Algorithm + (1 | Project) + (1 |
                                                             language),
  data = d,
  family = poisson(),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta = 0.9999, max_treedepth = 13),
  seed = SEED
)

m2 = brm(
  formula = Runtime ~ 1 + Algorithm + (1 | Project) + (1 | language),
  data = d,
  family = gaussian(),
  iter = 10000,
  warmup = 2500,
  chains = 4,
  cores = parallel::detectCores(),
  sample_prior = TRUE,
  control = list(adapt_delta = 0.9999, max_treedepth = 13),
  seed = SEED
)
