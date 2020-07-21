library(loo)       # For comparing different models' performance
library(tidyverse) # For transforming and visualizing data.
library(ggridges)
library(gridExtra) # Combining many plots into the same figure.
library(brms)      # BDA packages. Alternatively, one can use rethinking & rstanarm.
library(bayesplot) # Plotting BDA output by Gabry et al.
library(ggthemes)  # Themes for ggplot
library(patchwork)
ggplot2::theme_set(theme_tufte(base_size = 30))
bayesplot::color_scheme_set("gray") #Uses the viridis palette on bayesplots.
library(ggdag)
library(projpred)
library(extraDistr)
library(ggforce)

SAMPLES = 5000
WARMUP = 1000
CHAINS = 4
SEED = 2020
DELTA = 0.99
TREE = 13
set.seed(SEED)
options(mc.cores = parallel::detectCores())

setwd("~/Documents/linespots/linespots-analysis/Paper/data/")
load(file = "plot-models.RData")
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
d = subset(d, d$FixCount != 0)
d$EInspectF = ceiling(d$EInspectF * d$LOC)
d$LOC = scale(d$LOC)
d$FixCount = scale(d$FixCount)
d$Quality <- ifelse(d$comit_version == "None", "Base", "Good")




############################################################################
# AUROC Plots
mauroc1= brm(
  formula = AUROC ~ 1 + Algorithm + LOC + FixCount + (1 | Project),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("auroc-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(mauroc1,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(mauroc1, effects = c("Algorithm"))
pdf("auroc-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(mauroc1, effects = c("Algorithm"))$Algorithm
conditional_effects(mauroc1, effects = c("Algorithm"), robust = FALSE)$Algorithm

mauroc2= brm(
  formula = AUROC ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (1 + Algorithm | Quality),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

mauroc3= brm(
  formula = AUROC ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (1 + Algorithm | comit_version),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

loo_compare(loo(mauroc1), loo(mauroc2), loo(mauroc3))
mcmc_areas(mauroc3, pars = c("r_comit_version[None,AlgorithmBugspots]",
                             "r_comit_version[discourse,AlgorithmBugspots]",
                             "r_comit_version[angular,AlgorithmBugspots]",
                             "r_comit_version[conventional,AlgorithmBugspots]",
                             "b_AlgorithmBugspots"), prob = 0.95)
basic = posterior_samples(mauroc1, pars = "Algorithm")
good = posterior_samples(mauroc2, pars = "Algorithm")
tmp = data.frame("Bugspots" = c(basic$b_AlgorithmBugspots, good$b_AlgorithmBugspots),
                 "Quality" = c(
                   rep("Basic", nrow(basic)),
                   rep("Good", nrow(good))
                   )
                 )
ggplot(tmp, aes(x = Bugspots, y = Quality)) +
  geom_density_ridges()
conditional_effects(mauroc1, effects = "Algorithm")
conditional_effects(mauroc2, effects = "Algorithm")

############################################################################
# EXAM Plots
mexam3p = brm(
  formula = EXAM ~ 1 + Algorithm + LOC + FixCount + (1 | Project),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-1, 1), class=Intercept),
    prior(normal(0, 0.15), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = "only",
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("ppcheck.pdf", width = 7, height = 3.5)
pp_check(mexam3p, nsamples = 100) + scale_y_continuous(trans='sqrt')
dev.off()


mexam1= brm(
  formula = EXAM ~ 1 + Algorithm + LOC + FixCount + (1 | Project),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-1, 1), class=Intercept),
    prior(normal(0, 0.15), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("exam-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(mexam1,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(mexam1, effects = c("Algorithm"))
pdf("exam-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(mexam1, effects = c("Algorithm"))$Algorithm
conditional_effects(mexam1, effects = c("Algorithm"), robust = FALSE)$Algorithm

mexam2= brm(
  formula = EXAM ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | Quality),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-1, 1), class=Intercept),
    prior(normal(0, 0.15), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

mexam3= brm(
  formula = EXAM ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | comit_version),
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-1, 1), class=Intercept),
    prior(normal(0, 0.15), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)


mexam4= brm(
  formula = EXAM ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + comit_version,
  data = d,
  family = Beta(),
  prior = c(
    prior(normal(-1, 1), class=Intercept),
    prior(normal(0, 0.15), class=b),
    prior(weibull(2, 1), class=sd),
    prior(normal(50, 20), class=phi)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)


loo_compare(loo(mexam1), loo(mexam2), loo(mexam3), loo(mexam4))
a = mcmc_areas(mexam3, pars = c( "r_comit_version[None,AlgorithmBugspots]",
                                 #"r_comit_version[Good,AlgorithmBugspots]",
                                 "r_comit_version[conventional,AlgorithmBugspots]",
                                 "r_comit_version[discourse,AlgorithmBugspots]",
                                 "r_comit_version[angular,AlgorithmBugspots]"
                                 ),
                               prob = 0.95) +
  scale_y_discrete(labels=c("r_comit_version[None,AlgorithmBugspots]" = "none",
                            #"r_comit_version[Good,AlgorithmBugspots]" = "Good",
                            "r_comit_version[conventional,AlgorithmBugspots]" = "conventional",
                            "r_comit_version[discourse,AlgorithmBugspots]" = "discourse",
                            "r_comit_version[angular,AlgorithmBugspots]" = "angular"
                                          
                                          ))

b = plot(conditional_effects(mexam4, effects = "comit_version",
                             conditions = data.frame("Algorithm" = c("Bugspots", "Linespots"))),
                             plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("None" = "base",
                              #"Good" = "good",
                              "conventional" = "conventional",
                              "discourse" = "discourse",
                              "angular" = "angular")) +
  scale_y_continuous(breaks = c(0.175, 0.225, 0.275)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
pdf("comit-versions-1.pdf", width=7, height = 4.5)
b
dev.off()
pdf("comit-versions-2.pdf", width = 7, height = 4.5)
a
dev.off()



############################################################################
# AUCEC5 Plots

maucec51 = brm(
  bf(AUCEC5 ~ 1 + Algorithm + LOC + FixCount + (1 | Project), zi ~ Algorithm + (1 | Project)),
  data = d,
  family = zero_inflated_beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(normal(0, 2), class=b, dpar="zi"),
    prior(normal(50, 20), class=phi)
    
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("aucec5-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(maucec51,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(maucec51, effects = c("Algorithm"))
pdf("aucec5-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(maucec51, effects = c("Algorithm"))$Algorithm
conditional_effects(maucec51, effects = c("Algorithm"), robust = FALSE)$Algorithm

maucec52 = brm(
  bf(AUCEC5 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | Quality), zi ~ Algorithm + (1 | Project)),
  data = d,
  family = zero_inflated_beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(normal(0, 2), class=b, dpar="zi"),
    prior(normal(50, 20), class=phi)
    
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

maucec53 = brm(
  bf(AUCEC5 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | comit_version), zi ~ Algorithm + (1 | Project)),
  data = d,
  family = zero_inflated_beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(normal(0, 2), class=b, dpar="zi"),
    prior(normal(50, 20), class=phi)
    
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
loo_compare(loo(maucec51), loo(maucec52), loo(maucec53))
mcmc_areas(maucec53, pars = c("r_comit_version[None,AlgorithmBugspots]",
                            "r_comit_version[discourse,AlgorithmBugspots]",
                            "r_comit_version[angular,AlgorithmBugspots]",
                            "r_comit_version[conventional,AlgorithmBugspots]",
                            "b_AlgorithmBugspots"), prob = 0.95)
############################################################################
# AUCEC1 Plots
maucec11 = brm(
  bf(AUCEC1 ~ 1 + Algorithm + LOC + FixCount + (1 | Project), zi ~ Algorithm + (1 | Project)),
  data = d,
  family = zero_inflated_beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(normal(0, 2), class=b, dpar="zi"),
    prior(normal(50, 20), class=phi)
    
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("aucec1-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(maucec11,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(maucec11, effects = c("Algorithm"))
pdf("aucec1-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(maucec11, effects = c("Algorithm"))$Algorithm
conditional_effects(maucec11, effects = c("Algorithm"), robust = FALSE)$Algorithm

maucec12 = brm(
  bf(AUCEC1 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | Quality), zi ~ Algorithm + (1 | Project)),
  data = d,
  family = zero_inflated_beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(normal(0, 2), class=b, dpar="zi"),
    prior(normal(50, 20), class=phi)
    
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

maucec13 = brm(
  bf(AUCEC1 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | comit_version), zi ~ Algorithm + (1 | Project)),
  data = d,
  family = zero_inflated_beta(),
  prior = c(
    prior(normal(-2, 1), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(normal(0, 2), class=b, dpar="zi"),
    prior(normal(50, 20), class=phi)
    
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

loo_compare(loo(maucec11), loo(maucec12), loo(maucec13))
mcmc_areas(maucec13, pars = c("r_comit_version[None,AlgorithmBugspots]",
                              "r_comit_version[discourse,AlgorithmBugspots]",
                              "r_comit_version[angular,AlgorithmBugspots]",
                              "r_comit_version[conventional,AlgorithmBugspots]",
                              "b_AlgorithmBugspots"), prob = 0.95)

############################################################################
# EInspect100 Plots

mEInspect1001 = brm(
  formula = EInspect100 ~ 1 + Algorithm + LOC + FixCount + (1 | Project),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(-3, 0.5), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("einspect100-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(mEInspect1001,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(mEInspect1001, effects = c("Algorithm"))
pdf("einspect100-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(mEInspect1001, effects = c("Algorithm"))$Algorithm
conditional_effects(mEInspect1001, effects = c("Algorithm"), robust = FALSE)$Algorithm

mEInspect1002 = brm(
  formula = EInspect100 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | Quality),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(-3, 0.5), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

mEInspect1003 = brm(
  formula = EInspect100 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | comit_version),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(-3, 0.5), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
loo_compare(loo(mEInspect1001), loo(mEInspect1002), loo(mEInspect1003))
mcmc_areas(mEInspect1003, pars = c("r_comit_version[None,AlgorithmBugspots]",
                                   "r_comit_version[discourse,AlgorithmBugspots]",
                                   "r_comit_version[angular,AlgorithmBugspots]",
                                   "r_comit_version[conventional,AlgorithmBugspots]",
                                   "b_AlgorithmBugspots"), prob = 0.95)

############################################################################
# EInspect10 Plots

mEInspect101 = brm(
  formula = EInspect10 ~ 1 + Algorithm + LOC + FixCount + (1 | Project),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(-4, 0.5), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("einspect10-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(mEInspect101,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(mEInspect101, effects = c("Algorithm"))
pdf("einspect10-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(mEInspect101, effects = c("Algorithm"))$Algorithm
conditional_effects(mEInspect101, effects = c("Algorithm"), robust = FALSE)$Algorithm

mEInspect102 = brm(
  formula = EInspect10 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | Quality),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(-4, 0.5), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

mEInspect103 = brm(
  formula = EInspect10 ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | comit_version),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(-4, 0.5), class=Intercept),
    prior(normal(0, 0.25), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
loo_compare(loo(mEInspect101), loo(mEInspect102), loo(mEInspect103))
mcmc_areas(mEInspect103, pars = c("r_comit_version[None,AlgorithmBugspots]",
                              "r_comit_version[discourse,AlgorithmBugspots]",
                              "r_comit_version[angular,AlgorithmBugspots]",
                              "r_comit_version[conventional,AlgorithmBugspots]",
                              "b_AlgorithmBugspots"), prob = 0.95)

############################################################################
# EInspectF Plots

mEInspectF1 = brm(
  formula = EInspectF ~ 1 + Algorithm + LOC + FixCount + (1 | Project),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(6, 1), class=Intercept),
    prior(normal(0, 1), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("einspectf-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(mEInspectF1,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(mEInspectF1, effects = c("Algorithm"))
pdf("einspectf-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(mEInspectF1, effects = c("Algorithm"))$Algorithm
conditional_effects(mEInspectF1, effects = c("Algorithm"), robust = FALSE)$Algorithm

mEInspectF2 = brm(
  formula = EInspectF ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | Quality),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(6, 1), class=Intercept),
    prior(normal(0, 1), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

mEInspectF3 = brm(
  formula = EInspectF ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Algorithm | comit_version),
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(6, 1), class=Intercept),
    prior(normal(0, 1), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

mEInspectF4 = brm(
  formula = EInspectF ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + comit_version,
  data = d,
  family = negbinomial(),
  prior = c(
    prior(normal(6, 1), class=Intercept),
    prior(normal(0, 1), class=b),
    prior(weibull(2, 1), class=sd),
    prior(weibull(2, 1), class=shape)
  ),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

loo_compare(loo(mEInspectF1), loo(mEInspectF2), loo(mEInspectF3), loo(mEInspectF4))
e = mcmc_areas(mEInspectF3, pars = c( "r_comit_version[None,AlgorithmBugspots]",
                                 #"r_comit_version[Good,AlgorithmBugspots]",
                                 "r_comit_version[conventional,AlgorithmBugspots]",
                                 "r_comit_version[discourse,AlgorithmBugspots]",
                                 "r_comit_version[angular,AlgorithmBugspots]"
),
prob = 0.95) +
  scale_y_discrete(labels=c("r_comit_version[None,AlgorithmBugspots]" = "none",
                            #"r_comit_version[Good,AlgorithmBugspots]" = "Good",
                            "r_comit_version[conventional,AlgorithmBugspots]" = "conventional",
                            "r_comit_version[discourse,AlgorithmBugspots]" = "discourse",
                            "r_comit_version[angular,AlgorithmBugspots]" = "angular"
                            
  ))

f = plot(conditional_effects(mEInspectF4, effects = "comit_version",
                             conditions = data.frame("Algorithm" = c("Bugspots", "Linespots"))), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("None" = "n",
                              #"Good" = "g",
                              "conventional" = "c",
                              "discourse" = "d",
                              "angular" = "a")) +
  theme(axis.title.x = element_blank())
pdf("comit-versions2.pdf", width=14, height = 7)
f + e + plot_annotation(tag_levels = c('a'), tag_prefix = '(',
                        tag_suffix = ')')
dev.off()

############################################################################
# Runtime Plots
msln3 = brm(
  bf(Runtime ~ 1 + Algorithm + LOC + FixCount + (1 | Project)),
  d,
  family = mixture(shifted_lognormal(), shifted_lognormal()),
  prior = c(
    prior(normal(-2, 2), class = "Intercept", dpar = "mu1"),
    prior(normal(2, 2), class = "Intercept", dpar = "mu2"),
    prior(normal(0, 4), class = "b", dpar ="mu1"),
    prior(normal(0, 4), class = "b", dpar ="mu2"),
    prior(uniform(0, 0.005), class = "ndt1"),
    prior(uniform(0, 210), class = "ndt2"),
    prior(gamma(0.1, 0.01), class = "sd", dpar = "mu1"),
    prior(gamma(0.1, 0.01), class = "sd", dpar = "mu2"),
    prior(gamma(0.1, 0.01), class = "sigma1"),
    prior(gamma(0.1, 0.01), class = "sigma2")
  ),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("runtime-res-1.pdf", width = 7, height = 3.5)
mcmc_areas(msln3,prob = 0.95, pars=c("b_mu1_AlgorithmBugspots", "b_mu2_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(msln3, effects = c("Algorithm"))
pdf("runtime-res-2.pdf", width = 7, height = 3.5)
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()
conditional_effects(msln3, effects = c("Algorithm"))$Algorithm
conditional_effects(msln3, effects = c("Algorithm"), robust = FALSE)$Algorithm


############################################################################
foo = brm(
  formula = FixCount ~ 1 + comit_version + (1 | Project) + (1 | language) + Future + LOC,
  data = d,
  family = negbinomial(),
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)
pdf("test.pdf")
mcmc_areas(foo,prob = 0.95, pars=c("b_AlgorithmBugspots")) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
dev.off()
a = conditional_effects(foo, effects = c("Algorithm"))
pdf("test2.pdf")
plot(a, plot = FALSE)[[1]] +
  theme(axis.title=element_blank())
dev.off()

hist(d$AUCEC100 - (1 - d$EXAM), breaks = 100)

mean(c(0.661, 0.582, 0.798, 0.563,0.716)) 
mean(c(0.825,0.598,0.837,0.744,0.841))
mean(c(0.85,0.738,       0.855,       0.772,       0.844))


################################################################################
# Commit Convention Comparison
tmp = subset(d, d$Quality=="Good")
tmp$comit_version = "Good"
tmp = rbind(d, tmp)
levels(tmp$comit_version) = c(levels(tmp$comit_version), "Base") 
tmp$comit_version[tmp$comit_version == "None"]  = "Base" 

tmp$comit_version <- factor(tmp$comit_version, levels = c("Base",
                                                          "Good",
                                                          "conventional",
                                                          "discourse",
                                                          "angular"))


a = ggplot(tmp, aes(x=comit_version, y=EXAM)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("Base" = "base",
                              "Good" = "good",
                              "conventional" = "conventional",
                              "discourse" = "discourse",
                              "angular" = "angular"
                              )) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

b = ggplot(tmp, aes(x = EXAM, y = comit_version)) +
  geom_density_ridges() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  scale_y_discrete(labels = c("Base" = "base",
                              "Good" = "good",
                              "conventional" = "conventional",
                              "discourse" = "discourse",
                              "angular" = "angular"
  ))
a + b
pdf("threat-dens-1.pdf", width = 7, height = 4.5)
a
dev.off()
pdf("threat-dens-2.pdf", width = 7, height = 4.5)
b
dev.off()

e = ggplot(tmp, aes(x=comit_version, y=EInspectF)) + 
  geom_boxplot() +
  scale_y_continuous(trans='log10', breaks = c(1, 10, 100, 1000, 10000, 100000), labels = c("1", "10", "100", "1k", "10k", "100k")) +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels = c("Base" = "b",
                              "Good" = "g",
                              "conventional" = "c",
                              "discourse" = "d",
                              "angular" = "a"
  ))

f = ggplot(tmp, aes(x = EInspectF, y = comit_version)) +
  geom_density_ridges() +
  scale_x_continuous(trans='log10', breaks = c(1, 10, 100, 1000, 10000, 100000), labels = c("1", "10", "100", "1k", "10k", "100k")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  scale_y_discrete(labels = c("Base" = "base",
                              "Good" = "good",
                              "conventional" = "conventional",
                              "discourse" = "discourse",
                              "angular" = "angular"))
e + f

pdf("good-bad-1.pdf", width = 14, height = 7)
(a | b) /
(e | f) + plot_annotation(tag_levels = c('a'), tag_prefix = '(',
                         tag_suffix = ')')
dev.off()


################################################################################
# DAG
Runtime_dag <- dagify(
  P ~ L,
  LOC ~ P,
  FC ~ P,
  EM ~ A + P + LOC + FC + L,
  exposure = "A",
  outcome = "EM"
)
pdf("dag.pdf", width = 6, height = 3.5)
ggdag(Runtime_dag, text = TRUE, layout="circle") +
  theme_dag()
dev.off()


################################################################################
x = c(0, -1, -2, -3)
y = inv_logit_scaled(x)
mydata = data.frame(x=x, y=y)
lineframe = data.frame(xs1=-3, xe1=-2, ys1=0, ye1=0,
                       xs2=-3, xe2=-3, ys2=-0.02, ye2=0.02,
                       xs3=-2, xe3=-2, ys3=-0.02, ye3=0.02,
                       xs4=-1, xe4=0, ys4=0, ye4=0,
                       xs5=-1, xe5=-1, ys5=-0.02, ye5=0.02,
                       xs6=-3.5, xe6=-3.5, ys6=inv_logit_scaled(-2), ye6=inv_logit_scaled(-3),
                       xs7=-3.7, xe7=-3.3, ys7=inv_logit_scaled(-3), ye7=inv_logit_scaled(-3),
                       xs8=-3.7, xe8=-3.3, ys8=inv_logit_scaled(-2), ye8=inv_logit_scaled(-2),
                       xs9=0, xe9=0, ys9=-0.02, ye9=0.02,
                       xs10=-3.5, xe10=-3.5, ys10=inv_logit_scaled(-1), ye10=inv_logit_scaled(0),
                       xs11=-3.7, xe11=-3.3, ys11=inv_logit_scaled(0), ye11=inv_logit_scaled(0),
                       xs12=-3.7, xe12=-3.3, ys12=inv_logit_scaled(-1), ye12=inv_logit_scaled(-1))
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun = function(x) inv_logit_scaled(x)


test =  p + stat_function(fun = fun, xlim = c(-4, 4)) +
  geom_segment(aes(x = xs1, y = ys1, xend = xe1, yend = ye1), data = lineframe) +
  geom_segment(aes(x = xs2, y = ys2, xend = xe2, yend = ye2), data = lineframe) +
  geom_segment(aes(x = xs3, y = ys3, xend = xe3, yend = ye3), data = lineframe) +
  geom_segment(aes(x = xs4, y = ys4, xend = xe4, yend = ye4), data = lineframe) +
  geom_segment(aes(x = xs5, y = ys5, xend = xe5, yend = ye5), data = lineframe) +
  geom_segment(aes(x = xs6, y = ys6, xend = xe6, yend = ye6), data = lineframe) +
  geom_segment(aes(x = xs7, y = ys7, xend = xe7, yend = ye7), data = lineframe) +
  geom_segment(aes(x = xs8, y = ys8, xend = xe8, yend = ye8), data = lineframe) +
  geom_segment(aes(x = xs9, y = ys9, xend = xe9, yend = ye9), data = lineframe) +
  geom_segment(aes(x = xs10, y = ys10, xend = xe10, yend = ye10), data = lineframe) +
  geom_segment(aes(x = xs11, y = ys11, xend = xe11, yend = ye11), data = lineframe) +
  geom_segment(aes(x = xs12, y = ys12, xend = xe12, yend = ye12), data = lineframe) +
  geom_point(data=mydata, aes(x=x, y=y), colour="black", size=3) + theme_gray(base_size = 30)+
  theme(axis.title = element_blank())
pdf("logistic.pdf", width = 12, height = 4)
test + facet_zoom(xlim = c(-4, 0), ylim = c(0, 0.6), zoom.size=3)
dev.off()
