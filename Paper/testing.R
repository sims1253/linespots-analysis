library(loo)       # For comparing different models' performance
library(tidyverse) # For transforming and visualizing data.
library(ggridges)
library(gridExtra) # Combining many plots into the same figure.
library(brms)      # BDA packages. Alternatively, one can use rethinking & rstanarm.
library(bayesplot) # Plotting BDA output by Gabry et al.
library(ggthemes)  # Themes for ggplot
library(patchwork)
ggplot2::theme_set(theme_tufte())
bayesplot::color_scheme_set("viridis") #Uses the viridis palette on bayesplots.

library(projpred)
library(extraDistr)
library(skimr)

SAMPLES = 2000
WARMUP = 500
CHAINS = 4
SEED = 2020
DELTA = 0.99
TREE = 13
set.seed(SEED)
options(mc.cores = parallel::detectCores())

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

d = subset(d, d$FixCount != 0)
d$LOC = scale(d$LOC)
d$FixCount = scale(d$FixCount)
############################################################################

sln1 = brm(
  bf(Runtime ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Project * Algorithm * LOC) + (Project * Algorithm * FixCount)),
  data = d,
  family = shifted_lognormal(),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

sln2 = brm(
  bf(Runtime ~ 1 + Algorithm + LOC + FixCount + (1 | Project) + (Project * Algorithm * FixCount)),
  data = d,
  family = shifted_lognormal(),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

sln3 = brm(
  bf(Runtime ~ 1 + Algorithm + LOC + FixCount + (Algorithm | Project)),
  data = d,
  family = shifted_lognormal(),
  init = 0,
  iter = SAMPLES,
  warmup = WARMUP,
  chains = CHAINS,
  cores = parallel::detectCores(),
  sample_prior = FALSE,
  control = list(adapt_delta = DELTA, max_treedepth = TREE),
  seed = SEED
)

loo_compare(loo(sln1), loo(sln2), loo(sln3))
