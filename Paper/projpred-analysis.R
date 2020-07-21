library(tidyverse)
library(projpred)
library(bayesplot)
library(rstanarm)
library(gridExtra)
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
d = subset(d, d$FixCount != 0)

# Standardizing
d$LOC = scale(d$LOC)
d$FixCount = scale(d$FixCount)
d$Origin = scale(d$Origin)
d$commits = scale(d$commits)
d$Future = scale(d$Future)

predictors = tibble(d$LOC, d$language, d$Project)
predictors = matrix(as.numeric(unlist(predictors)),nrow=nrow(predictors))

exam = tibble(d$EXAM)
exam = matrix(as.numeric(unlist(exam)),nrow=nrow(exam))

AUCEC5 = tibble(d$AUCEC5)
AUCEC5 = matrix(as.numeric(unlist(AUCEC5)),nrow=nrow(AUCEC5))

EInspect100 = tibble(d$EInspect100)
EInspect100 = matrix(as.numeric(unlist(EInspect100)),nrow=nrow(EInspect100))

AUROC = tibble(d$AUROC)
AUROC = matrix(as.numeric(unlist(AUROC)),nrow=nrow(AUROC))

bs.data = tibble(predictors, exam, AUCEC5, EInspect100, AUROC)

n = nrow(bs.data) # Rows in the d frame
D = ncol(predictors) # Predictors in the model
p0 = 1  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic


projpred3 = stan_glm(EInspect100 ~ predictors,
                    family = gaussian(), data=bs.data, prior = prior_coeff,
                    chains = 4, iter = 4000, seed = SEED)
summary(projpred3) # Rhat and n_eff look good

cvs3 = cv_varsel(projpred3, method = 'forward')
cvs3$vind
suggest_size(cvs3)
p1 = varsel_plot(cvs3, stats = c('elpd', 'rmse'), deltas=T)
p2 = mcmc_areas(as.matrix(projpred3), pars = c('(Intercept)', names(cvs3$vind[1:suggest_size(cvs3)]), 'sigma'))
pdf("projpred-3.pdf")
grid.arrange(p1, p2, ncol=1)
dev.off()
# Based on this, the exam score for both algorithms is best predicted by:
# LOC, Project, Origin, Language


projpred4 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred4) # Rhat and n_eff look good

cvs4 = cv_varsel(projpred4, method = 'forward')
cvs4$vind
suggest_size(cvs4)
p1 = varsel_plot(cvs4, stats = c('elpd', 'rmse'), deltas=T)
p2 = mcmc_areas(as.matrix(projpred4), pars = c('(Intercept)', names(cvs4$vind[1:suggest_size(cvs4)]), 'sigma'))
pdf("projpred-4.pdf")
grid.arrange(p1, p2, ncol=1)
dev.off()
# Based on this, the AUCEC score for both algorithms is best predicted by:
# LOC, Project, Origin, Language


projpred5 = stan_glm(exam25 ~ predictors,
                     family = gaussian(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred5) # Rhat and n_eff look good

cvs5 = cv_varsel(projpred5, method = 'forward')
cvs5$vind
suggest_size(cvs5)
p1 = varsel_plot(cvs5, stats = c('elpd', 'rmse'), deltas=T)
# While suggest_size suggests 9 predictors, varsel_plot makes it seem like 4 or 5 might be a good point instead.
# That would give LOC, Algorithm, Project, Language and maybe Domain as predictors.
# The mcmc_area plot also shows Origin and commit to have noticable effects.
p2 = mcmc_areas(as.matrix(projpred5), pars = c('(Intercept)', names(cvs5$vind[1:suggest_size(cvs5)]), 'sigma'))
pdf("projpred-5.pdf")
grid.arrange(p1, p2, ncol=1)
dev.off()

projpred6 = stan_glm(EInspect25EXAM ~ predictors,
                     family = poisson(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED, adapt_delta=0.99999)
summary(projpred6) # Rhat and n_eff

cvs6 = cv_varsel(projpred6, method = 'forward')
cvs6$vind
suggest_size(cvs6)
p1 = varsel_plot(cvs6, stats = c('elpd', 'rmse'), deltas=T)
# While suggest_size suggests 9 predictors, varsel_plot makes it seem like 5 or 6 might be a good point instead.
# That would give Algorithm, Domain, Language, Project, LOC as predictors.


mcmc_areas(as.matrix(projpred6), pars = c('(Intercept)', names(cvs6$vind[1:suggest_size(cvs6)])))
p2 = mcmc_areas(as.matrix(projpred6), pars = c(names(cvs6$vind[1:suggest_size(cvs6)])))
pdf("projpred-6.pdf")
grid.arrange(p1, p2, ncol=1)
dev.off()

save(projpred1, projpred2, projpred3, projpred4, projpred5, projpred6,
     cvs1, cvs2, cvs3, cvs4, cvs5, cvs6, file="projpred.RData")
