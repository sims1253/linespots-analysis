library(tidyverse)
library(projpred)
library(bayesplot)
library(rstanarm)
options(mc.cores = parallel::detectCores())

SEED = 4082 # I asked my girlfriend for a number

#setwd('.../linespots-analysis')

d = read_delim('data.csv',
               delim = ",",
               locale = locale(decimal_mark = "."),
               col_names = TRUE,
               col_types = cols(
                 AUCEC = col_double(),
                 Algorithm = col_factor(),
                 Commits = col_double(),
                 Depth = col_double(),
                 Domain = col_factor(),
                 EXAM = col_double(),
                 FixCount = col_double(),
                 Fixed = col_double(),
                 Future = col_double(),
                 LOC = col_double(),
                 Missed = col_double(),
                 Origin = col_double(),
                 Project = col_factor(),
                 Source = col_factor(),
                 Choice = col_factor(),
                 Time = col_factor(),
                 Weighting = col_factor(),
                 fn = col_double(),
                 fp = col_double(),
                 hdMax = col_double(),
                 hdMaxLOC = col_double(),
                 tn = col_double(),
                 tp = col_double()
               )
)

# Standardizing
d$Commits = (d$Commits - mean(d$Commits)) / sd(d$Commits)
d$LOC = (d$LOC - mean(d$LOC)) / sd(d$LOC)
d$Origin = (d$Origin - mean(d$Origin)) / sd(d$Origin)


# For rq1 and 2 we are only interested in Linespots
ls.df = subset(d, d$Algorithm == "Linespots")

# Building the pure numerical dataframe needed for rstanarm
predictors = tibble(ls.df$Commits, ls.df$Domain, ls.df$LOC, ls.df$Origin, ls.df$Project, ls.df$Source, ls.df$Choice, ls.df$Time, ls.df$Weighting)
predictors = matrix(as.numeric(unlist(predictors)),nrow=nrow(predictors))

exam = tibble(ls.df$EXAM)
exam = matrix(as.numeric(unlist(exam)),nrow=nrow(exam))

aucec = tibble(ls.df$AUCEC)
aucec = matrix(as.numeric(unlist(aucec)),nrow=nrow(aucec))

ls.data = tibble(predictors, exam, aucec)

n = nrow(ls.data) # Rows in ls.data
D = ncol(predictors) # Predictors in the model, Algorithm is left out as we only look at Linespots
p0 = 3  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic


projpred1 = stan_glm(exam ~ predictors,
                    family = gaussian(), data=ls.data, prior = prior_coeff,
                    chains = 4, iter = 2000, seed = SEED)
summary(projpred1) # Rhat and n_eff look good

vs1 = varsel(projpred1, method = 'forward')
vs1$vind
suggest_size(vs1)
cvs1 = cv_varsel(projpred1, method = 'forward')
cvs1$vind
suggest_size(cvs1)
varsel_plot(cvs1, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred1), pars = c('(Intercept)', names(cvs1$vind[1:suggest_size(cvs1)]), 'sigma'))
# Based on this, the exam score for the linespots algorithm is best predicted by:
# LOC, Choice and Source. The simple varsel also proposes project as a fourth predictor.


projpred2 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = ls.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred2) # Rhat and n_eff look good

vs2 = varsel(projpred2, method = 'forward')
vs2$vind
suggest_size(vs2)
cvs2 = cv_varsel(projpred2, method = 'forward')
cvs2$vind
suggest_size(cvs2)
varsel_plot(cvs2, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred2), pars = c('(Intercept)', names(cvs2$vind[1:suggest_size(cvs2)]), 'sigma'))
# Based on this, the aucec score for the linespots algorithm is best predicted by using all predictors.
# This seems wrong and the mcmc_areas plot shows, that only Origin, Loc and Source have most of their
# areas not overlapping with 0. Thus we conclude that those three are the most valuable predictors.


# For the fourth research question we need both linespots and bugspots
predictors = tibble(d$Algorithm, d$Commits, d$Domain, d$LOC, d$Origin, d$Project, d$Source, d$Choice, d$Time, d$Weighting)
predictors = matrix(as.numeric(unlist(predictors)),nrow=nrow(predictors))

exam = tibble(d$EXAM)
exam = matrix(as.numeric(unlist(exam)),nrow=nrow(exam))

aucec = tibble(d$AUCEC)
aucec = matrix(as.numeric(unlist(aucec)),nrow=nrow(aucec))

bs.data = tibble(predictors, exam, aucec)

n = nrow(bs.data) # Rows in the d frame
D = ncol(predictors) # Predictors in the model
p0 = 3  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic

# Splitting these up, as the models wouldn't converge with too many predictors
# So far I haven't gotten this to work due to model building problems.
projpred3 = stan_glm(exam ~ predictors,
                    family = gaussian(), data=bs.data, prior = prior_coeff,
                    chains = 4, iter = 2000, seed = SEED)
summary(projpred3) # Rhat and n_eff look good

vs3 = varsel(projpred3, method = 'forward')
vs3$vind
suggest_size(vs3)
cvs3 = cv_varsel(projpred3, method = 'forward')
cvs3$vind
suggest_size(cvs3)
varsel_plot(cvs3, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred3), pars = c('(Intercept)', names(cvs3$vind[1:suggest_size(cvs3)]), 'sigma'))
# Going with 5 predictors, as the cross validated varsel proposes, the best predictors for overall exam
# score in the dataset are: Algorithm, LOC, Choice, Origin, Project


projpred4 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred4) # Rhat and n_eff look good

vs4 = varsel(projpred4, method = 'forward')
vs4$vind
suggest_size(vs4)
cvs4 = cv_varsel(projpred4, method = 'forward')
suggest_size(cvs4)
varsel_plot(cvs4, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred4), pars = c('(Intercept)', names(cvs4$vind[1:suggest_size(cvs4)]), 'sigma'))
# The cv varsel proposes the four predictors: LOC, Origin, Project, Source as the best to predict overall aucec