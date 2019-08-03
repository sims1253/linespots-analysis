library(tidyverse)
library(projpred)
library(bayesplot)
library(rstanarm)
options(mc.cores = parallel::detectCores())

SEED = 140919 # I asked my girlfriend for a number

#setwd("~/Documents/dev/linespots/linespots-analysis/")
#load(file="projpred.RData")

d = read_delim('data.csv',
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


# For rq1 and 2 we are only interested in Linespots
ls.df = subset(d, d$Algorithm == "Linespots")

# Building the pure numerical dataframe needed for rstanarm
predictors = tibble(ls.df$Commits, ls.df$Domain, ls.df$LOC, ls.df$Language, ls.df$Origin, ls.df$Project, ls.df$Time, ls.df$Weighting)
predictors = matrix(as.numeric(unlist(predictors)),nrow=nrow(predictors))

exam = tibble(ls.df$EXAM)
exam = matrix(as.numeric(unlist(exam)),nrow=nrow(exam))

exam25 = tibble(ls.df$EXAM25)
exam25 = matrix(as.numeric(unlist(exam25)),nrow=nrow(exam25))

aucec = tibble(ls.df$AUCECEXAM)
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

cvs1 = cv_varsel(projpred1, method = 'forward')
cvs1$vind
suggest_size(cvs1)
varsel_plot(cvs1, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred1), pars = c('(Intercept)', names(cvs1$vind[1:suggest_size(cvs1)]), 'sigma'))
# Based on this, the exam score for the linespots algorithm is best predicted by:
# LOC, Project, Origin. Language seems to barely not make it in.


projpred2 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = ls.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred2) # Rhat and n_eff look good

vs2 = varsel(projpred2, method = 'forward')
cvs2 = cv_varsel(projpred2, method = 'forward')
cvs2$vind
suggest_size(cvs2)
varsel_plot(cvs2, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred2), pars = c('(Intercept)', names(cvs2$vind[1:suggest_size(cvs2)]), 'sigma'))
# Based on this, the AUCEC score for the linespots algorithm is best predicted by:
# LOC, Project, Origin. Language seems to barely not make it in.



# For the fourth research question we need both linespots and bugspots
predictors = tibble(d$Algorithm, d$Commits, d$Domain, d$LOC, d$Language, d$Origin, d$Project, d$Time, d$Weighting)
predictors = matrix(as.numeric(unlist(predictors)),nrow=nrow(predictors))

exam = tibble(d$EXAM)
exam = matrix(as.numeric(unlist(exam)),nrow=nrow(exam))

exam25 = tibble(d$EXAM25)
exam25 = matrix(as.numeric(unlist(exam25)),nrow=nrow(exam25))

aucec = tibble(d$AUCECEXAM)
aucec = matrix(as.numeric(unlist(aucec)),nrow=nrow(aucec))

bs.data = tibble(predictors, exam, aucec, exam25)

n = nrow(bs.data) # Rows in the d frame
D = ncol(predictors) # Predictors in the model
p0 = 3  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic


projpred3 = stan_glm(exam ~ predictors,
                    family = gaussian(), data=bs.data, prior = prior_coeff,
                    chains = 4, iter = 2000, seed = SEED)
summary(projpred3) # Rhat and n_eff look good

cvs3 = cv_varsel(projpred3, method = 'forward')
cvs3$vind
suggest_size(cvs3)
varsel_plot(cvs3, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred3), pars = c('(Intercept)', names(cvs3$vind[1:suggest_size(cvs3)]), 'sigma'))
# Based on this, the exam score for both algorithms is best predicted by:
# LOC, Project, Origin, Language


projpred4 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred4) # Rhat and n_eff look good

cvs4 = cv_varsel(projpred4, method = 'forward')
cvs4$vind
suggest_size(cvs4)
varsel_plot(cvs4, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred4), pars = c('(Intercept)', names(cvs4$vind[1:suggest_size(cvs4)]), 'sigma'))
# Based on this, the AUCEC score for both algorithms is best predicted by:
# LOC, Project, Origin, Language


projpred5 = stan_glm(exam25 ~ predictors,
                     family = gaussian(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000, seed = SEED)
summary(projpred5) # Rhat and n_eff look good

cvs5 = cv_varsel(projpred5, method = 'forward')
cvs5$vind
suggest_size(cvs5)
varsel_plot(cvs5, stats = c('elpd', 'rmse'), deltas=T)
# While suggest_size suggests 9 predictors, varsel_plot makes it seem like 4 or 5 might be a good point instead.
# That would give LOC, Algorithm, Project, Language and maybe Domain as predictors.

mcmc_areas(as.matrix(projpred5), pars = c('(Intercept)', names(cvs5$vind[1:suggest_size(cvs5)]), 'sigma'))


save(projpred1, projpred2, projpred3, projpred4, projpred5, file="projpred.RData")
