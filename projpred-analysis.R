library(tidyverse)
library(projpred)
library(bayesplot)
library(rstanarm)
options(mc.cores = parallel::detectCores())

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

# Moving to log scale and standardizing
d$Commits = (log(d$Commits) - mean(log(d$Commits))) / sd(log(d$Commits))
d$LOC = (log(d$LOC) - mean(log(d$LOC))) / sd(log(d$LOC))
d$Origin = (log(d$Origin) - mean(log(d$Origin))) / sd(log(d$Origin))


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

n = 294 # Rows in the ls.df frame
D = 9 # Predictors in the model, Algorithm is left out as we only look at Linespots
p0 = 3  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic

# Splitting these up, as the models wouldn't converge with too many predictors
# So far I haven't gotten this to work due to model building problems.
projpred1 = stan_glm(exam ~ predictors,
                    family = gaussian(), data=ls.data, prior = prior_coeff,
                    chains = 4, iter = 2000)

projpred2 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = ls.data, prior = prior_coeff,
                     chains = 4, iter = 2000)


vs1 = varsel(projpred1, method='forward')
vs1$vind
varsel_plot(vs1, stats=c('elpd', 'rmse'))
cvs1 = cv_varsel(projpred1, method = 'forward')
suggest_size(cvs1)
varsel_plot(cvs1, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred1),
           pars = c(names(cvs1$vind[1:4]))) + coord_cartesian(xlim = c(-0.1, 0.1))

vs2 = varsel(projpred2, method='forward')
vs2$vind
varsel_plot(vs2, stats=c('elpd', 'rmse'))
cvs2 = cv_varsel(projpred2, method = 'forward')
suggest_size(cvs2)
varsel_plot(cvs2, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred2),
           pars = c(names(cvs2$vind[1:4]))) + coord_cartesian(xlim = c(-0.1, 0.1))



# For the fourth research question we need both linespots and bugspots

predictors = tibble(d$Commits, d$Domain, d$LOC, d$Origin, d$Project, d$Source, d$Choice, d$Time, d$Weighting)
predictors = matrix(as.numeric(unlist(predictors)),nrow=nrow(predictors))

exam = tibble(d$EXAM)
exam = matrix(as.numeric(unlist(exam)),nrow=nrow(exam))

aucec = tibble(d$AUCEC)
aucec = matrix(as.numeric(unlist(aucec)),nrow=nrow(aucec))

bs.data = tibble(predictors, exam, aucec)

n = 588 # Rows in the d frame
D = 10 # Predictors in the model
p0 = 3  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic

# Splitting these up, as the models wouldn't converge with too many predictors
# So far I haven't gotten this to work due to model building problems.
projpred3 = stan_glm(exam ~ predictors,
                    family = gaussian(), data=bs.data, prior = prior_coeff,
                    chains = 4, iter = 2000)

projpred4 = stan_glm(aucec ~ predictors,
                     family = gaussian(), data = bs.data, prior = prior_coeff,
                     chains = 4, iter = 2000)

vs3 = varsel(projpred3, method='forward')
vs3$vind
varsel_plot(vs3, stats=c('elpd', 'rmse'))
cvs3 = cv_varsel(projpred3, method = 'forward')
suggest_size(cvs3)
varsel_plot(cvs3, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred3),
           pars = c(names(cvs3$vind[1:4]))) + coord_cartesian(xlim = c(-0.1, 0.1))

vs1 = varsel(projpred4, method='forward')
vs4$vind
varsel_plot(vs4, stats=c('elpd', 'rmse'))
cvs4 = cv_varsel(projpred4, method = 'forward')
suggest_size(cvs4)
varsel_plot(cvs4, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred4),
           pars = c(names(cvs4$vind[1:4]))) + coord_cartesian(xlim = c(-0.1, 0.1))
