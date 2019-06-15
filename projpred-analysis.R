library(tidyverse)
library(tidybayes)
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

ls.df = subset(d, d$Algorithm == "Linespots")

n = 294 # Rows in the ls.df frame
D = 4 # Predictors in the model
p0 = 2  # Prior for the ideal number of predictors
tau0 = p0/(D-p0) * 1/sqrt(n)
prior_coeff = hs(global_scale = tau0, slab_scale = 1) # horseshoe prior with the magic


# Splitting these up, as the models wouldn't converge with too many predictors
# So far I haven't gotten this to work due to model building problems.
projpred1 = stan_glm(AUCEC ~ 1 + Commits + Domain + LOC + Project,
                    family = gaussian(), data=ls.df, prior = prior_coeff,
                    chains = 4, iter = 2000)

projpred2 = stan_glm(EXAM ~ 1 + Commits + Domain + LOC + Project,
                     family = gaussian(), data = ls.df, prior = prior_coeff,
                     chains = 4, iter = 2000)

projpred3 = stan_glm(AUCEC ~ 1 + Source + Choice + Time + Weighting + Origin,
                     family = gaussian(), data=ls.df, prior = prior_coeff,
                     chains = 4, iter = 2000)

projpred4 = stan_glm(EXAM ~ 1 + Source + Choice + Time + Weighting + Origin,
                     family = gaussian(), data=ls.df, prior = prior_coeff,
                     chains = 4, iter = 2000)

vs = varsel(projpred2, method='forward')
vs$vind
varsel_plot(vs, stats=c('elpd', 'rmse'))
cvs = cv_varsel(projpred2, method = 'forward')
suggest_size(cvs)
varsel_plot(cvs, stats = c('elpd', 'rmse'), deltas=T)

mcmc_areas(as.matrix(projpred2),
           pars = c(names(vs$vind[1:3]))) + coord_cartesian(xlim = c(-2, 2))

  