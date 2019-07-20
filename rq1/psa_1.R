library(extraDistr)

# Model was:
#m1.5 = brm(
#  formula = EXAM ~ 1 + Weighting + LOC + Origin + (1|Language) + (1|Project),
#  data = ls.df,
#  family=Beta(),
#  prior = c(
#    prior(normal(0,0.5), class=Intercept),
#    prior(normal(0,0.1), class=b),
#    prior(cauchy(0,0.5), class=sd),
#    prior(gamma(0.1, 0.1), class=phi)
#  ),
#  chains = 4,
#  cores = parallel::detectCores(),
#  seed = SEED
#)
# And the same predictors for the EXAM and AUCEC score

sensitivity <- function(intercept, parameters, var_intercepts) {
  set.seed(4082)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_weighting = rnorm(N, 0, parameters)
  b_loc = rnorm(N, 0, parameters)
  b_origin = rnorm(N, 0, parameters)
  sd_language = rhcauchy(N, var_intercepts)
  sd_project = rhcauchy(N, var_intercepts)
  
  b_language = rep(0, N)
  for (i in 1:N){
    b_language[i] = rnorm(1, 0, sd_language[i])
  }
  
  b_project = rep(0, N)
  for (i in 1:N){
    b_project[i] = rnorm(1, 0, sd_project[i])
  }
  # Reasoning for x limits:
  # The lowest value any of the predictors takes is -0.7 in the dataset.
  # The highest value any of the predictors takes is 23 in the dataset.
  plot(NULL, xlim=c(-1,23), ylim=c(0, 1.5))
  for ( i in 1:N ){
    curve(exp(
      (b_intercept[i] + b_weighting[i]*x + b_loc[i]*x + b_origin[i]*x + b_language[i]*x+ b_project[i]*x)/(
        1 + exp(b_intercept[i] + b_weighting[i]*x + b_loc[i]*x + b_origin[i]*x + b_language[i]*x+ b_project[i]*x))) , add=TRUE)
  }
}

sensitivity(10, 10, 10)
sensitivity(1, 10, 10)  # Lower intercept sd seems to make it worse
sensitivity(10, 1, 10)  # Lower predictor priors make it smoother
sensitivity(10, 10, 1)  # Lower varying intercept priors make it smoother
sensitivity(10, 1, 1)   # Combined it looks a lot smoother then before.
dev.off()

sensitivity(10, 1, 1)    # New benchmark
sensitivity(10, 0.1, 1)  # Again, looks smoother
sensitivity(10, 1, 0.1)  # Also looks smoother
sensitivity(10, 0.1, 0.1) # Combined it looks a lot smoother than before.
dev.off()
# For these three it is hard to identify if any individual change is better, but together the picture looks a lot smoother.


sensitivity(10, 0.1, 0.1)  #New Benchmark
sensitivity(10, 0.01, 0.1) # Hard to tell a difference
sensitivity(10, 0.1, 0.01) # Looks smoother
sensitivity(10, 0.01, 0.01) # Looks very smooth with all three combined. Maybe too flat.
sensitivity(10, 0.05, 0.05) # This middle ground looks like a good compromise.
