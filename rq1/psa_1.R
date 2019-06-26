library(extraDistr)

# Model was:
#m1.7 = brm(
#  formula = EXAM ~ 1 + Weighting + (1|Project) + Origin,
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
#)
# And the same predictors for the EXAM score

sensitivity <- function(intercept, origin, weighting, project) {
  set.seed(4082)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_origin = rnorm(N, 0, origin)
  b_weighting = rnorm(N, 0, weighting)
  sd_project = rhcauchy(N, project)
  
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
      (b_intercept[i] + b_weighting[i]*x + b_origin[i]*x + b_project[i]*x)/(
        1 + exp(b_intercept[i] + b_weighting[i]*x + b_origin[i]*x + b_project[i]*x))) , add=TRUE)
  }
}

sensitivity(10, 10, 10, 10)
sensitivity(1, 10, 10, 10)  # Lower intercept sd seems to make it worse
sensitivity(10, 1, 10, 10)  # Lower origin sd seems to make it better
sensitivity(10, 10, 1, 10)  # Lower weighting sd seems to make it better
sensitivity(10, 10, 10, 1)  # Lower project sd seems to make it better
dev.off()

sensitivity(10, 1, 1, 1)    # New benchmark
sensitivity(10, 0.1, 1, 1)  # Hard to tell
sensitivity(10, 1, 0.1, 1)  # Looks smoother
sensitivity(10, 1, 1, 0.1)  # Looks smoother
sensitivity(10, 1, 0.1, 0.1)  # Lets compare with and without the lower origin sd as it was hard to see individually
sensitivity(10, 0.1, 0.1, 0.1)# This seems a lot smoother than the one with origin=1 so lets lower origin as well.
# For these three it is hard to identify if any individual change is better, but together the picture looks a lot smoother.
dev.off()

sensitivity(10, 0.1, 0.1, 0.1)  #New Benchmark
sensitivity(10, 0.01, 0.1, 0.1) # Looks smoother
sensitivity(10, 0.1, 0.01, 0.1) # Looks smoother
sensitivity(10, 0.1, 0.1, 0.01) # Looks smoother
sensitivity(10, 0.01, 0.01, 0.01) # Looks very smooth with all three combined.
# This might actually be too flat so lets try  another one
sensitivity(10, 0.05, 0.05, 0.05)
# This looks like a good middle ground. 
