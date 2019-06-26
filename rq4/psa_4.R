library(extraDistr)

# First model was:
#  EXAM ~ 1 + Algorithm + (1|Project),

sensitivity1 <- function(intercept, algorithm, project) {
  set.seed(4082)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_algorithm = rnorm(N, 0, algorithm)
  sd_project = rhcauchy(N, project)
  
  b_project = rep(0, N)
  for (i in 1:N){
    b_project[i] = rnorm(1, 0, sd_project[i])
  }
  
  # Reasoning for x limits:
  # The lowest value any of the predictors takes is -0.7 in the dataset.
  # The highest value any of the predictors takes is 23 in the dataset.
  plot(NULL, xlim=c(0,23), ylim=c(0, 1.5))
  for ( i in 1:N ){
    curve(exp(
      (b_intercept[i] + b_algorithm[i]*x + b_project[i]*x)/(
        1 + exp(b_intercept[i] + b_algorithm[i]*x + b_project[i]*x))) , add=TRUE)
  }
}

sensitivity1(10, 10, 10)
sensitivity1(1, 10, 10)  # Lower intercept sd seems to make it worse
sensitivity1(10, 1, 10)  # Lower algorithm sd seems to make it better
sensitivity1(10, 10, 1)  # Lower project sd seems to make it better
dev.off()

sensitivity1(10, 1, 1)     # New benchmark
sensitivity1(10, 0.1, 1)   # Hard to tell but might look slightly better
sensitivity1(10, 1, 0.1)   # Again, hard to tell but maybe slightly better
sensitivity1(10, 0.1, 0.1) # The combination of both definitely looks better than before
dev.off()

sensitivity1(10, 0.1, 0.1) 
sensitivity1(10, 0.01, 0.1) # Looks better
sensitivity1(10, 0.1, 0.01) # Looks better
sensitivity1(10, 0.01, 0.01)# Looks better, but almost too smooth
# Trying a middle solution that is not too smooth
sensitivity1(10, 0.05, 0.05)
# This looks like a good middle ground. 



##############################################################################################
# Second model was:
# AUCEC ~ 1 + Algorithm + (1|Project) + (1|Domain) + Origin

sensitivity2 <- function(intercept, algorithm, project, domain, origin) {
  set.seed(4082)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_algorithm = rnorm(N, 0, algorithm)
  sd_project = rhcauchy(N, project)
  sd_domain = rhcauchy(N, domain)
  b_origin = rnorm(N, 0, origin)
  
  b_project = rep(0, N)
  for (i in 1:N){
    b_project[i] = rnorm(1, 0, sd_project[i])
  }
  
  b_domain = rep(0, N)
  for (i in 1:N){
    b_domain[i] = rnorm(1, 0, sd_domain[i])
  }
  
  # Reasoning for x limits:
  # The lowest value any of the predictors takes is -0.7 in the dataset.
  # The highest value any of the predictors takes is 23 in the dataset.
  plot(NULL, xlim=c(0,23), ylim=c(0, 1.5))
  for ( i in 1:N ){
    curve(exp(
      (b_intercept[i] + b_algorithm[i]*x + b_project[i]*x + b_domain[i]*x + b_origin[i]*x)/(
        1 + exp(b_intercept[i] + b_algorithm[i]*x + b_project[i]*x + b_domain[i]*x + b_origin[i]*x))) , add=TRUE)
  }
}

sensitivity2(10, 10, 10, 10, 10)
sensitivity2(1, 10, 10, 10, 10) # Seems to make it worse
sensitivity2(10, 1, 10, 10, 10) # Seems to make it worse
sensitivity2(10, 10, 1, 10, 10) # Seems to make it worse
sensitivity2(10, 10, 10, 1, 10) # Seems to make it worse
sensitivity2(10, 10, 10, 10, 1) # Seems to make it worse
# It might be hard to tell so I still try a combination of all and a combination of all but the intercept
sensitivity2(10, 1, 1, 1, 1) # A lot better
sensitivity2(1, 1, 1, 1, 1) # Better but not as good as the one before
dev.off()

sensitivity2(10, 1, 1, 1, 1)
sensitivity2(10, 0.1, 1, 1, 1)
sensitivity2(10, 1, 0.1, 1, 1)
sensitivity2(10, 1, 1, 0.1, 1)
sensitivity2(10, 1, 1, 1, 0.1)
sensitivity2(10, 0.1, 0.1, 0.1, 0.1)
# All the individual changes again are hard to tell but combined look a lot better than before
dev.off()

sensitivity2(10, 0.1, 0.1, 0.1, 0.1)
sensitivity2(10, 0.01, 0.1, 0.1, 0.1)
sensitivity2(10, 0.1, 0.01, 0.1, 0.1)
sensitivity2(10, 0.1, 0.1, 0.01, 0.1)
sensitivity2(10, 0.1, 0.1, 0.1, 0.01)
sensitivity2(10, 0.01, 0.01, 0.01, 0.01)
# Again, all individual ones look good but the final one seems too flat.

sensitivity2(10, 0.05, 0.05, 0.05, 0.05)
# This looks slightly better than the 0.1 version and still allows for more movement than 0.01
