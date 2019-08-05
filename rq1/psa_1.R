library(extraDistr)

# Models were:
# EXAM ~ 1 + Weighting + LOC + (1|Project)
# EXAM ~ 1 + Weighting + LOC + Origin + (1|Project) + (1|Domain) + (1|Language)
# and
# AUCECEXAM ~ 1 + Weighting + Origin + LOC + (1|Language) + (1|Project)
# AUCECEXAM ~ 1 + Weighting + LOC + (1|Project)

# Works for both simpler models
sensitivity1 <- function(intercept, parameters, var_intercepts) {
  set.seed(140919)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_weighting = rnorm(N, 0, parameters)
  b_loc = rnorm(N, 0, parameters)
  m_project = rhcauchy(N, var_intercepts)
  
  b_project = rep(0, N)
  for (i in 1:N) {
    b_project[i] = rnorm(1, m_project, intercept)
  }
  
  for (w in 0:1){
    plot(NULL, xlim = c(-1, 5), ylim = c(0, 1))
    for (i in 1:N) {
      curve(exp(b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_project[i]) / (
        1 + exp(b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_project[i])
      ) ,
      add = TRUE)
    }  
  }
  
}

sensitivity1(10, 10, 10)
dev.off()
sensitivity1(1, 1, 1)
dev.off()
sensitivity1(0.1, 0.1, 0.1) # This is way too small.
dev.off()
sensitivity1(1, 0.5, 1) # Seems sensible for both w
dev.off()
sensitivity1(1, 0.5, 0.5) # This seems to work as well which means we can use the same prior for all models.

# For 1 + Weighting + LOC + Origin + (1|Project) + (1|Domain) + (1|Language)
sensitivity2 <- function(intercept, parameters, var_intercepts) {
  set.seed(140919)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_weighting = rnorm(N, 0, parameters)
  b_loc = rnorm(N, 0, parameters)
  b_origin = rnorm(N, 0, parameters)
  m_language = rhcauchy(N, var_intercepts)
  m_project = rhcauchy(N, var_intercepts)
  m_domain = rhcauchy(N, var_intercepts)
  
  b_language = rep(0, N)
  for (i in 1:N) {
    b_language[i] = rnorm(1, m_language, intercept)
  }
  
  b_project = rep(0, N)
  for (i in 1:N) {
    b_project[i] = rnorm(1, m_project, intercept)
  }
  
  b_domain = rep(0, N)
  for (i in 1:N) {
    b_domain[i] = rnorm(1, m_domain, intercept)
  }
  
  for (w in 0:1){
    plot(NULL, xlim = c(-1, 5), ylim = c(0, 1))
    for (i in 1:N) {
      curve(
        exp(
          b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_origin[i] * x + b_language[i] + b_project[i] + b_domain[i]
        )
        / (
          1 + exp(
            b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_origin[i] * x + b_language[i] + b_project[i] + b_domain[i]
          )
        )
        ,
        add = TRUE
      )
    }
  }
}

sensitivity2(10, 10, 10)
dev.off()
sensitivity2(1, 1, 1)
dev.off()
sensitivity2(0.1, 0.1, 0.1) # This is way too small.
dev.off()
sensitivity2(1, 0.5, 0.5)  # This looks ok but quite a lot of weight on the 1.


# For 1 + Weighting + Origin + LOC + (1|Language) + (1|Project)
sensitivity3 <- function(intercept, parameters, var_intercepts) {
  set.seed(140919)
  N = 100
  
  b_intercept = rnorm(N, 0, intercept)
  b_weighting = rnorm(N, 0, parameters)
  b_loc = rnorm(N, 0, parameters)
  m_language = rhcauchy(N, var_intercepts)
  m_project = rhcauchy(N, var_intercepts)
  
  b_language = rep(0, N)
  for (i in 1:N) {
    b_language[i] = rnorm(1, m_language, intercept)
  }
  
  b_project = rep(0, N)
  for (i in 1:N) {
    b_project[i] = rnorm(1, m_project, intercept)
  }
  
  for (w in 0:1) {
    plot(NULL, xlim = c(-1, 5), ylim = c(0, 1))
    for (i in 1:N) {
      curve(
        exp(
          b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_language[i] + b_project[i]
        )
        / (
          1 + exp(
            b_intercept[i] + b_weighting[i] * w + b_loc[i] * x + b_language[i] + b_project[i]
          )
        )
        ,
        add = TRUE
      )
    }
  }
}

sensitivity3(10, 10, 10)
dev.off()
sensitivity3(1, 1, 1)
dev.off()
sensitivity3(0.1, 0.1, 0.1) # Again, this seems to focused around 0.5
dev.off
sensitivity3(1, 0.5, 0.5)  # This looks ok.

# Based on this we rerun the models in RQ1 with the following priors:
# prior(normal(0,1), class=Intercept),
# prior(normal(0,0.5), class=b),
# prior(cauchy(0,0.5), class=sd),
# prior(gamma(0.1, 0.1), class=phi)


sensitivity4 <- function(parameters, var_intercepts) {
  set.seed(140919)
  N = 100
  
  b_weighting = rnorm(N, 1, parameters)
  b_loc = rnorm(N, 0, parameters)
  m_project = rhcauchy(N, var_intercepts)
  
  b_project = rep(0, N)
  for (i in 1:N) {
    b_project[i] = rnorm(1, m_project, var_intercepts)
  }
  
  for (w in 1:2){
    plot(NULL, xlim = c(-1, 5), ylim = c(0, 1))
    for (i in 1:N) {
      curve(exp(b_weighting[i] * w + b_loc[i] * x + b_project[i]) / (
        1 + exp(b_weighting[i] * w + b_loc[i] * x + b_project[i])
      ) ,
      add = TRUE)
    }  
  }
  
}

sensitivity4(1, 0.05)




