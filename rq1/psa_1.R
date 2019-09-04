library(extraDistr)
setwd("~/Documents/dev/linespots/linespots-analysis")
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
  plot(NULL, xlim = c(-1, 5), ylim = c(0, 1), ylab = "Inverse Logit", xlab = "LOC")
  for (w in 1:3){
    for (i in 1:(N/3)) {
      j = i + ((w -1) * (N/3))
      curve(exp(b_intercept[j] + b_weighting[j] * w + b_loc[j] * x + b_project[j]) / (
        1 + exp(b_intercept[j] + b_weighting[j] * w + b_loc[j] * x + b_project[j])
      ) ,
      add = TRUE)
    }  
  }
  
}
pdf("psa-1.pdf")
sensitivity1(10, 10, 10)
dev.off()

sensitivity1(1, 1, 1)
dev.off()
pdf("psa-2.pdf")
sensitivity1(0.1, 0.1, 0.1) # This is way too small.
dev.off()
sensitivity1(1, 0.5, 1) # Seems sensible for both w
dev.off()
pdf("psa-3.pdf")
sensitivity1(0.5, 0.5, 0.1) # This seems to work as well which means we can use the same prior for all models.
dev.off()

# For 1 + Weighting + LOC + (1|Language) + (1|Project)
sensitivity2 <- function(intercept, parameters, var_intercepts) {
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
  
  plot(NULL, xlim = c(-1, 5), ylim = c(0, 1))
  for (w in 1:3) {
    for (i in 1:(N / 3)) {
      j = i + ((w - 1) * (N / 3))
      curve(exp(b_intercept[j] + b_weighting[j] * w + b_loc[j] * x + b_project[j] + b_language[i]) / (
        1 + exp(b_intercept[j] + b_weighting[j] * w + b_loc[j] * x + b_project[j] + b_language[i])
      ) ,
      add = TRUE)
    }
  }
}

sensitivity2(10, 10, 10)
dev.off()
sensitivity2(1, 1, 1)
dev.off()
sensitivity2(0.1, 0.1, 0.1) # Again, this seems to focused around 0.5
dev.off
sensitivity2(0.5, 0.5, 0.1)  # This looks ok.

# Based on this we rerun the models in RQ1 with the following priors:
# prior(normal(0,0.5), class=Intercept),
# prior(normal(0,0.5), class=b),
# prior(cauchy(0,0.1), class=sd),
# prior(gamma(0.1, 0.1), class=phi)

