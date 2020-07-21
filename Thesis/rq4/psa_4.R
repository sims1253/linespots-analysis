library(extraDistr)

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
  plot(NULL, xlim = c(-1, 5), ylim = c(0, 100))
  for (w in 1:3) {
    for (i in 1:(N / 3)) {
      j = i + ((w - 1) * (N / 3))
      curve(exp(b_intercept[j] + b_weighting[j] * w + b_loc[j] * x + b_project[j]) ,
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
sensitivity1(1, 0.5, 0.1) # This seems to work as well which means we can use the same prior for all models.
