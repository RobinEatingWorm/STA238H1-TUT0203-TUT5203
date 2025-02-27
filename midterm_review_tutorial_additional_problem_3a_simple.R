# Inverse CDF F^{-1} of X
F_inverse <- function(prob, theta) {
  cdf <- (2 + theta) / 4
  if (prob <= cdf) {
    return(1)
  }
  cdf <- cdf + theta / 4
  if (prob <= cdf) {
    return(2)
  }
  cdf <- cdf + (1 - theta) / 4
  if (prob <= cdf) {
    return(3)
  }
  return(4)
}

# MLE (could also be MOM estimate)
theta_hat <- (1655 - sqrt(3721809)) / -7678

# Sample size
n <- 3839

# Random samples from U ~ Unif(0, 1) representing probabilities
u <- runif(n, min = 0, max = 1)

# Transform U to X
x <- numeric(length = n)
for (i in 1:n) {
  x[i] <- F_inverse(u[i], theta_hat)
}

# Display counts of each plant type
a <- c(1, 2, 3, 4)
frequency <- numeric(length = length(a))
for (i in 1:length(a)) {
  frequency[i] = sum(x == a[i])
}
data.frame(x = a, frequency = frequency)
