# Inverse CDF F^{-1} of X
F_inverse <- function(prob, theta) {
  x <- c(1, 2, 3, 4)
  pmf <- c((2 + theta) / 4,  theta / 4, (1 - theta) / 4, (1 - theta) / 4)
  cdf <- cumsum(pmf)
  return(sapply(prob, \(prob) x[min(which(prob <= cdf))]))
}

# MLE (could also be MOM estimate)
theta_hat <- (1655 - sqrt(3721809)) / -7678

# Sample size
n <- 3839

# Random samples from U ~ Unif(0, 1) representing probabilities
u <- runif(n, min = 0, max = 1)

# Transform U to X
x <- F_inverse(u, theta_hat)

# Display counts of each plant type
a <- c(1, 2, 3, 4)
data.frame(x = a, frequency = sapply(a, \(a) sum(x == a)))
