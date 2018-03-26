source("honestRpart.R")

pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + X[, 4]*X[, 5] + 1*(X[, 6]<0.5) + (X[, 7] < 0.5)*(X[, 8]))
}

d <- 8
n <- 10000
subsample=0.1
leaf.size <- 20
ntree <- 1000

ntest <- 10
xtest <- matrix(runif(d*ntest), nrow=ntest)

for (TIME in 1:100) {
  X <- matrix(runif(d*n), nrow=n)
  Y <- pred(X) + runif(n, -1, 1)
  
}

