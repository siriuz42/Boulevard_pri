source("honestRpart.R")
library("plotrix")

# pred <- function(X) {
#  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + 2*X[, 4]*X[, 5] + X[, 7]^2*X[, 8])
#}

pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]^2 + 2*X[, 4]*X[, 5])
}

# pred <- function(X) {
#  return(rep(0, nrow(X)))
#}

error <- function(n, d = 1) {
  return(runif(n, -d, d))
}

d <- 5
n <- 1000
subsample <- 0.5
leaf.size <- 10
ntree <- 1000


ntest <- 20
xtest <- matrix(runif(d*ntest), nrow=ntest)
ytest <- c()

for(TIME in 1:20) {
  xtrain <- matrix(runif(d*n), nrow=n)
  ytrain <- pred(xtrain) + error(n)
  
  blv <- boulevard(xtrain, ytrain, ntree=ntree, subsample=subsample, lambda=0.3,
                   leaf.size=leaf.size, method="random") 
  ytest <- rbind(ytest, predict.boulevard(blv, xtest)) 
}

est.var <- predict.boulevard.variance(blv, newdata = xtest)
ci.width <- 1.96*sqrt(est.var)*sqrt(2)
blv.pred <- predict.boulevard(blv, xtest)
truth <- pred(xtest)

# plotmax <- max(truth, blv.pred+ci.width, blv.pred-ci.width, ytrain[1:ntest])
# plotmin <- min(truth, blv.pred+ci.width, blv.pred-ci.width, ytrain[1:ntest])

plotmax <- max(truth, blv.pred+ci.width, blv.pred-ci.width)
plotmin <- min(truth, blv.pred+ci.width, blv.pred-ci.width)

boxplot(ytest, ylim=c(plotmin, plotmax), xlab="Test Pts")
plotCI(blv.pred, uiw = ci.width, 
       add=TRUE, col="red", cex=1, slty=1)
points(truth, col="blue", pch=2)
points(ytrain[1:ntest], col="green")

for(TIME in 1:20) {
  points(ytest[TIME, ], col="green")
}



