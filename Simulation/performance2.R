source("honestRpart.R")
library("randomForest")
library("gbm")
library("MASS")

#### Start with some simulated data ####

  
trainRF <- function (X, Y, ntree=1000, xtest=NULL, ytest=NULL, subsample = 0.8, leaf.size=10) {
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  model <- randomForest(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, nodesize=leaf.size, sampsize = nrow(X)*subsample, keep.forest=TRUE)
  mse <- model$test$mse
  return(list(mse=model$mse, testmse=mse))
}

trainGBT <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10) {
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  data <- data.frame(Y = Y, X = X)
  model <- gbm(Y ~ ., data = data, distribution="gaussian", 
               n.trees = ntree, shrinkage = 0.05,
               bag.fraction = 1, train.fraction = 1, n.minobsinnode = leaf.size)
  colnames(xtest) <- paste("x", 1:ncol(X), sep="")
  testData <- data.frame(X = xtest)
  tmpPred <- predict.gbm(model, newdata=testData, n.trees=c(1:ntree))
  modelPred <- predict.gbm(model, newdata=data, n.trees=c(1:ntree))
  mse <- c()
  modelmse <- c()
  for (b in 1:ntree) {
    mse <- c(mse, mean((tmpPred[, b]-ytest)^2))
    modelmse <- c(modelmse, mean((modelPred[, b]-Y)^2))
  }
  return(list(mse=modelmse, testmse=mse))
}

trainSGBT <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, subsample=0.8, leaf.size=10) {
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  data <- data.frame(Y = Y, X = X)
  model <- gbm(Y ~ ., data = data, distribution="gaussian", 
               n.trees = ntree, shrinkage = 0.05,
               bag.fraction = subsample, train.fraction = 1, n.minobsinnode = leaf.size)
  colnames(xtest) <- paste("x", 1:ncol(X), sep="")
  testData <- data.frame(X = xtest)
  tmpPred <- predict.gbm(model, newdata=testData, n.trees=c(1:ntree))
  modelPred <- predict.gbm(model, newdata=data, n.trees=c(1:ntree))
  mse <- c()
  modelmse <- c()
  for (b in 1:ntree) {
    mse <- c(mse, mean((tmpPred[, b]-ytest)^2))
    modelmse <- c(modelmse, mean((modelPred[, b]-Y)^2))
  }
  return(list(mse=modelmse, testmse=mse))
}

trainBLV <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10, subsample=0.8, method="random") {
  model <- boulevard(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method=method)
  return(list(mse=model$mse, testmse=model$testmse))
}


#### Plot: Simulation 1 ####
pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]*X[, 4])
}


d <- 4
n <- 5000
subsample <- 0.3
leaf.size <- 20
ntree <- 1000
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + runif(n, -1, 1)
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")
setEPS()
postscript("syn1.eps", width=6, height=4.5)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(x=50:ntree, y=mat[-c(1:49), ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()

#### Plot: Simulation 2 ####
pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + X[, 3]*X[, 4])
}

d <- 4
n <- 5000
subsample <- 0.3
leaf.size <- 20
ntree <- 1000
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + sign(rnorm(n, 0, 1))*0.5
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("syn2.eps", width=6, height=4.5)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(x=50:ntree, y=mat[-c(1:49), ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()


#### Plot: Simulation 3 ####
pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + (1-X[, 3])^2 + X[, 4]*X[, 5] + (1-X[, 6])^6 + X[, 7])
}

d <- 7
n <- 5000
subsample <- 0.3
leaf.size <- 20
ntree <- 1000
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + runif(n, -1, 1)
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("syn3.eps", width=6, height=4.5)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(x=50:ntree, y=mat[-c(1:49), ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()

#### Plot: Simulation 4 ####
pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + (1-X[, 3])^2 + X[, 4]*X[, 5] + (1-X[, 6])^6 + X[, 7])
}

d <- 7
n <- 5000
subsample <- 0.3
leaf.size <- 20
ntree <- 1000
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + sign(runif(n, -1, 1))*0.5
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("syn4.eps", width=6, height=4.5)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(x=50:ntree, y=mat[-c(1:49), ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5),
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=5, byrow=TRUE)
dev.off()
