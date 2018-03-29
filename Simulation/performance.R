source("honestRpart.R")
library("randomForest")
library("gbm")
library("MASS")

#### Start with some simulated data ####

  
trainRF <- function (X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10) {
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  model <- randomForest(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, nodesize=leaf.size, keep.forest=TRUE)
  mse <- model$test$mse
  return(list(mse=model$mse, testmse=mse))
}

trainGBT <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10) {
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  data <- data.frame(Y = Y, X = X)
  model <- gbm(Y ~ ., data = data, distribution="gaussian", 
               n.trees = ntree, shrinkage = 10/ntree,
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
               n.trees = ntree, shrinkage = 10/ntree,
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
  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + X[, 4]*X[, 5] + 1*(X[, 6]<0.5) + (X[, 7] < 0.5)*(X[, 8]))
}


d <- 8
n <- 2000
subsample <- 0.5
leaf.size <- 20
ntree <- 500
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + rnorm(n, 0, 0.5)
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")
setEPS()
postscript("syn1_error.eps", width=6, height=4)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(mat, type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
                            "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
       col=c(1:5, 1:5), 
       lty=c(rep(1,5), rep(2,5)),
       lwd=1, cex=0.6, ncol=2)
dev.off()

#### Plot: Simulation 2 ####
pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + X[, 4]*X[, 5] + 1*(X[, 6]<0.5) + (X[, 7] < 0.5)*(X[, 8]))
}

d <- 8
n <- 2000
subsample=0.5
leaf.size <- 20
ntree <- 500
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + rnorm(n, 0, 1)
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("syn2_error.eps", width=6, height=4)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(mat, type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
                            "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
       col=c(1:5, 1:5), 
       lty=c(rep(1,5), rep(2,5)),
       lwd=1, cex=0.6, ncol=2)
dev.off()



#### Plot: Simulation 3 ####
pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + X[, 4]*X[, 5] + 1*(X[, 6]<0.5) + (X[, 7] < 0.5)*(X[, 8]))
}

d <- 8
n <- 2000
subsample=0.5
leaf.size <- 20
ntree <- 500
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + rnorm(n, 0, 3)
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelGBT <- trainGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelSGBT <- trainSGBT(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("syn3_error.eps", width=6, height=4)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(mat, type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
                            "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
       col=c(1:5, 1:5), 
       lty=c(rep(1,5), rep(2,5)),
       lwd=1, cex=0.6, ncol=2)
dev.off()

#### Plot: CASP ####
raw.data <- read.csv("CASP/CASP.csv", header=TRUE)
y <- as.vector(raw.data[["RMSD"]])
x <- raw.data[, 2:10]
n <- nrow(x)
ss <- sample(n, 20000, replace=FALSE)
xtrain <- x[ss, ]
xtest <- x[-ss, ]
ytrain <- y[ss]
ytest <- y[-ss]
d <- ncol(x)
ntree <- 500
subsample <- 0.5
leaf.size <- 100
  
modelGBT <- trainGBT(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelSGBT <- trainSGBT(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelBLV <- trainBLV(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("casp_error.eps", width=6, height=4)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(mat, type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
                            "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
       col=c(1:5, 1:5), 
       lty=c(rep(1,5), rep(2,5)),
       lwd=1, cex=0.6, ncol=2)
dev.off()

#### Plot: Maintenance ####
raw.data <- read.csv("Maintenance/data.tsv",header=FALSE)
ss <- sample(nrow(raw.data), 6000, replace=FALSE)
y <- as.vector(raw.data[[17]])
x <- raw.data[, 1:10]
xtrain <- x[ss, ]
xtest <- x[-ss, ]
ytrain <- y[ss]
ytest <- y[-ss]
d <- ncol(x)
ntree <- 500
subsample <- 0.5
leaf.size <- 100

modelGBT <- trainGBT(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelSGBT <- trainSGBT(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelBLV <- trainBLV(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("maintenance_error.eps", width=6, height=4)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(mat, type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
                            "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
       col=c(1:5, 1:5), 
       lty=c(rep(1,5), rep(2,5)),
       lwd=1, cex=0.6, ncol=2)
dev.off()

#### Plot: Boston ####
raw.data <- Boston
ss <- sample(nrow(raw.data), 400, replace=FALSE)
y <- as.vector(raw.data[[14]])
x <- raw.data[, 1:13]
xtrain <- x[ss, ]
xtest <- x[-ss, ]
ytrain <- y[ss]
ytest <- y[-ss]
d <- ncol(x)
ntree <- 500
subsample <- 0.5
leaf.size <- 20

modelGBT <- trainGBT(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelSGBT <- trainSGBT(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
modelRF <- trainRF(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size)
modelBLV <- trainBLV(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
modelRBLV <- trainBLV(X=xtrain, Y=ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")

setEPS()
postscript("boston_error.eps", width=6, height=4)
mat <- c()
mat <- cbind(modelGBT$mse, modelGBT$testmse, 
             modelSGBT$mse, modelSGBT$testmse,
             modelRF$mse, modelRF$testmse,
             modelBLV$mse, modelBLV$testmse,
             modelRBLV$mse, modelRBLV$testmse)

matplot(mat, type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,2,1,2,1,2,1,2,1,2), lwd=1,
        ylab="MSE", "xlab"="Ensemble Size")
legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
                            "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
       col=c(1:5, 1:5), 
       lty=c(rep(1,5), rep(2,5)),
       lwd=1, cex=0.6, ncol=2)
dev.off()


