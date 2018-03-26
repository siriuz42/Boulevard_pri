library(rpart)
library(treeClust)

honest.rpart.structure <- function(X, Y, method="standard", structY=NULL, leaf.size=3, diameter.test=NULL) {
  n <- nrow(X)
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  pass.diameter.check <- FALSE
  while (!pass.diameter.check) {
    if (method=="random") {
      trainData <- data.frame(Y=rnorm(n), X=X)
    } else {
      trainData <- data.frame(Y=structY, X=X)
    }
    tree.structure <- rpart(Y ~ . , data=trainData, control=rpart.control(cp=0, minsplit=leaf.size+1))
    if (!is.null(diameter.test)) {
      colnames(diameter.test) <- paste("x", 1:ncol(X), sep="")
      where <- rpart.predict.leaves(tree.structure, newdata=data.frame(newdata), type="where")
      if (length(where)==length(unique(where))) return(tree.structure)
    } else {
      return(tree.structure)
    }
    method <- "random"
  }
}

honest.rpart <- function(X, Y, method="standard", structY=NULL, subset=NULL, leaf.size=3, diameter.test=NULL) {
  colnames(X) <- paste("x", 1:ncol(X), sep="")
  trainData <- data.frame(Y, X)
  
  tree <- list()
  if (is.null(subset)) {
    newX <- X
    tree$y <- Y
    tree$weight <- rep(0, length(Y))
  } else {
    subset <- sort(subset)
    newX <- X[subset, ]
    tree$subset <- subset
    tree$y <- Y[subset]
    tree$weight <- rep(0, length(subset))
  }
  if (!is.null(diameter.test)) {
    tree$rpart.tree <- honest.rpart.structure(X, Y, method=method, structY=structY, leaf.size=leaf.size, diameter.test=diameter.test)
  } else {
    tree$rpart.tree <- honest.rpart.structure(X, Y, method=method, structY=structY, leaf.size=leaf.size)
  }
  
  tmpWhere <- unique(rpart.predict.leaves(tree$rpart.tree, newdata=data.frame(X=X), type="where"))
  where <- rpart.predict.leaves(tree$rpart.tree, newdata=data.frame(X=newX), type="where")
  tmpPredict <- c()
  for (i in tmpWhere) {
    tmpCount <- sum(where==i)
    if (tmpCount > 0) {
      tmpPredict <- c(tmpPredict, mean(tree$y[where==i]))
      tree$weights[where==i] <- 1/tmpCount
    } else {
      tmpPredict <- c(tmpPredict, 0)
    }
  }
  tree$predict <- tmpPredict
  names(tree$predict) <- tmpWhere
  return(tree)
}

honest.rpart.predict <- function(tree, newdata) {
  if (is.vector(newdata)) {
    newdata = matrix(newdata, nrow=1)
  }
  colnames(newdata) = paste("x", 1:ncol(newdata), sep="")
  where <- rpart.predict.leaves(tree$rpart.tree, newdata=data.frame(X=newdata), type="where")
  return(as.vector(tree$predict[paste(where)]))
}

predict.boulevard <- function(blv, X) {
  ntree <- length(blv$trees)
  lambda <- blv$lambda
  ans <- rep(0, nrow(X))
  for (b in 1:ntree) {
    ans <- (b-1)/b*ans + lambda/b*honest.rpart.predict(blv$trees[[b]], newdata = X)
  }
  return(ans)
}

boulevard <- function(X, Y, ntree=1000, lambda = 0.5, subsample=0.8, xtest=NULL, ytest=NULL, leaf.size=10, method="random") {
  n <- nrow(X)
  tree <- list()
  nss <- floor(n * subsample)
  ans <- rep(0, nrow(X))
  if (!is.null(xtest)) {
    predtest <- rep(0, nrow(xtest))
  }
  trainmse <- c()
  testmse <- c()
  for (b in 1:ntree) {
    if (b%%50 == 0) {
      cat("Training:", b, "\n")
    }
    res <- Y - ans
    if (method=="random") {
      tree[[b]] <- honest.rpart(X, res, method=method, subset=sample(n, nss, replace=FALSE), leaf.size=leaf.size)
    } else if (method=="standard") {
      tree[[b]] <- honest.rpart(X, res, method=method, structY=res, subset=sample(n, nss, replace=FALSE), leaf.size=leaf.size)
    }
    ans <- (b-1)/b*ans + lambda/b*honest.rpart.predict(tree[[b]], newdata = X)
    trainmse <- c(trainmse,mean((ans/lambda*(1+lambda)-Y)^2))
    if (!is.null(xtest)) {
      predtest <- (b-1)/b*predtest + lambda/b*honest.rpart.predict(tree[[b]], newdata=xtest)
      testmse <- c(testmse, mean((predtest/lambda*(1+lambda)-ytest)^2))
    }
  }
  return(list(trees=tree, mse=trainmse, testmse=testmse, lambda=lambda))
}


