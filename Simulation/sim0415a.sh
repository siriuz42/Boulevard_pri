#!/bin/bash
# Telling how many nodes and processors should be used.
#PBS -l nodes=6:ppn=8,walltime=48:00:00
# Naming the file
#PBS -N Boulevard
# Outputting error
#PBS -j oe
# Not sure what the two next lines do
#PBS -q default
#PBS -S /bin/bash
#PBS -m abe
#PBS -M yz793@cornell.edu

######## I WONDER IF I NEED THIS LINE ######
cd $PBS_O_WORKDIR

# Telling cluster that you are using R
R --vanilla > mysnow.out <<EOF

# Looking for what machines are available to use.
library(snowfall)
pbsnodefile = Sys.getenv("PBS_NODEFILE")
machines <- scan(pbsnodefile, what="")
print(machines)
nmach = length(machines)
nmach

# Initializing the nodes
sfInit(parallel=TRUE,type='SOCK',cpus=nmach,socketHosts=machines)


#################################################################################
#  All of the above 'R --vanilla...' is for the cluster
#  All of the below 'R --vanilla...' is an R file
#  This is the beginning of a 'regular' R file
#################################################################################
sfSource("honestRpart.R")

suffix <- format(Sys.time(), "%y%m%d%H%M%S")
######## HERE BEGINS THE SIMULATION ########

pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + X[, 3]^2 + 2*X[, 4]*X[, 5])
}

error <- function(n, d = 1) {
    return(runif(n, -d, d))
}

node.work <- function(dummy, n, d, subsample, leaf.size, ntree, errsig=1, lambda = 0.5) {
    xtrain <- matrix(runif(d*n), nrow=n)
    ytrain <- pred(xtrain) + error(n, d=errsig)
    blv <- boulevard(xtrain, ytrain, ntree=ntree, subsample=subsample, lambda = lambda,
                     leaf.size=leaf.size, method="random") 
    return(predict.boulevard(blv, xtest)) 
}

d <- 5
n <- 5000
subsample <- 0.8 
leaf.size <- 20  
ntree <- 1000
lambda <- 0.5
errsig <- 2

ntest <- 10
xtest <- t(matrix(c(0.5, 0.5, 0.5, 0.5, 0.5,
                    0.2, 0.2, 0.2, 0.2, 0.2,
                    0.1, 0.9, 0.1, 0.9, 0.1,
                    0.1, 0.1, 0.9, 0.9, 0.9,
                    0.9, 0.1, 0.1, 0.1, 0.9,
                    0.5, 0.1, 0.9, 0.1, 0.5,
                    0.3, 0.2, 0.7, 0.8, 0.6,
                    0.4, 0.2, 0.3, 0.6, 0.7,
                    0.2, 0.7, 0.8, 0.3, 0.5,
                    0.3, 0.6, 0.4, 0.9, 0.5), nrow=d))

sfExportAll()

result <- sfSapply(1:100, node.work, 
                   n=n, d=d, subsample=subsample, errsig=errsig, leaf.size=leaf.size, ntree=ntree)
result <- t(result)
save(result, file=paste("result_", suffix, ".RData", sep=""))

xtrain <- matrix(runif(d*n), nrow=n)
ytrain <- pred(xtrain) + error(n, d=errsig)
blv <- boulevard(xtrain, ytrain, ntree=ntree, subsample=subsample, lambda = lambda,
                 leaf.size=leaf.size, method="random") 
                 
est.var <- predict.boulevard.variance(blv, newdata = xtest)
ci.width <- 1.96*sqrt(est.var)*sqrt(2)
blv.pred <- predict.boulevard(blv, xtest)
truth <- pred(xtest)

# plotmax <- max(truth, blv.pred+ci.width, blv.pred-ci.width, ytrain[1:ntest])
# plotmin <- min(truth, blv.pred+ci.width, blv.pred-ci.width, ytrain[1:ntest])


plotmax <- max(truth, blv.pred+ci.width, blv.pred-ci.width, result)
plotmin <- min(truth, blv.pred+ci.width, blv.pred-ci.width, result)

setEPS()
postscript(file=paste("fig_", suffix, ".eps", sep=""),
           width=8, height=6)
boxplot(result, ylim=c(plotmin, plotmax), xlab="Test Pts")
plotCI(blv.pred, uiw = ci.width, 
       add=TRUE, col="red", cex=1, slty=1)
points(truth, col="blue", pch=2)
dev.off()
sfStop()

EOF
