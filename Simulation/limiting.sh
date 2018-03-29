#!/bin/bash
# Telling how many nodes and processors should be used.
#PBS -l nodes=6:ppn=8,walltime=48:00:00
# Naming the file
#PBS -N Boulevard_Limiting_03_26
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
# library(snow)
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

######## HERE BEGINS THE SIMULATION ########

pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + X[, 4]*X[, 5] + 1*(X[, 6]<0.5) + (X[, 7] < 0.5)*(X[, 8]))
}


sim <- function(dummy, xtest, d, n, subsample, leaf.size, ntree, error) {
  ans <- c()
  X <- matrix(runif(d*n), nrow=n)
  Y <- pred(X) + error(n)
  blv <- boulevard(X, Y, ntree=ntree, subsample=subsample, 
                   leaf.size=leaf.size, method="random") 
  ans <- predict.boulevard(blv, xtest)
}

error1 <- function(n) {
  return(rnorm(n))
}

error2 <- function(n) {
  return(rnorm(n, 0, 4))
}

error3 <- function(n) {
  return(runif(n, -1, 1))
}

error4 <- function(n) {
  return(runif(n, -4, 4))
}


d <- 8
n <- 10000
subsample <- 0.5 
leaf.size <- 50  
ntree <- 2000

xtest <- matrix(runif(d*5), ncol=d)
sfExportAll()

limiting <- list()
limiting[[1]] <- sfLapply(1:1000, sim, 
                          xtest=xtest, d=d, n=n, subsample=subsample, leaf.size=leaf.size, ntree=ntree, 
                          error=error1)
limiting[[2]] <- sfLapply(1:1000, sim, 
                          xtest=xtest, d=d, n=n, subsample=subsample, leaf.size=leaf.size, ntree=ntree, 
                          error=error2)
limiting[[3]] <- sfLapply(1:1000, sim, 
                          xtest=xtest, d=d, n=n, subsample=subsample, leaf.size=leaf.size, ntree=ntree, 
                          error=error3)
limiting[[4]] <- sfLapply(1:1000, sim, 
                          xtest=xtest, d=d, n=n, subsample=subsample, leaf.size=leaf.size, ntree=ntree, 
                          error=error4)


save(limiting, file="limiting_03_29.RData")

sfStop()

EOF
