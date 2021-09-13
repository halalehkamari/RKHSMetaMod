# RKHSMetaMod
# prediction function: This function calculates the predicted values for a new dataset based on the best RKHS meta-model estimator. 
# Example:
#
d <- 3; n <- 50; nT <- 50; nnew <- 200

library(lhs); library(RKHSMetaMod)

X <- maximinLHS(n, d); XT <- maximinLHS(nT, d); Xnew <- maximinLHS(nnew, d)

c <- c(0.2,0.6,0.8)

F <- 1;for (a in 1:d) F <- F*(abs(4*X[,a]-2)+c[a])/(1+c[a])

FT <- 1;for (a in 1:d) FT <- FT*(abs(4*XT[,a]-2)+c[a])/(1+c[a])

sigma <- 0.2

epsilon <- rnorm(n,0,1);Y <- F + sigma*epsilon

epsilonT <- rnorm(nT,0,1);YT <- FT + sigma*epsilonT

Dmax <- 3; kernel <- "matern"; frc <- c(10,100); gamma <- c(.5,.01,.001)

res <- RKHSMetMod(Y,X,kernel,Dmax,gamma,frc,FALSE)

mu <- vector(); l <- length(gamma); for(i in 1:length(frc)){mu[i]=res[[(i-1)*l+1]]$mu}

Err <- PredErr(X,XT, YT,mu,gamma, res, kernel,Dmax)

pred <- prediction(X, Xnew, kernel, Dmax, res, Err)
