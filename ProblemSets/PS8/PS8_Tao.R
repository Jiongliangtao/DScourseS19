#4
set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1] <- 1 
eps <- rnorm(N,mean=0,sd=0.5)
beta<- c(1.5,-1,-0.25,0.75,3.5,-2,0.5,1,1.25,2)
Y <- X%*%beta + eps

#5
beta.hat.matrix<-solve(t(X)%*%X)%*%(t(X)%*%Y)

#6

alpha <- 0.0000003

#gradient function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}
#gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta.hat.matrix
beta <- runif(dim(X)[2])
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

#estimate model 
est<-lm(Y~X+0)
summary(est)
beta

#compare answer
cbind(est$coefficients,beta)

beta.hat.gd<-beta

#7
library(nloptr)
## Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
beta.hat.matrix.LBFGS<-result$solution

options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
result1 <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
beta.hat.NM<-result1$solution


#8
objfun  <- function(theta,Y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradient <- function (theta ,Y,X) {
  grad <- as.vector ( rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig^3)
  return ( grad )
}

beta0 <- runif(dim(X)[2]+1)
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
result2 <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
beta.hat.MLS <- result2$solution
cbind(beta.hat.matrix,beta.hat.gd,beta.hat.matrix.LBFGS,beta.hat.NM,beta.hat.MLS)

#9
beta.hat.easy <- lm(Y ~ X -1)
beta.hat.easy

library(stargazer)
stargazer(beta.hat.matrix,beta.hat.gd,beta.hat.matrix.LBFGS,beta.hat.NM,beta.hat.MLS,beta.hat.easy)
