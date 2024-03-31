library(nloptr)

set.seed(100)

# Define dimensions
N <- 100000
K <- 10

# Generate X matrix
X <- cbind(1, matrix(rnorm(N * (K - 1)), ncol = K - 1))

# Generate epsilon vector
sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)

# Generate beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y vector
Y <- X %*% beta + eps

# using the closed-form solution "(X'X)^-1X'Y)"
ols_est <- solve(t(X) %*% X) %*% t(X) %*% Y

print(ols_est)
print(beta)

# the estimated betas and the defined betas are very similar

#-------------------------------------------------------------
# Problem 6
#-------------------------------------------------------------

step <- 0.0000003
iter <- 500
gradient1 <- function(X, Y, beta) return(as.vector(-2*t(X)%*%(Y-X%*%beta)))

beta.grad <- matrix(0, ncol = 1, nrow = ncol(X))

# create a vector to contain all xs for all steps
x.All <- vector("numeric", iter)

for(i in 1:iter) {
  beta.grad <- beta.grad - step * gradient1(X, Y, beta.grad)
  x.All[i] <- beta.grad
  print(beta.grad)
}

print(paste("The estimate of Beta is ", beta.grad, sep = ""))
print(beta)

#-------------------------------------------------------------
# Problem 7: L-BFGS
#-------------------------------------------------------------

## Our objective function
objfun1 <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
} 

## Gradient of our objective function
gradient2 <- function(beta,y,X) {
  return (as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

result <- nloptr( x0=beta0,eval_f=objfun1,eval_grad_f=gradient2,opts=options,y=Y,X=X)
print(result)

#-------------------------------------------------------------
# Problem 7: Nelder-Mead
#-------------------------------------------------------------

objfun2 <- function(beta, X, Y) {
  return(sum((Y - X %*% beta)^2))
}

# initial values
beta1 <- runif(dim(X)[2])

# Algorithm parameters
options <- list("algorithm" = "NLOPT_LN_NELDERMEAD", "xtol_rel" = 1.0e-8, "maxeval"=1e3)

# Find the optimum!
res <- nloptr(x0 = beta1, eval_f = objfun2, Y=Y, X=X, opts = options)
print(res)
print(result)
print(beta)

# the results are very similar, but the L-BFGS is just slightly more precise to the 
# actual estimates

#-------------------------------------------------------------
# Problem 8: MLE Beta Estimate
#-------------------------------------------------------------

# Define the objective function
objfun3 <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  log_likelihood <- sum(-0.5 * (log(2 * pi * (sig^2)) + ((Y - X %*% beta) / sig)^2))
  return(-log_likelihood) # Negative log-likelihood for minimization
}

# Define the gradient function
gradient3 <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Set initial parameter values
theta0 <- runif(dim(X)[2]+1)  # Initialize beta with random values and sigma of Y

# Define optimization options
options <- list("algorithm" = "NLOPT_LN_SBPLX", "xtol_rel" = 1.0e-6, "maxeval"=1e4)

# Run optimization
result <- nloptr(x0 = theta0, eval_f = objfun3, eval_grad_f = gradient3, opts = options, Y = Y, X = X)

# Extract the estimated parameters
betahat <- result$solution[1:(length(result$solution) - 1)]
sigmahat <- result$solution[length(result$solution)]

# Print the result
print(result)
print(beta)


#-------------------------------------------------------------
# Problem 9: OLS Normal Way
#-------------------------------------------------------------
library(modelsummary)
ols_easy <- lm(Y ~ X-1)
modelsummary(ols_easy, stars = TRUE, output = 'latex')
