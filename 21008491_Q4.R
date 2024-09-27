#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
binomialMLE <- function(x, n, p0 = 0.5, tol = 1e-6, MaxIter = 100) {
#######################################################################
##
##    This function illustrates the MLE method for maximising the binomial
##    log-likelihood function using the Newton-Raphson method.
##    The log-likelihood function is given by:
##    L(p; x) = sum[n * log(p/(1-p)) + n * log(1 - p)] + constant.
##    The arguments are:
##
##    x       A vector containing the data values x1,...,xN.
##    n       A vector containing the known values n1,...,nN.
##    p0      The initial value for the MLE which is 0.5.
##    tol     The "tolerance": the algorithm is judged to 
##            have converged when either the gradient or the
##            relative change in the estimate is less than 
##            this. The default value is 10^-6.
##    MaxIter The maximum number of iterations that can occur.
##
##    The function returns a list containing the MLE of p,
##    the standard error of the MLE, the log-likelihood at the MLE,
##    the gradient at the MLE, and the number of iterations taken.
##
#######################################################################
 #
 # Started by initialising all the quantities and qualitatives that we need, 
 # as follows:
 #
 # p         Current value of the estimate (initialise to p0)
 # Iter      Number of iterations (initialise to 0)
 # Conv      Convergence flag (initialise to FALSE)
 #
 p <- p0
 Iter <- 0
 convergence=FALSE
 #
 # Then we define the log-likelihood function evaluated at p. After, we 
 # derive the log-likelihood function and achieve the first derivative of the 
 # log-likelihood function and the second derivative of the log-likelihood function.
 # The function are as follows: 
 #
 # Log-likelihood function
 logL <- function(p) {
   sum(x * log(p / (1 - p)) + n * log(1 - p))
  }
 #  
 # First derivative of the log-likelihood
 dlogL <- function(p) { 
   sum(x / p - (n - x) / (1 - p)) 
  }
 #  
 # Second derivative of the log-likelihood
 d2logL <- function(p) { 
   sum(-x / p^2 - (n - x) / (1 - p)^2) 
  }
 #
 # Now looping. The "while" condition will become FALSE as soon
 # as either the absolute value of the gradient (first derivative of the 
 # log-likelihood function) becomes less than tol, OR the absolute value of the
 # relative change becomes less than tol, OR the iteration count reaches MaxIter.
 #
  while (Iter < MaxIter && convergence==FALSE) {
    delta <- dlogL(p)/d2logL(p)
    p_new <- p - delta # Update p using Newton-Raphson method
    #
    # Now looping again. The "while" condition will become FALSE as soon
    # as the p value is in the range (0,1). If the p value is not in the range
    # we adjust the step size (delta value) and recalculate p_new accordingly.
    #
    while (p_new <= 0 || p_new >= 1) {
      delta <- delta / 2
      p_new <- p - delta # Update p using Newton-Raphson method
    }
    #
    # Check for convergence using an "if" loop.
    #
    if (abs(dlogL(p)) < tol && (abs(p_new - p) < tol)) {
      convergence=TRUE
    }
    p <- p_new # Updating the p value
    Iter <- Iter + 1 # Updating the iteration Count
  } 
  p_hat=p # Value of the MLE of p
  se <- sqrt(1 /(-d2logL(p))) # Value of the standard error
  #
  # Return list
  #
  return(list(p=p_hat, StdErr=se, logL=logL(p), gradient=dlogL(p) , N.Iter=Iter))
}
#######################################################################