mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#################################
### glm for a standard probit ###
#################################

myprobit = glm(admit ~ gre + gpa + rank, family=binomial(link = "probit"), 
               control=list(maxit=500, epsilon=1e-8), data = mydata)

## model summary
summary(myprobit)
coefsMAIN = coef(myprobit)

### input data ###
X = as.matrix(cbind(1, mydata[,2:4]))
y = mydata[,1]
init = c(0,0,0,0)

### input data ###
X = as.matrix(cbind(1, mydata[,2:4]))
y = mydata[,1]
init = c(0,0,0,0)

##########################################
### MLE function for a standard probit ###
##########################################

myprobMLE = function(params, X, y){
  
  # Arguments are starting parameters (coefficients), model matrix, response 
  b = params
  mu = X%*%b
  # compute the log likelihood
  ll = sum(y*log(pnorm(mu)) + (1-y)*log(1-pnorm(mu)))  
  ll
}

### Fit with optim
# make tolerance really low to duplicate glm result
outMLE = optim(init, myprobMLE, X=X, y=y, 
               control=list(fnscale=-1, maxit=1000, reltol=1e-8))  

# outMLE
coefsMLE = outMLE$par

### compare
rbind(coefsMLE, coefsMAIN)

#################################################
### EM for latent variable approach to probit ###
#################################################

probEM = function(params, X, y, tol=.00001, maxits=100, showits=T){
  # Arguments are starting parameters (coefficients), model matrix, response, 
  # tolerance, maximum iterations, and whether to show iterations
  
  #starting points
  b = params
  mu = X%*%b
  it = 0
  converged = FALSE
  z = rnorm(length(y))                                                    # z is the latent variable ~N(0,1)
  
  if (showits)                                                            # Show iterations
    cat(paste("Iterations of EM:", "\n"))
  while ((!converged) & (it < maxits)) {                                  # while no convergence and we haven't reached our max iterations do this stuff
    zOld = z                                                              # create 'old' values for comparison
    
    z = ifelse(y==1, mu+dnorm(mu)/pnorm(mu), mu-dnorm(mu)/pnorm(-mu))     # E step create a new z based on current values
    
    b = solve(t(X)%*%X) %*% t(X)%*%z                                      # M step estimate b
    mu = X%*%b
    
    ll= sum(y*pnorm(mu, log.p=T) + (1-y)*pnorm(-mu, log.p=T))
    
    it = it + 1
    if (showits & (it == 1 | it%%5 == 0))
      cat(paste(format(it), "...", "\n", sep = ""))
    converged = max(abs(zOld - z)) <= tol
  }
  
  if (showits)                                                            # Show last iteration
    cat(paste0(format(it), "...", "\n"))
  
  return(list(b=t(b), ll=ll ))
}

outEM = probEM(params=init, X=X, y=y, tol=1e-8, maxit=100); outEM  # can lower tolerance to duplicate glm result
coefsEM = outEM$b

### compare
rbind(coefsMAIN, coefsMLE, coefsEM)
rbind(logLik(myprobit),  outMLE$value, outEM$ll)
