# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

ComputeMaximumLikelihoodEstimations <- function(dataset) {
  # TODO(jpleitao): Document this!
  #
  # Args:
  #
  #
  coefsNormal <- ComputeMLE(dataset, 'Normal')
  coefsGamma <- ComputeMLE(dataset, 'Gamma')
  coefsLogistic <- ComputeMLE(dataset, 'Logistic')
  
  # TODO(jpleitao): Put bi-dimensional arrays, so having like:
  # array['normal'] = {'mu', 'sigma'}
  
  return(c(coefsNormal, coefsGamma, coefsLogistic))
}

LikelihoodNormal <- function(dataset, mu, sigma) {
  - sum(dnormal(dataset, mu, sigma, log=TRUE))
}

ComputeMLE <- function(dataset, density_function) {
  # TODO(jpleitao): Document this!
  #
  # Args:
  #
  #
  library(bbmle)
  
  meanDataset <- mean(dataset)
  sdDataset <- sd(dataset)
  
  printSummary = FALSE
  
  if (density_function == 'Normal') {
    guess = list(mu=meanDataset, sigma=sdDataset)
    
    fit = mle2(function(mu, sigma) {
      -sum(dnorm(dataset, mean=mu, sd=sigma, log=TRUE))
      }, start=guess, method="L-BFGS-B")
    
    if (printSummary == TRUE) {
      cat(summary(fit), '\n\n-----------------------------------------------\n')
    }
    
    coefs <- coef(fit)
    
    return(c(coefs['mu'], coefs['sigma']))
    
  } else if (density_function == 'Gamma') {
    distShape <- meanDataset^2/sdDataset^2
    distScale <- sdDataset^2/meanDataset
    
    guess = list(k=distShape, teta=distScale)
    
    fit = mle2(function(k, teta) {
      -sum(dgamma(dataset, shape=k, scale=teta, log=TRUE))
    }, start=guess, method="L-BFGS-B")
    
    if (printSummary == TRUE) {
      cat(summary(fit), '\n\n-----------------------------------------------\n')
    }
    
    coefs <- coef(fit)
    
    return(c(coefs['k'], coefs['teta']))
    
  } else if (density_function == 'Logistic') {
    distLoc <- meanDataset
    distScale <- sqrt(3)*(sdDataset/pi)
    
    guess = list(loc=distLoc, scal=distScale)
    
    fit = mle2(function(loc, scal) {
      -sum(dlogis(dataset, location=loc, scale=scal, log=TRUE))
    }, start=guess, method="L-BFGS-B")
    
    if (printSummary == TRUE) {
      cat(summary(fit), '\n\n-----------------------------------------------\n')
    }
    
    coefs <- coef(fit)
    
    return(c(coefs['loc'], coefs['scal']))
    
  } else {
    stop('Invalid argument for function ComputeMLE!')
  }
}
