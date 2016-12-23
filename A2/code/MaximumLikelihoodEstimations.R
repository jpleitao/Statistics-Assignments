# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

ComputeMaximumLikelihoodEstimations <- function(dataset) {
  # Compute the maximum likelihood estimations for the parameters of the
  # selected probability distributions (Normal, Gamma and Logistic).
  #
  # Args:
  #   dataset: An array of doubles with the observed data.
  #
  coefsNormal <- ComputeMLE(dataset, 'Normal')
  coefsGamma <- ComputeMLE(dataset, 'Gamma')
  coefsLogistic <- ComputeMLE(dataset, 'Logistic')
  
  return(list(normal=coefsNormal, gamma=coefsGamma, logistic=coefsLogistic))
}

ComputeMLE <- function(dataset, densityFunction) {
  # Compute the maximum likelihood estimations for the parameters of a given
  # selected probability distributions (Normal, Gamma and Logistic), specified
  # in the "densityFunction" parameter.
  #
  # Args:
  #   dataset: An array of doubles with the observed data.
  #   densityFunction: A string with the name of the probability distribution
  #                    for which the parameters are being estimated. 
  #
  library(bbmle)
  
  meanDataset <- mean(dataset)
  sdDataset <- sd(dataset)
  
  printSummary = FALSE
  
  if (densityFunction == 'Normal') {
    guess = list(mu=meanDataset, sigma=sdDataset)
    
    fit = mle2(function(mu, sigma) {
      -sum(dnorm(dataset, mean=mu, sd=sigma, log=TRUE))
      }, start=guess, method="L-BFGS-B")
    
    if (printSummary == TRUE) {
      cat(summary(fit), '\n\n-----------------------------------------------\n')
    }
    
    coefs <- coef(fit)
    
    return(list(mu=coefs['mu'], sigma=coefs['sigma']))
    
  } else if (densityFunction == 'Gamma') {
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
    
    return(list(k=coefs['k'], teta=coefs['teta']))
    
  } else if (densityFunction == 'Logistic') {
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
    
    return(list(loc=coefs['loc'], scal=coefs['scal']))
    
  } else {
    stop('Invalid argument for function ComputeMLE!')
  }
}

PrintEstimations <- function(estimations) {
  # Prints the computed Maximum Likelihood Estimations for the parameters of 
  # the considered probability distribution functions (Normal, Gamma, Logistic).
  #
  # Args:
  #   estimations: The computed Maximum Likelihood Estimations for the
  #                parameters of the considered probability distribution
  #                functions.
  #
  cat('\n------------------ Maximum Likelihood Estimations -----------------\n')
  for (i in 1:length(estimations)) {
    cat('->', names(estimations)[i], ':\n')
    
    for (j in 1:length(estimations[[i]])) {
      cat('   ', names(estimations[[i]][j]), ' = ',
          as.numeric(estimations[[i]][j]), '\n')
    }
  }
}
