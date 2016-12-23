# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

DrawQQPlots <- function(dataset, estimations) {
  # Draws the QQ-Plots for the the probability distribution functions considered
  # (Normal, Gamma and Logistic)
  #
  # Args:
  #   dataset: An array of doubles with the observed data.
  #   estimations: The computed Maximum Likelihood Estimations for the
  #                parameters of the considered probability distribution
  #                functions.
  #
  DrawQQPlot(dataset, 'Normal', estimations$normal)
  DrawQQPlot(dataset, 'Gamma', estimations$gamma)
  DrawQQPlot(dataset, 'Logistic', estimations$logistic)
}

DrawQQPlot <- function(dataset, densityFunction, estimations) {
  # Plots the QQ-Plot for a given probability distribution function.
  #
  # Args:
  #   dataset: An array of doubles with the observed data.
  #   densityFunction: The probability distribution function for which the
  #                    QQ-Plot will be plotted.
  #   estimations: The computed Maximum Likelihood Estimations for the
  #                parameters of the given probability distribution function.
  #
  #
  lengthPpoints <- length(dataset)
  
  # Save plot to file
  png(paste('images/QQPlot_', densityFunction, '.png'), width=1000, height=800)
  
  if(densityFunction == 'Normal') {
    meanDataset <- as.numeric(estimations[[1]])
    sdDataset <- as.numeric(estimations[[2]])
    
    # Draw QQ-Plot for the Normal distribution
    qqplot(dataset,
           qnorm(ppoints(lengthPpoints), mean=meanDataset,
                 sd=sdDataset),
           xlab='Observed Values',
           ylab=paste('Quantiles Normal(u=', meanDataset, ', sigma=',
                      sdDataset, ')'),
           main=expression('Normal QQ-plot'))
    
    # Draw least squares line overlapped with the QQ-Plot
    qqline(dataset,
           distribution=function(p) qnorm(p, mean=meanDataset, sd=sdDataset),
           col=1)
  } else if(densityFunction == 'Gamma') {
    distShape <- estimations[[1]]
    distScale <- estimations[[2]]
    
    # Draw QQ-Plot for the Gamma distribution
    qqplot(dataset,
           qgamma(ppoints(lengthPpoints), shape=distShape, scale=distScale),
           xlab='Observed Values',
           ylab=paste('Quantiles Gamma(k=', distShape, ',teta=', distScale,')'),
           main=expression('Gamma QQ-plot'))
    
    # Draw least squares line overlapped with the QQ-Plot
    qqline(dataset,
           distribution=function(p) qgamma(p, shape=distShape, scale=distScale),
           col=1)
  } else if(densityFunction == 'Logistic') {
    distLoc <- estimations[[1]]
    distScale <- estimations[[2]]
    
    # Draw QQ-Plot for the Logistic distribution
    qqplot(dataset,
           qlogis(ppoints(lengthPpoints), location=distLoc, scale=distScale),
           xlab='Observed Values',
           ylab=paste('Quantiles Logistic(alfa=', distLoc, ',beta=',
                      distScale,')'),
           main=expression('Logistic QQ-plot'))
    
    # Draw least squares line overlapped with the QQ-Plot
    qqline(dataset,
           distribution=function(p) qlogis(p, location=distLoc, scale=distScale),
           col=1)
  } else {
    stop('Invalid argument for function DrawQQPlot!')
  }
  
  dev.off()
}
