# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

DrawQQPlots <- function(dataset) {
  # TODO(jpleitao): Document this!
  #
  # Args:
  #
  #
  DrawQQPlot(dataset, 'Normal')
  DrawQQPlot(dataset, 'Gamma')
  DrawQQPlot(dataset, 'Logistic')
}

DrawQQPlot <- function(dataset, density_function) {
  # TODO(jpleitao): Document this!
  #
  # Args:
  #
  #
  meanDataset <- mean(dataset)
  sdDataset <- sd(dataset)
  lengthPpoints <- length(dataset)
  
  # Save plot to file
  # png(paste('images/QQPlot_', density_function, '.png'), width=1000, height=800)
  
  if(density_function == 'Normal') {
    # Draw QQ-Plot for the Normal distribution
    qqplot(dataset,
           qnorm(ppoints(lengthPpoints), mean=meanDataset, sd=sdDataset),
           xlab='Observed Values',
           ylab=paste('Quantiles Normal(u=', meanDataset, ',sigma=',
                      sdDataset, ')'),
           main=expression('Normal QQ-plot'))
    
    # Draw least squares line overlapped with the QQ-Plot
    qqline(dataset,
           distribution=function(p) qnorm(p, mean=meanDataset, sd=sdDataset),
           col=1)
  } else if(density_function == 'Gamma') {
    distShape <- meanDataset^2/sdDataset^2
    distScale <- sdDataset^2/meanDataset
    
    # Draw QQ-Plot for the Gamma distribution
    qqplot(dataset,
           qgamma(ppoints(lengthPpoints), shape=distShape, scale=distScale),
           xlab='Observed Values',
           ylab=paste('Quantiles Gamma(k=', distShape, ',teta=', sdDataset,')'),
           main=expression('Gamma QQ-plot'))
    
    # Draw least squares line overlapped with the QQ-Plot
    qqline(dataset,
           distribution=function(p) qgamma(p, shape=distShape, scale=distScale),
           col=1)
  } else if(density_function == 'Logistic') {
    distLoc <- meanDataset
    distScale <- sqrt(3)*(sdDataset/pi)
    
    # Draw QQ-Plot for the Logistic distribution
    qqplot(dataset,
           qlogis(ppoints(lengthPpoints), location=distLoc, scale=distScale),
           xlab='Observed Values',
           ylab=paste('Quantiles Logistic(alfa=', distLoc, ',beta=',
                      sdDataset,')'),
           main=expression('Logistic QQ-plot'))
    
    # Draw least squares line overlapped with the QQ-Plot
    qqline(dataset,
           distribution=function(p) qlogis(p, location=distLoc, scale=distScale),
           col=1)
  } else {
    stop('Invalid argument for function DrawQQPlot!')
  }
  
  # dev.off()
}
