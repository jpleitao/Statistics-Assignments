# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

DrawHistograms <- function(dataset) {
  # General function to draw a frequency histogram of the data overlapped with
  # the density function of the three distributions to be considered in this
  # work: Normal, Gamma and Logistic.
  #
  # Args:
  #   dataset: An array of doubles with the data whose histogram will be plotted.
  #
  DrawHistogram(dataset, 'Normal')
  DrawHistogram(dataset, 'Gamma')
  DrawHistogram(dataset, 'Logistic')
}

DrawHistogram <- function(dataset, plot_density) {
  # Function to draw a frequency histogram of the supplied dataset overlapped
  # with the density function of a given probability distribution.
  # The parameters of this probability distribution are estimated with the
  # method of moments.
  # 
  # Args:
  #   dataset: An array of doubles with the data whose histogram will be plotted.
  #   plot_density: A string containing the name of the probability distribution
  #                 whose density function will be overlapped to the dataset's
  #                 frequency histogram.
  #
  #
  meanDataset <- mean(dataset)
  sdDataset <- sd(dataset)
  
  # Save plot to file
  png(paste('images/Histogram_', plot_density, '.png'), width=1000, height=800)
  
  if (plot_density == 'Normal') {
    # Plot the histogram and add the corresponding distribution
    histTitle <- paste('Histogram with Normal density (u=', meanDataset,
                       ', sigma=', sdDataset)
    
    hist(dataset, freq=FALSE, main=histTitle)
    
    # Overlap the normal curve to the obtained histogram
    curve(dnorm(x, mean=meanDataset, sd=sdDataset), col="darkblue", lwd=2,
          add=TRUE)
    
  } else if (plot_density == 'Gamma') {
    # Values for the parameters obtained by solving a system of equations
    distShape <- meanDataset^2/sdDataset^2
    distScale <- sdDataset^2/meanDataset
    
    # Plot the histogram and add the corresponding distribution
    histTitle <- paste('Histogram with Gamma density (k=', distShape,
                       ', teta=', distScale)
      
    hist(dataset, freq=FALSE, main=histTitle)
    
    # Overlap the gamma curve to the obtained histogram
    curve(dgamma(x, shape=distShape, scale=distScale), col="darkblue", lwd=2,
          add=TRUE)
    
  } else if (plot_density == 'Beta') {
    # Values for the parameters obtained by solving a system of equations, using
    # Wolfram Alpha. Please visit:
    # <url>https://www.wolframalpha.com/input/?i=x%2F(x%2By)%3Da,+(x+*+y)%2F(+
    # (x+%2B+y+%2B+1)+*+(x+%2B+y)+*+(x+%2B+y)+)%3Db%5E2</url>
    distShape1 <- (-(meanDataset^3) + (meanDataset^2) -
                     meanDataset*(sdDataset^2)) / (sdDataset^2)
    
    distShape2 <- ( (meanDataset)^3 -2*(meanDataset^2) +
                      meanDataset*(sdDataset^2) + meanDataset -
                      (sdDataset^2) ) / (sdDataset^2)
    
    # Plot the histogram and add the corresponding distribution
    histTitle <- paste('Histogram with Beta density (alfa=', distShape1,
                       ', beta=', distShape2)
    
    hist(dataset, freq=FALSE, main=histTitle)
    
    # Overlap the beta curve to the obtained histogram
    curve(dbeta(x, shape1=distShape1, shape2=distShape2), col="darkblue",
          lwd=2, add=TRUE)
    
    # The call to the function dbeta with these parameters fires a warning
    # stating that NaN values were produced. Clearly this is not an adequate
    # distribution to model the given dataset!
    
  } else if (plot_density == 'Logistic') {
    # Values for the parameters obtained by solving a system of equations
    distLoc <- meanDataset
    distScale <- sqrt(3)*(sdDataset/pi)
    
    # Plot the histogram and add the corresponding distribution
    histTitle <- paste('Histogram with Logistic density (alfa=', distLoc,
                       ', beta=', distScale)
    
    hist(dataset, freq=FALSE, main=histTitle, ylim=c(0, 0.3))
    
    # Overlap the beta curve to the obtained histogram
    curve(dlogis(x, location=distLoc, scale=distScale), col="darkblue",
          lwd=2, add=TRUE)
  } else if (plot_density == 'Cauchy') {
    # Plot the histogram and add the corresponding distribution
    histTitle <- paste('Histogram with Cauchy density (alfa=', meanDataset,
                      ', beta=', sdDataset)
    
    hist(dataset, freq=FALSE, main=histTitle)
    
    # Overlap the normal curve to the obtained histogram
    curve(dcauchy(x, location=meanDataset, scale=sdDataset), col="darkblue",
          lwd=2, add=TRUE)
  } else {
    # Just plot the histogram
    hist(dataset)
  }
  
  dev.off()
}
