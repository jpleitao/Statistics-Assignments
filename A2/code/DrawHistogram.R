# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

# Determines if only the frequency histogram of the data is plotted, or if a
# density of a probability distribution is also plotted
PLOT_DENSITY = FALSE

DrawHistogram <- function(dataset) {
  #
  # Args:
  #   dataset: An array of doubles with the data whose histogram will be plotted.
  #
  #
  if (PLOT_DENSITY) {
    hist(dataset, freq=FALSE)
    
    # The following line is just here because the distribution resembles the
    # Normal distribution and will only be added to overlap the normal curve to
    # the obtained histogram
    curve(dnorm(x, mean=mean(dataset), sd=sd(dataset)), col="darkblue", lwd=2,
          add=TRUE)
  } else {
    # Just plot the histogram
    hist(dataset, freq=FALSE)
  }
}
