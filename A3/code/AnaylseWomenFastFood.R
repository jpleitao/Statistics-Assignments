# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

AnaylseWomenFastFood <- function(dataset) {
  # Analyses the average number of weekly purchases made by women in the last
  # month and determines if its average level is significantly higher than 2.5 .
  #
  # Args:
  #   dataset: A dataframe with the observed data.
  #
  
  DEBUG = TRUE
  
  significanceLevel = 0.05
  confidenceLevel = 1 - significanceLevel
  
  # Filter data: Only collect information for the women and then select only
  # the "Compras" field
  womenData <- subset(dataset, Sexo=='2')
  womenData <- womenData$Compras
  
  # Just plot the histogram of the desired data
  png('images/Histogram_WomenData.png', width=1000, height=800)
  hist(womenData, main='Histogram of Women Average Fast-Food Purchases')
  
  # Test for normality with Shapiro-Wilk
  cat('\n-> Shapiro-Wilk Test for Normality\n')
  shapiroResult <- shapiro.test(womenData)
  
  if (DEBUG) {
    print(shapiroResult)
  }
  
  if (shapiroResult$p.value <= significanceLevel) {
    cat(paste('\nThe obtained p-value for the Shapiro-Wilk Test is smaller ',
              'than the significance level ', significanceLevel, '. Therefore,',
              ' at the referred significance level we reject the null ',
              'hypothesis stating that the observed data follows a normal ',
              'distribution.', sep=''))
  } else {
    cat(paste('\nThe obtained p-value for the Shapiro-Wilk Test is larger ',
              'than the significance level ', significanceLevel, '. Therefore,',
              ' at the referred significance level we accept the null ',
              'hypothesis stating that the observed data follows a normal ',
              'distribution.', sep=''))
  }
  
  # The obtained p-value for the Shapiro-Wilk Test is very small (7.191e-12)
  # therefore, at the significance level of 0.05, we reject the null hypothesis
  # stating that the observed data follows a normal distribution.
  # Nevertheless, since the length of the observed data is considerably higher
  # than 30 (168 > 30) we can still apply the T-Test for the mean of a
  # population and obtain an approximate p-value.
  
  cat('\n-> T-Test for the mean of a population\n')
  # In the T-Test for the mean of a population we are considering the following
  # hypothesis:
  #     H0: m = 2.5
  #     H1: m > 2.5
  tResult <- t.test(womenData, alternative='greater', mu=2.5, paired=FALSE,
                    conf.level=confidenceLevel)
  
  if (DEBUG) {
    print(tResult)
  }
  
  if (tResult$p.value > significanceLevel) {
    cat(paste('\nWe obtained an approximate p-value of ', tResult$p.value,
              ' which is greater than the selected significance level alfa=',
              significanceLevel, ' . Therefore, at the referred significance ',
              'level, we accept the null hypothesis stating that m=2.5',
              sep=''))
  } else {
    cat(paste('\nWe obtained an approximate p-value of ', tResult$p.value,
              ' which is smaller or equal to the selected significance level ',
              'alfa=', significanceLevel, ' . Therefore, at the referred ',
              'significance level, we rejwct the null hypothesis stating that ',
              'm=2.5 and accept the alternative hypothesis stating that ',
              'm > 2.5', sep=''))
  }
  
  # We obtain an approximate p-value of 0.2779 for the T-Test. For a
  # significance level of alfa = 0.05 we verify that p-value > alfa. Therefore,
  # and at the referred significance level, we accept the null hypothesis
  # stating that m = 2.5
  
  dev.off()
}