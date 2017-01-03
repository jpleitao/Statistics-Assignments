# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 2

ComputeKolmogorovSmirnovTests <- function(dataset, estimations) {
  # Performs the Kolmogorov-Smirnov Test for each of the probability
  # distributions considered (Normal, Gamma and Logistic) in order to determine
  # which one of these distributions is more likely to justify the observed data. 
  #
  # Args:
  #   dataset: An array of doubles with the data.
  #   estimations: Maximum likelihood estimations for the parameters of the
  #                probability distribution functions considered in the work
  #                (Normal, Gamma and Logistic).
  #
  
  # Normal
  cat('\n--------------- KS Test for Normal ---------------\n')
  print(ks.test(dataset, 'pnorm', mean=estimations$normal$mu,
          sd=estimations$normal$sigma))
  
  # Gamma
  cat('\n--------------- KS Test for Gamma ---------------\n')
  print(ks.test(dataset, 'pgamma', shape=estimations$gamma$k,
          scale=estimations$gamma$teta))
  
  #Logistic
  cat('\n--------------- KS Test for Logistic ---------------\n')
  print(ks.test(dataset, 'plogis', location=estimations$logistic$loc,
          scale=estimations$logistic$scal))
}
