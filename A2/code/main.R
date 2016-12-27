# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

main <- function() {
  # Starting point of the execution.
  #
  # Returns:
  #    An array with the mean and the standard deviation of the dataset (This is
  #    temporary, as it is usefull in an initial stage to easily know the mean
  #    and standard deviation of the dataset).
  #
  # Add source to scripts
  source('code/DrawHistograms.R')
  source('code/LoadDataset.R')
  source('code/MaximumLikelihoodEstimations.R')
  source('code/DrawQQPlots.R')
  source('code/KolmogorovSmirnovTests.R')
  
  # Load dataset
  dataset <- LoadDataset(paste('data/Amostra.xlsx', sep=''))
  
  # Draw histograms and save it to a file
  DrawHistograms(dataset)
  
  # Maximum Likelihood Estimations
  estimations <- ComputeMaximumLikelihoodEstimations(dataset)
  PrintEstimations(estimations)
  
  # Draw QQ-Plots using MLE estimations
  DrawQQPlots(dataset, estimations)
  
  # Kolmogorov-Smirnov Test
  ComputeKolmogorovSmirnovTests(dataset, estimations)
  
  # Based on the results of the Kolmogorov-Smirnov Test, and at the significance
  # level of 95%, all of the three considered null hypothesis cannot be rejected.
  # Nevertheless, the test for the Logistic probability distribution registers
  # the higher p-value, meaning that this is the most likely distribution that
  # justifies the observed data.
  
  # Answer question 2: P(9 <= X <= 10) = F(10) - F(9)
  first <- plogis(10, location=estimations$logistic$loc,
                  scale=estimations$logistic$scal)
  second <- plogis(9, location=estimations$logistic$loc,
                   scale=estimations$logistic$scal) 
  result <- first - second
  cat('The probability of observing a concentration of the referred substance',
      ' ranging from 9 and 10 micromicrocuries is', result, '.\n\n')
  
  # Answer question 3: P(X > c) = 0.05 <=> P(X <= c) = 1 - 0.05 <=> F(c) = 0.95
  # that is, the quantile of order 95 of X
  cat('The concentration that has probability of 0.05 of being exceeded is',
      qlogis(0.95, location=estimations$logistic$loc,
             scale=estimations$logistic$scal), 'micromicrocuries.')
}

main()
