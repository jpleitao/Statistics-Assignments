# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

ComputeKolmogorovSmirnovTests <- function(dataset, estimation) {
  # TODO(jpleitao): Document this!
  #
  # Args:
  #
  #
  
  # Normal
  cat('\n---------------KS Test for Normal ---------------\n')
  print(ks.test(dataset, 'pnorm', mean=estimation$normal$mu,
          sd=estimation$normal$sigma))
  
  # Gamma
  cat('\n---------------KS Test for Gamma ---------------\n')
  print(ks.test(dataset, 'pgamma', shape=estimation$gamma$k,
          scale=estimation$gamma$teta))
  
  #Logistic
  cat('\n---------------KS Test for Logistic ---------------\n')
  print(ks.test(dataset, 'plogis', location=estimation$logistic$loc,
          scale=estimation$logistic$scal))
}
