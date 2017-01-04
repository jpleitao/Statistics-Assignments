# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

AnalyseAgeGroupFastFood <- function(dataset) {
  #
  # Args:
  #   dataset: A dataframe with the observed data.
  #
  
  # TODO(jpleitao): Justify here why we are considering ANOVA
  
  # Check if we can perform ANOVA!
  canPerformAnova <- TestIfAnova(dataset)
  
  print(canPerformAnova)
  
  # TODO(jpleitao): Add code for if we can perform ANOVA and if we cannot!
  
}