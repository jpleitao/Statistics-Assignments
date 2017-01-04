# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

main <- function() {
  # Starting point of the execution.
  #
  
  # Add source to scripts
  source('code/LoadDataset.R')
  source('code/AnaylseWomenFastFood.R')
  source('code/AnalyseWomenMenFastFood.R')
  source('code/TestIfAnova.R')
  source('code/AnalyseAgeGroupFastFood.R')
  
  # Load dataset
  dataset <- LoadDataset('data/FastFoodNovo.xlsx')
  
  # Analyse average fast-food consumptions by women
  cat('\n------------- Analyse Fast-Food consumptions by Women -------------\n')
  # AnaylseWomenFastFood(dataset)
  
  # Compare average fast-food consumptions by women and men
  # AnalyseWomenMenFastFood(dataset)
  
  # Compare average fast-food consumptions according to age group
  AnalyseAgeGroupFastFood(dataset)
  
  return(dataset)
}

dataset <- main()