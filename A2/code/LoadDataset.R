# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 2

LoadDataset <- function(filePath) {
  # Loads the data contained in the specified file path. Assumes the data has
  # only one collumn and that the first element is its label.
  #
  # Args:
  #   fileParth: The path to the file containing the dataset.
  #
  # Returns:
  #   An array of doubles containing the data in the specified file.
  #
  library(readxl)
  dataset <- read_excel(filePath, col_names=TRUE)
  
  # Convert dataset from "list" to "double"
  dataset <- dataset[1:nrow(dataset), 1]
  return(dataset)
}
