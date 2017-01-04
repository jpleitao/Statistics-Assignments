# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

LoadDataset <- function(filePath) {
  # Loads the data contained in Excel file located in the specified file path.
  #
  # Args:
  #   fileParth: The path to the file containing the dataset.
  #
  # Returns:
  #   An array of doubles containing the data in the specified file.
  #
  library(readxl)
  dataset <- read_excel(filePath, col_names=TRUE)
  
  return(dataset)
}