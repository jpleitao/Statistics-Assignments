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
  source(paste('/media/jpleitao/Data/PhD/PDCTI/Statistics/',
               'Statistics-Assignments/A2/code/DrawHistograms.R', sep=''))
  source(paste('/media/jpleitao/Data/PhD/PDCTI/Statistics/',
               'Statistics-Assignments/A2/code/LoadDataset.R', sep=''))
  
  # Load dataset
  dataset <- LoadDataset(paste('/media/jpleitao/Data/PhD/PDCTI/Statistics/', 
                               'Statistics-Assignments/A2/data/Amostra.xlsx',
                               sep=''))
  
  # Draw histograms and save it to a file
  # DrawHistograms(dataset)
  
  # Draw QQ-Plots
  # DrawQQPlots(dataset)
  
  return(c(mean(dataset), sd(dataset)))
  
}

l<- main()

meanDataset <- l[1]
sdDataset <- l[2]
