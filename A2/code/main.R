# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

main <- function() {
  # Add source to scripts
  source(paste('/media/jpleitao/Data/PhD/PDCTI/Statistics/Statistics-Assignments',
               '/A2/code/DrawHistogram.R', sep=''))
  source(paste('/media/jpleitao/Data/PhD/PDCTI/Statistics/Statistics-Assignments',
               '/A2/code/LoadDataset.R', sep=''))
  
  # Load dataset
  dataset <- LoadDataset(paste('/media/jpleitao/Data/PhD/PDCTI/Statistics/', 
                               'Statistics-Assignments/A2/data/Amostra.xlsx',
                               sep=''))
  
  # Draw histogram and save it to a file
  DrawHistogram(dataset)
  
  return(c(mean(dataset), sd(dataset)))
  
}

l<- main()

meanDataset <- l[1]
sdDataset <- l[2]
