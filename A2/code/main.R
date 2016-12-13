# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics

# Add source to scripts
source('/media/jpleitao/Data/PhD/PDCTI/Statistics/Statistics-Assignments/A2/code/DrawHistogram.R')
source('/media/jpleitao/Data/PhD/PDCTI/Statistics/Statistics-Assignments/A2/code/LoadDataset.R')

# Load dataset
dataset <- LoadDataset("/media/jpleitao/Data/PhD/PDCTI/Statistics/Statistics-Assignments/A2/data/Amostra.xlsx")


# Draw histogram and save it to a file
DrawHistogram(dataset)