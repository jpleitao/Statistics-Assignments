# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

TestIfAnova <- function(dataset, groups, significanceLevel) {
  # Given a dataset, the current function checks if all the required assumptions
  # to perform an Analysis of Variance (ANOVA) are met or not.
  #
  # Args:
  #   dataset: A dataframe with the observed data.
  #   groups: The different groups considered in this exercise.
  #   significanceLevel: The significance level to consider in the tests.
  #
  # Returns:
  #   A boolean value, indicating if the ANOVA can, or cannot, be performed.
  
  library(car)
  library(nortest)
  
  # Let numberAgeGroups <- length(groups).
  # We will create "numberAgeGroups" groups. These will be our Xi. Each group
  # will have a dimension ni (for i ranging from 1 to "numberAgeGroups").
  # For each group we have observed a sample (xi_1, ... , xi_ni), and each xi_j
  # is an observation of a random variable Xi_j.
  # For each of these variables the following properties will have to be
  # verified:
  #   1) Each random variable Xi_j follows a Normal Distribution.
  #   2) All "numberAgeGroups" samples are independent from each other.
  #   3) All "numberAgeGroups" samples have the same variance.
  #
  
  stop <- FALSE
  
  # Property 1: Each random variable Xi_j follows a normal distribution
  # To verify this property we must check, for each group, is the observed
  # sample (xi_1, ... , xi_ni) can be obtained from a Normal Distribution.
  for (i in groups) {
    # Get data in the current group
    groupData <- subset(dataset, Idade==i)
    groupData <- groupData$Compras
    
    # For the current group test the normality with the K-S Test with
    # Lilliefors Correction or the Shapiro-Wilk Test for normality
    if (length(groupData) > 30) {
      cat(paste('Group ', i, ' has length higher than 30 (ni=',
                length(groupData), ')\n', sep=''))
      testResult <- lillie.test(groupData)
    } else {
      cat(paste('Group ', i, ' has length less or equal to 30 (ni=',
                length(groupData), ')\n', sep=''))
      testResult <- shapiro.test(groupData)
    }
    
    # Inspect the obtained p-value
    if (testResult$p.value <= significanceLevel) {
      cat(paste('Group ', i, ' does not follow a Normal Distribution as a ',
                'p-value of ', testResult$p.value, ' was registered\n', sep=''))
      stop <- TRUE
    } else {
      cat(paste('Group ', i, ' follows a normal distribution\n', sep=''))
    }
  }
  
  if (stop) {
    return(FALSE) 
  }
  
  # Property 2: Yes, the samples are independent from each other
  
  # Property 3: Test if all the "numberAgeGroups" samples have the same variance
  if (length(groups) > 1) {
    # Perform Levene Test for homogeneity of variance accross the groups
    temp <- data.frame(Compras=dataset$Compras, Idade=factor(dataset$Idade))
    leveneResult <- leveneTest(Compras ~ Idade, data=temp, center=mean)
    
    # Get p-value from Levene Test
    leveneP <- (leveneResult$`Pr(>F)`)[1]
    
    if (leveneP <= significanceLevel) {
      cat('The variances of the groups are different')
      return(FALSE)
    } else {
      cat('The variances of the groups are the same')
      return(TRUE)
    }
  }
  
  return(TRUE)
}