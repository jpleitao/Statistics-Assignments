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
  
  # Property 1: Each random variable Xi_j follows a normal distribution
  # To verify this property we must check, for each group, is the observed
  # sample (xi_1, ... , xi_ni) can be obtained from a Normal Distribution.
  for (i in groups) {
    # Get data in the current group
    groupData <- subset(dataset, Idade==i)
    groupData <- groupData$Compras
    
    # For the current group test the normality with the Shapiro-Wilk test
    testResult <- shapiro.test(groupData)
    
    # Inspect the obtained p-value
    if (testResult$p.value <= significanceLevel) {
      print(paste('Group ', i, ' does not follow a Normal Distribution', sep=''))
      return(FALSE)
    }
  }
  
  # Property 2: Yes, the samples are independent from each other
  # TODO(jpleitao): How to exactly justify this???
  
  # Property 3: Test if all the "numberAgeGroups" samples have the same variance
  #
  # I HAVE ONE QUESTION HERE: By saying that the samples have to have the same
  # variance are we saying that the variance of the observed samples has to be
  # the same or that the variance of the populations of the samples have to be
  # the same? The first hypothesis seems a bit harsh, so I'am pretty sure it is
  # the second one!
  
  if (length(groups) > 1) {
    # Perform Levene Test for homogeneity of variance accross the groups
    temp <- data.frame(Compras=dataset$Compras, Idade=factor(dataset$Idade))
    leveneResult <- leveneTest(Compras ~ Idade, data=temp)
    
    # Get p-value from Levene Test
    leveneP <- (leveneResult$`Pr(>F)`)[1]
    
    if (leveneP <= significanceLevel) {
      print('The variances of the groups are different')
      return(FALSE)
    } else {
      print('The variances of the groups are the same')
      return(TRUE)
    }
  }
  
  return(TRUE)
}