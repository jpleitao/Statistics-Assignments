# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

AnalyseAgeGroupFastFood <- function(dataset) {
  # Analyses the average weekly number of purchases in the last month and
  # determines if it is significantly different according to the age group of
  # the people that answered the survey.
  #
  # Args:
  #   dataset: A dataframe with the observed data.
  #
  
  DEBUG = TRUE
  
  # TODO(jpleitao): Justify here why we are considering ANOVA
  
  # Start by getting all the age groups
  ageGroups <- unique(dataset$Idade)
  
  significanceLevel = 0.05
  
  # Check if we can perform ANOVA!
  canPerformAnova <- TestIfAnova(dataset, ageGroups, significanceLevel)
  
  # Regardless of the type of test that we perform, we will consider the
  # following two hypothesis:
  # H0: m1 = m2 = ... = mr, for r the number of groups
  # H1: The means of the groups are not all equal
  # 
  
  if (canPerformAnova) {
    # ANOVA
    
    testData <- c()
    testGroup <- c()
    
    for (i in ageGroups) {
      groupData <- subset(dataset, Idade==i)
      groupData <- groupData$Compras
      
      testData <- c(testData, groupData)
      testGroup <- c(testGroup, rep(i, length(groupData)))
    }
    
    cat('\n\n')
    print(testData)
    cat('\n')
    
    dataF <- data.frame(y=testData, group=factor(testGroup))
    fitAnova <- lm(formula=testData ~ testGroup, data=dataF)
    
    testResult <- anova(fitAnova)
    
    pValue <- testResult$`Pr(>F)`
    pValue <- pValue[1]
    
  } else {
    # Kruskal-Wallis
    
    testData <- list()
    
    for (i in ageGroups) {
      groupData <- subset(dataset, Idade==i)
      groupData <- groupData$Compras
      
      testData[[i]] <- groupData
    }
    
    print(testData)
    
    cat('\n-> Kruskal-Wallis Test for Mean Equality\n')
    testResult <- kruskal.test(testData)
    
    pValue <- testResult$p.value
  }

  if (DEBUG) {
    cat('\n\n')
    print(testResult)
    cat('\n')
  }
  
  if (pValue <= significanceLevel) {
    cat(paste('\nThe p-value of the performed test is too small and, at the ',
                'specified significance level the null hypothesis is rejected ',
                'and the means of the groups are considered to be ',
                'statistically different (The alternative hypothesis is ',
                'accepted)', sep=''))
  } else {
    cat(paste('\nThe p-value of the performed test is high and, at the ',
                'specified significance level the null hypothesis is accepted ',
                'and the means of the groups are considered to be ',
                'statistically equal', sep=''))
  }
}