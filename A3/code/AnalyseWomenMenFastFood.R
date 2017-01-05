# Joaquim Leitão - 2011150072
# 2016/2017 School Year
# Doctoral Program in Information Science and Technology - Statistics
# Assignment 3

AnalyseWomenMenFastFood <- function(dataset) {
  # Analyses the average number of weekly purchases made by women and men in
  # the last month and determines if the average level of women purchases is
  # significantly higher than that of the man.
  #
  # Args:
  #   dataset: A dataframe with the observed data.
  
  # TODO(jpleitao):
  # VERY IMPORTANT NOTE: My problem here is whether or not I can perform a
  # T-Test, because both X and Y do not follow a Normal Distribution, but their
  # respective lengths are considerably higher than 30 (they are both higher
  # than 100...). However, if I invoke the Central Limit Theorem to state that
  # the means of samples approach a Normal Distribution (and therefore I can
  # satfisfy the assumptions of the T-Test) I get a very high p-value when the
  # means of both variables are considerably different: mean(X) = 2.586310 and
  # mean(Y) = 3.616477 (Recall that H0: mx - my = 0 vs H1: mx - my > 0).
  # So it doesn't make much sense to actually accept the null hypothesis and say
  # that X and Y have the same mean (or at least say that we do not have enough
  # statistical evidence to reject their mean equality).
  #
  # Is it possible that I cannot invoke the central limit theorem here??
  
  library(car)
  
  DEBUG = TRUE

  significanceLevel = 0.05
  confidenceLevel = 1 - significanceLevel
    
  # X: "Average number of fast-food purchases per week made by women in the
  #    last month"
  # Y: "Average number of fast-food purchases per week made by men in the
  #    last month"
  #
  # We want to test H0: mx - my = 0 vs H1: mx - my > 0

  # Filter Women data
  womenData <- subset(dataset, Sexo=='2')
  womenData <- womenData$Compras
  
  # Filter Men data
  menData <- subset(dataset, Sexo=='1')
  menData <- menData$Compras
  
  # Test X and Y for normality
  # In the previous exercise we have already tested X for normality and,
  # according to the Shapiro-Wilk Test at a significance level of 0.05 we reject
  # the null hypothesis stating that the data follows a normal distribution.
  # Therefore, in this exercise, we just need to perform the same test for Y.
  cat('\n-> Shapiro-Wilk Test for Normality (Men)\n')
  shapiroResult <- shapiro.test(menData)
  
  if (DEBUG) {
    print(shapiroResult)
  }
  
  if (shapiroResult$p.value <= significanceLevel) {
    cat(paste('\nThe obtained p-value for the Shapiro-Wilk Test is smaller ',
              'than the significance level ', significanceLevel, '. Therefore,',
              ' at the referred significance level we reject the null ',
              'hypothesis stating that the observed data for the men follows ',
              'a normal distribution.', sep=''))
  } else {
    cat(paste('\nThe obtained p-value for the Shapiro-Wilk Test is larger ',
              'than the significance level ', significanceLevel, '. Therefore,',
              ' at the referred significance level we accept the null ',
              'hypothesis stating that the observed data for the men follows ',
              'a normal distribution.', sep=''))
  }
  
  # The Shapiro-Wilk test for normality for the Y variable returns a p-value of
  # 2.515e-10. Indeed, this is a very small value which, at the significance
  # level of 0.05, makes us reject the null hyphtoesis stating that the data
  # follows a normal distribution.
  #
  # Even though both X and Y variables do not follow a normal distribution,
  # since the length of the observed data for both variables is considerably
  # higher than 30 (168 for women and 176 for men) we can still aplly the T-Test
  # for the mean difference of two populations, obtaining an approximate
  # p-value.
  #
  # Furthermore, because X and Y are independent, we will be applying the T-Test
  # for the mean difference of two independent populations. The next step in
  # this exercise is to evaluate whether or not the variances of X and Y are
  # equal. Such evaluation can be achieved by performing the Levene's Test.
  cat('\n-> Levene\'s Test for Variance Equality\n')
  
  cat(paste('\n  *) Means: X=', mean(womenData), '   Y=', mean(menData), '\n\n',
            sep=''))
  
  temp <- data.frame(Compras=dataset$Compras, Sexo=factor(dataset$Sexo))
  leveneResult <-leveneTest(Compras ~ Sexo, data=temp)
  
  if (DEBUG) {
    print(leveneResult)
  }
  
  # Get p-value from Levene Test
  leveneP <- (leveneResult$`Pr(>F)`)[1]
  
  if (leveneP <= significanceLevel) {
    cat('\n     Result: Variances of X and Y are different\n')
    varianceEquality = FALSE
  } else {
    cat('\n     Result: Variances of X and Y are equal\n')
    varianceEquality = TRUE
  }
  
  # Perform T-Test according to the result of the Levene Test
  #     H0: mx - my = 0
  #     H1: mx - my > 0
  cat('\n-> T-Test Mean Difference\n')
  tResult <- t.test(womenData, menData, alternative='greater', mu=0,
                    paired=FALSE, var.equal=varianceEquality, conf.level=0.95)

  if (DEBUG) {
    print(tResult)
  }
  
  if (tResult$p.value > significanceLevel) {
    cat(paste('\nWe obtained an approximate p-value of ', tResult$p.value,
              ' which is greater than the selected significance level alfa=',
              significanceLevel, ' . Therefore, at the referred significance ',
              'level, we accept the null hypothesis stating that mx - my = 0',
              sep=''))
  } else {
    cat(paste('\nWe obtained an approximate p-value of ', tResult$p.value,
              ' which is smaller or equal to the selected significance level ',
              'alfa=', significanceLevel, ' . Therefore, at the referred ',
              'significance level, we reject the null hypothesis stating that ',
              'mx - my = 0 and accept the alternative hypothesis stating that ',
              'mx - my > 0', sep=''))
  }
  
}