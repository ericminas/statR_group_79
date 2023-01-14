### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December X. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Please use the ggplot2 library for all graphs in this homework.


## Please write below your (and your teammates') name, matriculation number. 
## Name: Chaahat Jain
## Matriculation number: 7025099

## Name: Denis Krieger
## Matriculation Number: 7021772

## Name: Eric Minas
## Matriculation Number: 2568884

###########################################################################################
###########################################################################################

library(languageR)
library(ggplot2)
library(dplyr)
library(carData)
#######################
### Exercise 1: Correlation
#######################

## We will use the dataset UN98 from the package carData. 
## a) Load the package and inspect the data set
# install.packages("carData")
head(UN98)
str(UN98)
summary(UN98)

## b) create the dataset AsiaMale, containing the variables educationMale lifeMale GDPperCapita 
##    economicActivityMale and illiteracyMale and the subset of Asian countries.
AsiaMale <- subset(UN98, region == "Asia", c("educationMale", "lifeMale", "GDPperCapita", "economicActivityMale", "illiteracyMale"))

## c) Let's say you're interested in whether there is a linear relationship between 
## illiteracy percentage and life expectancy of males in the different countries. 
## Take a look at the relationship between the two variables by 
## means of a scatterplot (use the ggplot library for this).
ggplot(AsiaMale, aes(illiteracyMale,lifeMale)) + geom_point()

## d) Judging from the graph, do you think that the two variables are 
## in any way correlated with one another?
# The two variables are correslated. There is a negative correlation.

## e) Get the correlations between all variables in the data set using cor().
## Tell R to only include complete pairs of observations. 
cor(AsiaMale, use = "pairwise.complete.obs")

## f) Concentrate on the row for the life expectancy in males. Interpret the five numbers you see there
##   explaining for each number which direction the correlation takes and how strong it is.
# - educationMale :       medium correlation,       direction = lower-left to upper-right
# - lifemale :            very strong correlation,  direction = lower-left to upper-right (is the same variable)
# - GDPPerCapita:         medium correlation,       direction = lower-left to upper-right
# - economicActivityMale: negligible correlation,   direction = upper-left to lower-right (if any)
# - illiteracyMale:       medium correlation,       direction = upper-left to lower-right 

## g) Is the correlation between life expectancy and GDPperCapita significant? Use cor.test()
cor.test(AsiaMale$GDPperCapita, AsiaMale$lifeMale)
# yes there is a significant correlation as p-value is < 0.05. 

## h) Calculate the Spearman rank correlation between life expectancy and GDPperCapita and compare
## it to the pearson correlation calculated above.
cor.test(AsiaMale$GDPperCapita, AsiaMale$lifeMale, method = "spearman")
# the correlation is stronger in spearman case than pearson 

## i) make a scatterplot of this relationship.
ggplot(AsiaMale, aes(GDPperCapita,lifeMale)) + geom_point()

## j) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson 
## correlation to describe the relationship between the two variables?
# Pearson detects linear relationships 
# Spearman detects monotonic increase
# Since the relationship is monotonic but not linear, we use Spearman 

## k) Using the function paired.r from the package psych, compare the correlations between life expectancy 
##  and economic activity on the one hand, and life expectancy and illiteracy on the other hand.
##  Hint: the degrees of freedom in a correlation test are equal to N-2
# install.packages("psych")
library(psych)
paired.r(cor(AsiaMale$lifeMale, AsiaMale$economicActivityMale, use = "pairwise.complete.obs"), 
         cor(AsiaMale$lifeMale, AsiaMale$illiteracyMale, use = "pairwise.complete.obs"),
         n = length(AsiaMale$lifeMale))
## l) What do you conclude from k?
# There is a significant difference in the correlations of life expectancy wrt economicActivity compared to illiteracy.

## m) What would be the result, if the two variables would be independent?
# If both variables are independent, then z = 0 since ideally correlation would be 0.

################################
### Exercise 2: Regression
################################


## We will use the same dataset as above, but first scale the GDP to be in the unit of
## thousand dollars
AsiaMale$GDPt = AsiaMale$GDPperCapita/1000

## a) Run a regression model of life expectancy by GDPt and look at the summary.
## General form: 
## "modelname <- lm(outcome ~ predictor, data = dataFrame)"
## "summary(modelname)"

GDPLife <- lm(lifeMale ~ GDPt, data = AsiaMale)

## b) Interpret the model from a. What do intercept and the coefficient of GDPt tell you?
summary(GDPLife)
# Intercept tells us the average lifeExpectancy when GDP is 0. 
# Coefficient tells us the average increase in lifeExpectancy predicted when GDP increases by 1 unit.
# So, when GDP increases by 1 unit, the average lifeExpectancy goes up by half a year.

## c) What about the model fit: What proportion of the total variance is explained by your model?
# install.packages("broom")
# library(broom)
# GDPLife %>% 
  # Get the model-level details
  # glance() %>% 
  # Pull out r.squared
  # pull(r.squared)
summary(GDPLife)
# GDP explaines 38% of the total lifeExpectancy

## d) Now let's turn to the relationship between life expectancy and illiteracy.  Run the regression and 
# interpret.
IlliLife <- lm(lifeMale ~ illiteracyMale, data = AsiaMale)
summary(IlliLife)
# So, when illiteracy decreases by 1 unit, the average lifeExpectancy goes up by around 3 months.

## e) Plot lifeMale by illiteracyMale and add a regression line to your plot
ggplot(AsiaMale, aes(lifeMale, illiteracyMale)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

###################################
### Exercise 3: Multiple Regression
###################################

## We will use the same data set as in 2. This time, we will look at the effect of illiteracy rate
## and GDP on life expectancy simultaneously. 

## a) Run a multiple regression model with illiteracyMale and GDPt as predictors
## General form: 
## "modelname <- lm(outcome ~ predictor1+predictor2+.., data = dataFrame, na.action = an action)"
## "summary(modelname)"
GDPIlliLife <- lm(lifeMale ~ illiteracyMale + GDPt, data = AsiaMale, na.action = na.omit)
## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?
summary(GDPIlliLife)
# intercept gives the value when both predictors are 0. 
# Coefficients tell the influence these predictors have in the model.
# both predictors are significant (p-value almost 0).

## c) Compare to the model in 2a (only including GDP), has the model fit improved? How about
## the model in 2d (only including illiteracy)?
# This model explains 63% of the variability in the data which is much better compared to the previous two models. 

## d) Look up the GDP and illiteracyMale for United.States and Brazil in the original data set (UN98)
US <- UN98["United.States", c(12, 9)]
Brazil <- UN98["Brazil", c(12, 9)]

## e) Using the model from 3a:  What is the predicted life expectancy for United.States and Brazil?
##  Calculate "by hand", i.e. do not use predict() and show your calculation. Don't forget to divide
##  the GDPperCapita by 1000 first!
USPrec <- coefficients(GDPIlliLife)[1] + coefficients(GDPIlliLife)[2]*US$illiteracyMale +
  coefficients(GDPIlliLife)[3]*US$GDPperCapita/1000
BrazilPrec <- coefficients(GDPIlliLife)[1] + coefficients(GDPIlliLife)[2]*Brazil$illiteracyMale +
  coefficients(GDPIlliLife)[3]*Brazil$GDPperCapita/1000

## f) Run an additional model of life expectancy for the AsiaMale data set including also economicActivityMale
GDPIlliEcoLife <- lm(lifeMale ~ illiteracyMale + GDPt + economicActivityMale, data = AsiaMale, na.action = na.omit)

## g) Do you think inclusion of economicActivity into the model is a good idea?
summary(GDPIlliEcoLife)
# This is a bad idea. Economic Activity is not significant (p-value = 0.85) & the R-squared value has decreased.
# There is serious risk of overfitting by doing this
