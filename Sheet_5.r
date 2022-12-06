### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, December X. Write the code below the questions.
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via cms
## Please use the ggplot2 library for all graphs in this homework.


## Please write below your (and your teammates) name, matriculation number.
## Name: Chaahat Jain
## Matriculation number: 7025099

## Name: Denis Krieger
## Matriculation Number: 7021772

## Name: Eric Minas
## Matriculation Number: 2568884

## Only 1 member needs to submit!

###########################################################################################
###########################################################################################
install.packages("psych")

library(languageR)
library(ggplot2)
library(dplyr)
library(carData)
library(psych)
#######################
### Exercise 1: Correlation
#######################

## We will use the dataset UN98 from the package carData.
## a) Load the package and inspect the data set
summary(carData::UN98)

## b) create the dataset AsiaMale, containing the variables educationMale lifeMale GDPperCapita
##    economicActivityMale and illiteracyMale and the subset of Asian countries.
AsiaMale <-
  carData::UN98 %>%
  filter(region == "Asia") %>%
  subset(
    select = c(
      "educationMale",
      "lifeMale",
      "GDPperCapita",
      "economicActivityMale",
      "illiteracyMale"
    )
  )

## c) Let's say you're interested in whether there is a linear relationship between
## illiteracy percentage and life expectancy of males in the different countries.
## Take a look at the relationship between the two variables by
## means of a scatterplot (use the ggplot library for this).
ggplot(AsiaMale, aes(lifeMale, illiteracyMale)) + geom_point()

## d) Judging from the graph, do you think that the two variables are
## in any way correlated with one another?
# They seem not be be correlated. (no trend visible)

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
cor.test(x = AsiaMale$lifeMale, y = AsiaMale$GDPperCapita)
# Yes, the result is wihtin the confidence interval.

## h) Calculate the Spearman rank correlation between life expectancy and GDPperCapita and compare
## it to the pearson correlation calculated above.
cor(
  x = AsiaMale$lifeMale,
  AsiaMale$GDPperCapita,
  method = "spearman",
  use = "pairwise.complete.obs"
)
# difference = | 0.6176 - 0.6761 | = 0.0585
# The correlation is higher for the speaman method.

## i) make a scatterplot of this relationship.
ggplot(AsiaMale, aes(lifeMale, GDPperCapita)) + geom_point()

## j) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson
## correlation to describe the relationship between the two variables?
# Spearman is better, because there are a few outliers that influence influence the correlation.
# These outliers are better represented with the spearman method.

## k) Using the function paired.r from the package psych, compare the correlations between life expectancy
##  and economic activity on the one hand, and life expectancy and illiteracy on the other hand.
##  Hint: the degrees of freedom in a correlation test are equal to N-2
r_le_ea <-
  cov(AsiaMale$lifeMale, AsiaMale$economicActivityMale, use = "pairwise.complete.obs") / (sd(AsiaMale$lifeMale, na.rm = TRUE) * sd(AsiaMale$economicActivityMale, na.rm = TRUE))
r_le_i <-
  cov(AsiaMale$lifeMale, AsiaMale$illiteracyMale, use = "pairwise.complete.obs") / (sd(AsiaMale$lifeMale, na.rm = TRUE) * sd(AsiaMale$illiteracyMale, na.rm = TRUE))


le_VS_ea <-
  paired.r(xy = r_le_ea, r_le_i, n = length(AsiaMale$lifeMale))
le_VS_ea

## l) What do you conclude from k?
# That there is a strong correlation between the models

## m) What would be the result, if the two variables would be independent
# TODO

################################
### Exercise 2: Regression
################################


## We will use the same dataset as above, but first scale the GDP to be in the unit of
## thousand dollars
AsiaMale$GDPt = AsiaMale$GDPperCapita / 1000

## a) Run a regression model of life expectancy by GDPt and look at the summary.
## General form:
## "modelname <- lm(outcome ~ predictor, data = dataFrame)"
## "summary(modelname)"

lifeExpByGDPt <- lm(lifeMale ~ GDPt, data = AsiaMale)
summary(lifeExpByGDPt)

## b) Interpret the model from a. What do intercept and the coefficient of GDPt tell you?
# intercept = the point where the regression line intercepts the y-axis, in this case, it is 62.65.
# coefficient = the slope of the line, in this case 0.5, which means that the line is slowly going up.

## c) What about the model fit: What proportion of the total variance is explained by your model?
# the variance between the life expectancy and the GDP per capita, scaled to thousands of dollars.

## d) Now let's turn to the relationship between life expectancy and illiteracy.  Run the regression and
# interpret.
lifeExpByIll <- lm(lifeMale ~ illiteracyMale, data = AsiaMale)
summary(lifeExpByIll)

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
lifeExpByGDPt_Ill <-
  lm(lifeMale ~ illiteracyMale + GDPt,
     data = AsiaMale,
     na.action = na.exclude)
summary(lifeExpByGDPt_Ill)

## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?
# the intercepts show the starting point of both regression lines
# the esitmated stadard errors is the estimated standard error in regards to both means of illiteracy and GDPt
# the t value is the t-test value for the predictor
# Pr(>|t|) is the p-value associated with the corresponding t-value

# Because all the Pr(...) values are < 0.05, they are significant.

## c) Compare to the model in 2a (only including GDP), has the model fit improved? How about
## the model in 2d (only including illiteracy)?
# The model has sightly improved for GDPt, and has gotten slightly worse for illiteracy.

## d) Look up the GDP and illiteracyMale for United.States and Brazil in the original data set (UN98)
illiteracyMale <-
  carData::UN98 %>% filter(region == "United.States" |
                             region == "Brazil") %>% subset(select = c("illiteracyMale"))
illiteracyMale

## e) Using the model from 3a:  What is the predicted life expectancy for United.States and Brazil?
##  Calculate "by hand", i.e. do not use predict() and show your calculation. Don't forget to divide
##  the GDPperCapita by 1000 first!

## f) Run an additional model of life expectancy for the AsiaMale data set including also economicActivityMale

## g) Do you think inclusion of economicActivity into the model is a good idea?
