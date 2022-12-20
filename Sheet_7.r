### Stats with R Exercise sheet 7

##############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, January 1. Write the code below the questions.
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number.
## Name: Chaahat Jain
## Matriculation number: 7025099

## Name: Denis Krieger
## Matriculation Number: 7021772

## Name: Eric Minas
## Matriculation Number: 2568884

###############################################################################
###############################################################################

# The following line of code clears your workspace.

rm(list = ls())


########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises unless specified differently!
########
library(ggplot2)


##a) Load the dataset Salaries from package carData and store it in a variable called data.
# Familiarize yourself with the content of the dataset:
# https://r-data.pmagunia.com/dataset/r-dataset-package-car-salaries
library(carData)
data <- carData::Salaries


## b) Run a simple regression, just including 'years since PhD' as predictor and salary as the dependent variable
##  Store it in lm1
lm1 <- lm(salary ~ yrs.since.phd, data = data)

## c) Report and explain the effect of 'years since PhD'
summary(lm1)
anova(lm1)
# The years since PhD have an effect on the salary (p-value < 0.05).
# The more years have passed since getting the PhD, the more the person earns on average.

## d) Make a scatterplot of salary by 'years since PhD', including the regression line
ggplot(data, aes(salary, yrs.since.phd)) +
  geom_point() +
  geom_smooth(method = "lm")


## e) Next, fit a model of salary including 'years since PhD' and discipline as predictors, store it in lm2
lm2 <- lm(salary ~ yrs.since.phd + discipline, data = data)

## f) Report and explain the effects of 'years since PhD' and discipline.
summary(lm2)
anova(lm2)

# years since PhD and salary
# -> The years since PhD have an effect on the salary (p-value < 0.05).
#    The more years have passed since getting the PhD, the more the person earns on average.
# discipline and salary
# -> the discipline (in gerneal, regardless of the level) has an effect on the salary (p-vale < 0.05)
# -> discipline B has no significant effect on the salary (p-value = 0.17 > alpha), thus discipline A must have an effect.


##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
data$sal_pred = fitted(lm2)

## g) Now, plot the original data (salary by 'years since PhD' with different colors for disicpline), but use the
## fitted values (sal_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.
ggplot(data, aes(salary, yrs.since.phd, color = discipline)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = discipline), alpha = 0.1) 


## h) Run a regression model that includes also the interaction
# between 'years since PhD' and discipline and store it as lm3
lm3 <-  lm(salary ~ yrs.since.phd * discipline, data = data)

## i) Plot the results of the model! (This time no need to specify the pred data set)
ggplot(data, aes(salary, yrs.since.phd, color = discipline)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = discipline), alpha = 0.1) 


## j) Report the results of lm3 and interpret with the help of the graph in i)
summary(lm3)
anova(lm3)

# years since PhD and salary
# -> The years since PhD have an effect on the salary (p-value < 0.05).
#    The more years have passed since getting the PhD, the more the person earns on average.
# years since PhD and discipline
# -> these predictors have no effect on each other (p-vale > 0.05)
# discipline and salary
# -> the discipline (in gerneal, regardless of the level) has an effect on the salary (p-vale < 0.05)
# -> discipline B has no significant effect on the salary (p-value = 0.17 > alpha), thus discipline A must have an effect.


## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot,
## see lecture notes for syntax)
par(mfcol=c(2,3))
plot(lm3, which=seq(1,6))

## l) Interpret what you see in k) and possibly suggest further steps
# There are a few values that have a high leverage and potentially distort the mean.
# This shows when inspecting the Q-Q plot an looking on the top-right part, where the points stray from the line, 
# it is also visible in the Cook's distanve graph, as there are many bars that are high.
# The residuals are also not too good, looking at the Residuals vs Fitted graph, we can observe 
# that the red line starts to deviate from the dashed line on the right side.

# This might be an indication, that the predictors are not the best choice, or it can be the data set, that has incredibly skewed data.
