### Stats with R Exercise sheet 9

##################################################################################
# Week 11: Model Families and Logistic Regression
##################################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 8. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Chaahat Jain
## Matriculation number: 7025099

## Name: Eric Minas
## Matriculation Number: 2568884

## Name: Denis Krieger
## Matriculation Number: 7021772

##################################################################################
##################################################################################

# The following line of code clears your workspace:
rm(list = ls())
install.packages("rstudioapi")
library(rstudioapi)
# Set the path to source file location:
setwd(dirname(getActiveDocumentContext()$path)) 

##################################################################################
## Exercise 1: Logistic regression
##################################################################################
install.packages("carData")
require(carData)
require(dplyr)
install.packages("lme4")
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.
## a) Build a simple logistic regression model that models the probability of survival 
##    (binary) based on sex (categorical) and  passengerClass (categorical) without 
##    an interaction and store it in mSurv. 
##    You have to use the glm() function and specify the family correctly.
mSurv <- glm(data = TitanicSurvival, survived ~ sex + passengerClass, family = binomial)

## b) Look at the summary. What group does the intercept correspond to?
summary(mSurv)
# intercept corresponds to first class female passengers

## c) Were men more likely to survive than women? Is the effect significant?
# no clue

## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##    Calculate their expected survival on the logit scale (i.e. the scale of the model) 
##    either by hand or using predict() with a new data.frame
predict(mSurv, data.frame(
  sex = c("female", "male"),
  passengerClass = c("1st", "3rd")
), type = "response")

## e) Transform your results from d to the probability scale, using the formula given on the slides. 
##    You can check your calculation by asserting the probabilities lie in the 0-1 range. 
##    For whom does the model predict the higher probability of survival?

# The model predicts a higher chance of survival for Rose (89%) compared to Jack (10%)



##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption 
## and sleep (among others). The data set "coffee.csv" contains data from 10 students, 
## who reported on 10 randomly chosen days of the year: 
##  sleep:  how many hours of sleep they had in the previous night
##  mood:   how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
## In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they don't feel well
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat
coffeedat <- read.csv(file = "coffee.csv")



## b) Plot the number of consumed cups of coffee in three individual scatterplots 
##    by sleep, mood, and temperature. 
##    You can use geom_jitter() to get a nicer plot
ggplot(coffeedat, aes(sleep, coffee)) + geom_point()
ggplot(coffeedat, aes(mood, coffee)) + geom_point()
ggplot(coffeedat, aes(temperature, coffee)) + geom_point()

## c) Can you detect an obvious relationship in any of the plots?
# In temperature, most coffee is consumed between 10 - 25.
# Worse mood is related to more coffee consumption.
# Less sleep is related to more coffee consumption

## d) Fit a simple linear regression model with all three predictors and store it in linmod
linmod <- lm(data = coffeedat, coffee ~ sleep + mood + temperature)

## e) Fit a generalized linear model with the appropriate family 
##    (hint: coffee is a count variable) and store it in poimod

poimod <-
  glm(coffee ~ sleep + mood + temperature,
      family = poisson ,  ### How did you figure out it should be poisson??
      data = coffeedat)
## f) Look at the two summaries of the models and write what changed?
summary(linmod)
summary(poimod)

# linear model suggests sleep and temperature are insignificant which is different from glm.


## g) In fact, we have repeated measures in our design, so refit the model 
##    including a random intercept for subject using glmer() with the correct 
##    family specification and store it in mixedpoi
mixedpoi <-
  glmer (coffee ~ sleep + mood + temperature
         + (1 | subj),
         family = poisson,
         data = coffeedat)

## h) Look at the summary and report what changed in comparison to both linmod and poimod.
summary(mixedpoi)
# temperature is now insignificant.

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin
mixedlin <-
  lmer (coffee ~ sleep + mood + temperature
         + (1 | subj),
         data = coffeedat)

## j) Compare the AIC for all four models. Which one has the best fit?
summary(mixedlin)
# mixedpoi is best fit. (lin does not have AIC?)
## k) And which model is conceptually the appropriate one? Explain why.
# mixedpoi as well:
# 1) The model needs a random effect (and intercept), because a persons response to the factors (sleep, mood, temp.) vary from person to person.
# 2) The Poission distribution is the best fit as coffee is counting a number.

## l) Finally, report on the effects of interest in light of our research hypotheses 
##    specified above for the model you chose in k)
summary(mixedpoi)
## nullHyp: students consuming more coffee is not dependent on their sleep
  # We can reject this hypothesis since sleep is a significant predictor. 
  # Since slope is negative, students consume more coffee when they are tired.

## nullHyp: students consuming more coffee is not dependent on their mood
  # We can reject this hypothesis since sleep is a significant predictor. 
  # Since slope is negative, students consume more coffee when they are not feeling well

## nullHyp: students consuming more coffee is not dependent on temperature outside:
  # We cannot reject this hypothesis as temperature is not significant (p > 0.05)

