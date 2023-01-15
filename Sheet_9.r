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

## Name: Denis Krieger
## Matriculation Number: 7021772

## Name: Eric Minas
## Matriculation Number: 2568884

##################################################################################
##################################################################################

# The following line of code clears your workspace:
rm(list = ls())

library(rstudioapi)
# Set the path to source file location:
setwd(dirname(getActiveDocumentContext()$path))

##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.
## a) Build a simple logistic regression model that models the probability of survival
##    (binary) based on sex (categorical) and  passengerClass (categorical) without
##    an interaction and store it in mSurv.
##    You have to use the glm() function and specify the family correctly.
summary(TitanicSurvival)

mSurv <-
  glm(survived ~ sex + passengerClass,
      family = binomial(logit),
      data = TitanicSurvival)

## b) Look at the summary. What group does the intercept correspond to?
summary(mSurv)

## c) Were men more likely to survive than women? Is the effect significant?
# No, men were less likely to survive (2.5 time LESS likely then women), this effect is significant (p-value < 0.005).

## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##    Calculate their expected survival on the logit scale (i.e. the scale of the model)
##    either by hand or using predict() with a new data.frame
predict(mSurv, data.frame(
  sex = c("female", "male"),
  passengerClass = c("1st", "3rd")
))
# odds rose =  2.11
# odds jack = -2.13

## e) Transform your results from d to the probability scale, using the formula given on the slides.
##    You can check your calculation by asserting the probabilities lie in the 0-1 range.
##    For whom does the model predict the higher probability of survival?

#rose
print(exp(2.11) / (1 + exp(2.11))) # = 0.8918713
# jack
print(exp(-2.13) / (1 + exp(-2.13))) # = 0.106215
# jacks's is not precise, because I rounded the value from the step before.

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

# could not get it to work without a new library
library(ggpubr)
base_p <- ggplot(coffeedat, aes(y = coffee))
ggarrange(
  base_p + geom_jitter(aes(x = sleep)),
  base_p + geom_jitter(aes(x = mood)),
  base_p + geom_jitter(aes(x = temperature)),
  ncol = 3
)

## c) Can you detect an obvious relationship in any of the plots?
# yes:
# increased values for sleep and mood decrease the average coffee consumption.
# no obvious relationship can be found for temperature.

## d) Fit a simple linear regression model with all three predictors and store it in linmod
linmod <- lm(coffee ~ sleep + mood + temperature, data = coffeedat)

## e) Fit a generalized linear model with the appropriate family
##    (hint: coffee is a count variable) and store it in poimod
poimod <-
  glm(coffee ~ sleep + mood + temperature,
      family = poisson ,
      data = coffeedat)

## f) Look at the two summaries of the models and write what changed?
summary(linmod)
summary(poimod)
# poimod has different values for al coefficients, this is because if the family.
# Poisson uses logarithmic scaling, this means that the coefficient values are in the logarithmic scale as well.
# The model got more confident in addition (all p-values got smaller).

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
# The p-values got smaller again, thus the model is more confident
# But temperature is now insignificant

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin
# glmer with family = gaussian is the same as the below
mixedlin <-  lmer (coffee ~ sleep + mood + temperature
                   + (1 | subj),
                   data = coffeedat)

## j) Compare the AIC for all four models. Which one has the best fit?
AIC(linmod, poimod, mixedpoi, mixedlin)
# mixedpoi is the best fit, as it has the smallest AIC-value.

## k) And which model is conceptually the appropriate one? Explain why.
# mixedpoi as well:
# 1) The model needs a random effect (and intercept), because a persons response to the factors (sleep, mood, temp.) vary from person to person.
# 2) The Poission distribution is the best fit as discussed in the lecture.

## l) Finally, report on the effects of interest in light of our research hypotheses
##    specified above for the model you chose in k

# nullHyp: students consuming more coffee is not dependent on their sleep
# We can reject this hypothesis since sleep is a significant predictor. 
# Since slope is negative, students consume more coffee when they are tired.

# nullHyp: students consuming more coffee is not dependent on their mood
# We can reject this hypothesis since sleep is a significant predictor. 
# Since slope is negative, students consume more coffee when they are not feeling well

# nullHyp: students consuming more coffee is not dependent on temperature outside:
# We cannot reject this hypothesis as temperature is not significant (p > 0.05)
