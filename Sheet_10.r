### Stats with R Exercise sheet 10

###############################################################################
# Week 12: Model Selection, Transformations, Power
###############################################################################

## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, January 22. Write the code below the questions.
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

# The following line of code clears your workspace:
rm(list = ls())


###############################################################################
### Exercise 1 Simplifying random effect structures
###############################################################################

library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for
##  effects of Frequency, the type of the previous Word and the native
##  language of the participant:

m = lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType + Frequency | Subject) +
    (PrevType + NativeLanguage | Word),
  data = lexdec,
  REML = F
)

## a) Unfortunately, the maximal model given above gives a warning that indicates
##    that the model is too complex for the data. In order to get a model that converges
##    without warnings, try to use backwards selection on the random effects.
##    First exclude the random effect that is least contributing to the model fit and so on
##    (this may require multiple steps and a large number of fitted models!).
##    Use model comparison to decide which effects can be excluded.
##    You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1

m1_1 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType  | Subject) +
    (PrevType + NativeLanguage | Word),
  data = lexdec,
  REML = F
)
m1_2 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (Frequency | Subject) +
    (PrevType + NativeLanguage | Word),
  data = lexdec,
  REML = F
)
m1_3 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (1 | Subject) +
    (PrevType + NativeLanguage | Word),
  data = lexdec,
  REML = F
)
m1_4 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType + Frequency | Subject) +
    (PrevType  | Word),
  data = lexdec,
  REML = F
)
m1_5 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType + Frequency | Subject) +
    (NativeLanguage | Word),
  data = lexdec,
  REML = F
)
m1_6 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType + Frequency | Subject) +
    (1 | Word),
  data = lexdec,
  REML = F
)
m1_7 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType + NativeLanguage | Word),
  data = lexdec,
  REML = F
)
m1_8 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType + Frequency | Subject) ,
  data = lexdec,
  REML = F
)
anova(m, m1_1, m1_2, m1_3, m1_4, m1_5, m1_6, m1_7, m1_8)
# selection based on the min of AIC and BIC -> m1_7 has the best fit, additionally m1_3 (--> (1|subj)) is the next-best candidate, supporting the notion that subject is not needed)
#Thus, m1_7 will be used as the base in the second round:
m2_1 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (PrevType  | Word),
  data = lexdec,
  REML = F
)
m2_2 <- lmer(
  RT ~ PrevType + Frequency + NativeLanguage +
    (NativeLanguage | Word),
  data = lexdec,
  REML = F
)
m2_3 <- lmer(RT ~ PrevType + Frequency + NativeLanguage +
               (1 | Word),
             data = lexdec,
             REML = F)

anova(m1_7,m2_2, m2_2, m2_3)
# no significant changes in fit were found, m1_7 is the best (found) optimisation.

## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?
# Yes, the model with the formula: 
# RT ~ PrevType + Frequency + NativeLanguage + (PrevType + NativeLanguage | Word)
# has a better fit and produces no warnings, any furhter changes on it does not significantly produce a better fit (p-values > 0.1)

## c) Another approach is to simplify the random effect structure by excluding correlations.
##    Try out whether this would have solved the problem.
# I already did it initlally, excluding the random slopes, does improve the model fit and remove the error, with the formula:
# RT ~ PrevType + Frequency + NativeLanguage + (1 | Subject) + (PrevType + NativeLanguage | Word)
# But the effect from this change is less than the effect from removing the random effect for subject all together.


###############################################################################
### Exercise 2 Simulations and power
###############################################################################

## In the following we provide you with code for simulations.
## The goal of the exercise is for you to try out the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable
## the results are -- this is necessary because we are sampling the data randomly,
## so it could be that we sometimes get more or less "lucky" draws.

n        <- 2000 # number of observations to be simulated
predA    <- rnorm(n, 80, 20)
predB    <- rnorm(n, 30, 30)
interact <- 0.08 * (predA * predB)
error    <- rnorm(n, 0, 50)

resp     <- 42 + 0.2 * predA - 5.3 * predB + interact + error
d        <- data.frame(predA, predB, resp)


# get the values
resp[1]
mean(predA)
mean(predB)
mean(interact)

## a) Write down what values you would hope for the model to estimate in the ideal case:
##    i)   intercept   =  42 (first value in the resp calculation)
##    ii)  predA       =  80 (mean of rnorm)
##    iii) predB       =  30 (mean of rnorm)
##    iv)  predA:predB =  0.08 * 80 * 30 = 192

## b) Can the model recover the original model structure and estimate correct coefficients
##    for the predictors?


# intercept = 105.2
mean(c(59.6, 152.1, 166.2, 73.9, 74.2))
# predA = 80.48
mean(c(81.6, 81.9, 78.1, 81.6, 79.2))
# predB = 29.6
mean(c(29.9,29.5, 29.4, 31.1, 28.1))
# predA:predB = 189.12
mean(c(196.9, 193.0, 177.4, 204.8, 173.5))

# The model is very close to predA, predB.
# It is further away (but still very close)from the interaction beweteen predA and predB.
# And very far away from the intercept.

## c) What happens if you change the number of subjects? (specify the numbers you tried out!)

# subject = 2000 (x10)
# intercept   = 26.455 (far closer)
mean(c(9.3, 31.93, 102.8, -31.8, 13.4, 33.1))
# predA       = 80 (perfect)
mean(c(80.6, 80.0, 79.8, 79.5, 79.6, 80.5))
# predB       = 30.46667 (slightly closer; almost perfect)
mean(c(30.5, 31.1, 30.7, 30.7, 29.1, 30.7))
# interaction = 195.4667 (closer)
mean(c(197.9, 198.0, 195.2, 197.0, 186.9, 197.8))

# subject = 20 (x (1/10))
intercepts <-c()
predAs <- c()
predBs <- c()
interactions <- c()

for(i in 1:5){
  n        <- 20 # number of observations to be simulated
  predA    <- rnorm(n, 80, 20)
  predB    <- rnorm(n, 30, 30)
  interact <- 0.08 * (predA * predB)
  error    <- rnorm(n, 0, 50)
  
  resp     <- 42 + 0.2 * predA - 5.3 * predB + interact + error
  d        <- data.frame(predA, predB, resp)
  
  
  # get the values
  intercepts <- append(intercepts, resp[1])
  predAs <-append(predAs, mean(predA))
  predBs<-append(predBs, mean(predB))
  interactions<-append(interactions, mean(interact))
}

mean(intercepts)    # 102.27
mean(predAs)        # 77.31
mean(predBs)        # 34.28
mean(interactions)  # 213.48

# subject = 20000 (x 100)
intercepts <-c()
predAs <- c()
predBs <- c()
interactions <- c()

for(i in 1:5){
  n        <- 20000 # number of observations to be simulated
  predA    <- rnorm(n, 80, 20)
  predB    <- rnorm(n, 30, 30)
  interact <- 0.08 * (predA * predB)
  error    <- rnorm(n, 0, 50)
  
  resp     <- 42 + 0.2 * predA - 5.3 * predB + interact + error
  d        <- data.frame(predA, predB, resp)
  
  
  # get the values
  intercepts <- append(intercepts, resp[1])
  predAs <-append(predAs, mean(predA))
  predBs<-append(predBs, mean(predB))
  interactions<-append(interactions, mean(interact))
}

mean(intercepts)    # 97.17 (far off)
mean(predAs)        # 79.95 (almost perfect)
mean(predBs)        # 30.05  (almost perfect)
mean(interactions)  # 192.06  (almost perfect)


## d) What happens if you change the variance of the error term? (specify the numbers you tried out!)

# variance = 10 (x (1/5))
intercepts <-c()
predAs <- c()
predBs <- c()
interactions <- c()

for(i in 1:5){
  n        <- 200 # number of observations to be simulated
  predA    <- rnorm(n, 80, 20)
  predB    <- rnorm(n, 30, 30)
  interact <- 0.08 * (predA * predB)
  error    <- rnorm(n, 0, 10)
  
  resp     <- 42 + 0.2 * predA - 5.3 * predB + interact + error
  d        <- data.frame(predA, predB, resp)
  
  
  # get the values
  intercepts <- append(intercepts, resp[1])
  predAs <-append(predAs, mean(predA))
  predBs<-append(predBs, mean(predB))
  interactions<-append(interactions, mean(interact))
}

mean(intercepts)    # 67.65   (fairly close)
mean(predAs)        # 79.74   (close)
mean(predBs)        # 31.86   (close)
mean(interactions)  # 203.85  (a bit off)

## e) What happens if you change the effect sizes? (specify the numbers you tried out!)
# what are the effect sizes?


## Next we include the above code into a loop to calculate the power of the experiment
## number of simulated data sets
sim = 1000 # number of simulations
n   = 100  # number of participants in each simulation

## results matrix
results = matrix(nrow = sim, ncol = 4)

colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for (i in c(1:sim)) {
  predA    <- rnorm(n, 80, 20)
  predB    <- rnorm(n, 30, 30)
  interact <- 0.08 * (predA * predB)
  error    <- rnorm(n, 0, 50)
  resp     <- 42 + 0.2 * predA - 5.3 * predB + interact + error
  d        <- data.frame(predA, predB, resp)
  m1       <- lm(resp ~ predA * predB, data = d)
  
  ## store the resulting p-values in the results matrix
  results[i, ] = summary(m1)$coefficients[, 4]
}


## f) We use the above code and the results matrix to calculate power. Recall that the power is
##    the probability of rejecting the Null hypothesis, given a specific effect size.
##    We can approximate this by calculating the proportion of simulated datasets,
##    where the effect comes out significant, i.e. below 0.05.
##    Calculate the power based on the simulations for all three effects of interest
##    (i.e., predA, predB and the interaction) individually.

# use this for F, H
alpha <- 0.05
# use this for G
alpha <- 0.01

# power predA as percent = 3.08 %
powerPredA <- (length(results[results[, "predA"] < alpha]) / n) / 100
# power predB as percent = 40 %
powerPredB <- (length(results[results[, "predB"] < alpha]) / n) / 100
# interaction = 40 %
powerInteraction <- (length(results[results[, "interaction"] < alpha]) / n) / 100

## g) How does power change when you decrease your alpha level to 0.01?
# predB and interaction stay at 4%, while the power for predA falls to less than 1 %.

## h) How does power change, when you decrease the number of participants in each simulated data
##    set to 50? (alpha-level = 0.05)
# the power of both predB and the interaction decrease massively (40 % -> 2%) and predA falls even further than with alpha = 0.01
