### Stats with R Exercise sheet 8

##############################################################################
# Week 10: Linear Mixed Effects Models
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, January 8. Write the code below the questions.
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via cms

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

library(languageR)
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


###############################################################################
### 1. Linear mixed model for chicken growth
###############################################################################

## a) We will first look at the dataset ChickWeight, which is already
##    loaded in base R. Check out the help page of the data set to understand
##    how the data was collected and look at the summary.
help(ChickWeight)
summary(ChickWeight)

## b) Let's plot the data.
##    1) Group the data by Diet and Time. Use a function summarySE()
##       from Rmisc library to get the mean and se of weight.
##       Assign resulting dataset to aggData.
library(Rmisc)
? summarySE()
aggData <-
  summarySE(
    data = ChickWeight,
    measurevar = "weight",
    groupvars = c("Diet", "Time")
  )

##    2) Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet.
##       Also add errorbars (mean+/-1.96*se)
ggplot(aggData, aes(Time, weight, color = Diet)) +
  geom_line() +
  geom_errorbar(ymin = aggData$weight - 1.96 * aggData$se,
                ymax = aggData$weight + 1.96 * aggData$se)

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data
ggplot(ChickWeight, aes(Time, weight, color = Chick)) +
  geom_line() +
  facet_wrap(ChickWeight$Diet)

## d) What do you observe, looking at c?
# Diet 1 has a lot of outliers,
# Diet 2 is more concentrated with only 2 extreme outliers
# Diet 3 is more concentrated than diet 2
# Diet 4 is the most concentrated diet.
# Overall Diet 3 seems to be the best perfoming (highest lowpoint and highest high-point)

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##    looking for an interaction between time after birth and the diet type. Before running the model,
##    specify:

##    1) What fixed effect(s) do you enter into the model?
# time, diet and their interaction.
# We choose time, because it changes predictably for each chick.
# We choose diet since it is constant for each chick,
# The interaction is needed, because we want to check whether the time and diet influence the weight. If that is the case then diet works.

##    2) what random effect(s) should be included to account for the repeated measures structure of the data?
# intercept for each chick, because we have different starting points and we need to make it comparable across each chick.

##    3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?
# Random slope is time because for every chick, we are keeping track of values at different points of time.

## f) Run the model you specified in e) using lmer() and assign it to chickmod
chickmod <-
  lmer(weight ~  Time * Diet +
         (1 + Time | Chick), data = ChickWeight)

## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull <-
  lmer(weight ~ Time + Diet +
         (1 + Time | Chick), data = ChickWeight)

## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chickmod, chicknull)

## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis
# p-value << 0.005, I can therefore expect that the likelyhood ratio is significant and chickmod and chicknull contain equal data

## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod, condVar = TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

# The slopes (vertical lines) seem quite equal for all chickens and only the intercepts are different.
# If that is actually the case, then the diet has no significant effect on the weight of the chicken after the observed time.

#####################################################
### 2. Random effect structures
#####################################################

## a) Let's return to the lexdec data set from Sheet 4 and suppose, we want to look
##    at effects of the word type of the previously presented word (each subject saw a
##    different randomized sequence) and effects of the complexity of the word itself, while
##    taking into account the dependence between data points collected on the same word and from the same subject.
##    Which of the following models has a maximal random effect structure given the experimental design?
##    Motivate your choice.

m1 = lmer(RT ~ PrevType + Complex +
            (PrevType | Subject) +
            (Complex | Word),
          lexdec)
m2 = lmer(RT ~ PrevType + Complex +
            (PrevType + Complex | Subject) +
            (PrevType | Word),
          lexdec)
m3 = lmer(RT ~ PrevType + Complex +
            (PrevType + Complex | Subject) +
            (PrevType + Complex | Word),
          lexdec)
m4 = lmer(RT ~ PrevType + Complex +
            (Complex | Subject) +
            (PrevType | Word),
          lexdec)
m5 = lmer(RT ~ PrevType + Complex +
            (PrevType + Complex | Subject) +
            (1 | Word),
          lexdec)

# m5 is the best approach:
# it takes both the subject and the item into account
# it takes the (random-) effect of the previous word type and complexity for the subject (=participant) into account.
# it takes the (random-) effect of the word into account.
# there is no need to add prevType/complexity for the word, as the word itself does not change due to those factors.
# Therefore the intersections suffices.

## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##    to their final math grade in school. The summer school course has 200 participants, coming from 8 different
##    partner Universities from all over Germany. These 200 participants were randomly split into 10 tutorial groups,
##    where each tutorial was held by a different tutor.
##    Given the design of your study, what random effects should you add to the following model:
##    NOTE: We accept only the answers with explanations!

## lmer(advancedalgebrascore ~ mathGrade, someData)

# We should include the tutor as a random effect, as the effectiveness of the tutors teaching is depending on the tutors skill,
# as well as the participants (i.e. students that only look at their phone will have worse grades than those that pay attention)
# If the data contains the german state the student got thier final math grade in, we can also take that into account,
# because not all states teach the same solving techniques /  talk about the same things.