### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, December 18. Write the code below the questions.
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number.
## Name: Chaahat Jain
## Matriculation number: 7025099

## Name: Denis Krieger
## Matriculation Number: 7021772

## Name: Eric Minas
## Matriculation Number: 2568884

###########################################################################################



#################################
### Exercise 1: One-way ANOVA
#################################


#install.packages("tidyr")
install.packages("purrr")

library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library(tidyr)
library(purrr)

## This time we will be working with the "anorexia" data frame (package 'MASS')


## This is a data set of a clinical study with 3 conditions: Two groups received an active treatment,
## while the control group did not receive treatment. The study population is anorexia patients
## and the recorded response is the weight before the study and the weight after the study for
## for each patient.


## a) Load the dataset, store it into a variable called "data", and briefly inspect it.
## Feel free to make some plots and calculate some statistics in order to understand
## the data.
#install.packages("MASS")
data <- MASS::anorexia
summary(data)

ggplot(data, aes(Prewt, Postwt, shape = Treat, size = 2)) + geom_point()
# wt = weight
# treat = treatment


## b) In a first step, we will concentrate on the dependent variable Postwt and
##  Treat as the predictor variable (we will assume that the weight before treatment is comparable between groups).
##  Please formulate a sensible research hypothesis.
# There is at least one treatment method that shows significant weight gain in patients.

## c) Build a boxplot of Postwt depending on "Treat". Please use ggplot here and below!
ggplot(data, aes(Treat, Postwt)) + geom_boxplot()

## d) Looking at the boxplots, is there a difference between the weight between the
##  3 treatment groups?
# yes, the mean post-treatment weight of "FT" is significantly higher than the other two (very similar) mean weights.

## e) Now we are ready to perform 1-way ANOVA: please use the function aov() on
## Postwt depending on Treat and assign the result to aov1way
aov1way <- aov(Postwt ~ Treat, data = data)

## Before we interpret the results, let's check the ANOVA assumptions and whether
## they are violated or not and why:

## f) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified
## answer to whether it is violated or not.)
# Slide 32 => no repeated measures
# use duplicate to get a logical vector where the value indicates whether some var is duplicated
# use reduce to find out if any TRUE was in the vector, if not then the assumption holds
reduce(duplicated(data), function(acc, val) {
  acc | val
}) # => False, i.e. there are no duplicates

## g) Normality of residuals (figure out the best way to check this assumption)
# use the shapiro test on the residuals that were calculated
shapiro.test(aov1way$residuals)
# Because 0.05 > p-value (0.5036), we can expect the residuals to be normally distributed.

## h) What do you conclude from your results in g? (give a detailed justified answer to whether it is violated or not)
# I conclude that the residuals of the one-way ANOVA test are normally distrubuted, because the p-value of the shapio test are
# greater than 0.05 (alpha) by a large margin, approx. 10 times as large.

## i) Homogeneity of variance of residuals (figure out the best way to check this assumption)
# The barlett test can be used to test this
bartlett.test(Postwt ~ Treat, data = data)

## j) What do you conclude from i? (give a detailed justified answer to whether it is violated or not)
# Because the p-value (0.01212) is smaller than 0.05, no significant differences in the variances between the groups can be found.

## k) What are your options if you detect that the data violates the ANOVA assumptions?
# The welch test could be run (alternatively the Brown and Forsythe test, which we haven't discussed, can be run as well)

## l) Now we turn to the results. Look at the summary of aov1way
summary(aov1way)

## m) State your conclusion
# The treatment has a significant affect on the patients post-treatment weight (p-value = 0.000444, alpha = 0.05).

## n) Use paired.t.test in order to test which levels of Treat are actually different. Use
## "bonferroni" as the method of p-value adjustment.
pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = "bonferroni")
# The only treatment that has a significant impact is "Cont" (0.0003).

## o) Bonferroni is known to be a conservative method: it preserves the nominal alpha level,
##  but lacks power to detect effects. An alternative is the "holm" method, which also
##  preserves the overall alpha level, but is less conservative. Try this method.
pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = "holm")
# now all three treatments have a significant impact

## p) State your conclusions.
# Using the holm methods shows that all three treatmens have a significant impact (<0.05) on the patients post-treatment weight.


##################################
### Exercise 2: 2-way ANOVA
##################################

## Above, we have only looked at post treatment weights. If the sample is big and
## patients were randomly assigned to treatment groups, this is fine to measure the
## success of the treatment as we can assume that weight before the treatment is
## similar between groups.

## a) Create a graph to see whether prewt is similar between Treat groups.
data_graph <-
  data %>% group_by(Treat) %>% summarise(mean_pre_wt = mean(Prewt),
                                         mean_post_wt = mean(Postwt))

ggplot(data_graph, aes(Treat)) +
  geom_point(aes(y = mean_pre_wt, color = "blue", size = 3)) +
  geom_point(aes(y = mean_post_wt, color = "red", size = 3)) +
  labs(y = "weight in kg", x = "Treatment") +
  scale_color_manual(
    labels = c("pre-treatment weight", "post-treatment weight"),
    values = c("blue", "red")
  )

## b) What is your conclusion?
# Yes, the pre-treatment weight is quite similar to each other (within +/- 2.5 kg of each other).

## Next, we will transform the data set, such that we have one variable combining
## both Prewt and Postwt values and an additional factor coding for Time. This will allow us
## to directly address the change in weight under different treatments in a factorial
## ANOVA.
## Please run the following command.

data_long = anorexia %>% pivot_longer(c(Postwt, Prewt), names_to = "Time", values_to = "Weight") %>%
  mutate(Time = factor(Time, levels = c("Prewt", "Postwt")))
summary(data_long)

## c) Plot boxplots for the distribution of `Weight` for each of the `Time`
## values for data_long. Build 3 plots (each containing 2 boxplots) side by side depending on the
## `Treat` variable.

ggplot(data_long, aes(x = Time, y = Weight), ncol = 3) +
  geom_boxplot(data = data_long %>% filter(Treat == "CBT")) +
  geom_boxplot(data = data_long %>% filter(Treat == "Cont")) +
  geom_boxplot(data = data_long %>% filter(Treat == "FT")) +
  facet_grid( cols = vars(Treat))

## d) Describe the pattern you observe in c)
# both CBT and Cont have barely any change in mean weights for the before/after measurements.
# FT has a significant increase in weight after the treatment.

## e) build a two-way ANOVA including Time and Treat as predictors and their interaction
##  and assign it to aov2way.
aov2way <- aov(Weight ~ Time * Treat, data = data_long)
summary(aov2way)

## f) Report your results in line with the research question.
# The thesis holds, at least one treatment shows significant weight gain after the treatment.
# This can be seen in the p-value for Treat (<0.0005) in combination with the interaction between Time and Treat (p-value < 0.05).
# The interaction shows that at least one treatment had significant impact on the weight. 

## g) In order to evaluate the interaction, we will use pairwise tests again. The
## function, we are going to use here is TukeyHSD. Please call the function on the
##  two-way anova
TukeyHSD(aov2way)

## h) The interaction between Time and Treat produces 15 (!) different comparisons,
##  but not all of them are meaningful to us. Please select three comparisons to report,
##  which conceptually make most sense! Explain your choice!
# I selected the values with the smallest p-adj (p-value adjusted for multiple comparisons)

# NAME                  | p-adj
# Postwt:FT-Postwt:Cont | 0.0000730
# Postwt:FT-Postwt:Cont | 0.0001871
# Postwt:FT-Prewt:CBT   | 0.0012549

#################################################
### Exercise 3: independence assumption
#################################################

## The two-way ANOVA above violates the independence assumption.
##  a) Explain why.
# Because the interaction between Time and Weight was included,
# both of these variables were included twice, violating the assumption.

##  b) Can you think of a way to conduct an ANOVA on this dataset without violating
##  the independence assumption, but taking into account differences between groups
##  prior to treatment?
# Taking the difference between the pre- and post-weight of all participants
# instead of comparing them like we did before would save that problem.
