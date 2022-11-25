### Stats with R Exercise sheet 3

#################################################
#Tests for Categorical Data and cleaning data
#################################################

## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, November 27th. Write the code below the questions.
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via cms

## Make sure you answered ALL subquestions and that your code actually runs before submitting!


## Please write below your (and your teammates) name, matriculation number.
## Name: Eric Minas
## Matriculation number: 2568884
## Name:
## Matriculation number:
## Name:
## Matriculation number:

## Only 1 member needs to submit!

#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia22.csv from cms
## The made-up dataset insomnia contains data of a survey on 60 students.
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

## a. Load the libraries stringr, dplyr, tidyr and forcats
#install.packages("stringr")
library(stringr)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("forcats")
library(forcats)

## b. read in the data
setwd("/home/l1/Desktop/statR_group_79/")
insomnia <- read.csv("insomnia22.csv", header = TRUE)

## c. get a summary of the dataset
summary(insomnia)

## d. the variable sleepProblem should be a numerical variable and have 0 for no Problem
##    and 1 for sleep problems.
##    Make sure that this is the case
# num of entires that have neither 1 or 0 = 3
insomnia %>%
  filter(sleepProblem != 0 & sleepProblem != 1)

insomnia$sleepProblem[insomnia$sleepProblem == "yes"] <- 1
insomnia$sleepProblem[insomnia$sleepProblem == "no"] <- 0
insomnia$sleepProblem[insomnia$sleepProblem == 11] <- 1

## e. how many students encounter sleep problems?
# -> 25 students have sleep problems
insomnia %>% filter(sleepProblem == 1) %>% count(sleepProblem)

## f. how many different drinks do students name? (transform the variable into a
## factor first)
insomnia$drink <- as.factor(insomnia$drink)

# 3 distinct levels, 10 levels overall
levels(insomnia$drink)

## g. collapse factor levels which were spelled wrong. Make sure you first handle
## case and whitespace incongruencies, before you fix individual misspellings)

# trim + case cleanup
insomnia <- insomnia %>%
  mutate(drink = str_trim(drink)) %>%
  mutate(drink = str_to_lower(drink))

# collapsing
insomnia <- insomnia %>%
  mutate(drink = fct_collapse(drink, coffee = c("cofee", "coffe", "koffee"))) %>%
  mutate(drink = fct_collapse(drink, tea = c("tee")))

# checking
insomnia$drink <- as.factor(insomnia$drink)
levels(insomnia$drink)


## You realize that most students had multiple exams in the week from Feb 22 to
## Feb 26. As students had to learn a lot and were possibly worried, they might
## misjudge or exaggerate their sleep problems as occurring "regularly"
## We want to exclude all data that was collected between and including Feb 15
## and Feb 26!
## h.  First show how many data points will be concerned, you need to transform
##     the date column to a Date object first!
insomnia$date <- as.Date(insomnia$date)

affected <- insomnia %>%
  filter(date >= "2021-02-15" & date <= "2021-02-26")

count(affected) # -> 10 students
## i. Now filter out this part of the data and assign the result to clean
clean <- insomnia %>%
  filter(date < "2021-02-15" | date > "2021-02-26")

####################################################################
### Exercise 2: Chi-squared test
####################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

## a. formulate in plain English what the Null hypothesis is in this context
# The preferred drink does not influence sleep problems.

## b. conduct a chisquare test to test this hypothesis using the function chisq.test()
##    and assign the result to chi
chi <- chisq.test(clean$drink, clean$sleepProblem)
chi

## c. the last call produced a warning. To understand why this warning arises, look
##    at observed and expected frequencies of chi
table(clean$drink, clean$sleepProblem, dnn= c("drinks", "sleepProblems"))

## d. What are the expected frequencies? Do we need to look at expected or
##    observed frequencies?
# We expect that the sleep problems are randomly distributed over the drink choices:
# - coffee -> expected: 53/3 = 17.6 ~ 18
# - tea -> expected: 53/3 = 17.6 ~ 18
# - water -> expected: 53/3 = 17.6 ~ 17

## e. a possible solution is to sample more participants. Given that the smallest
##    admissible value is 5, from which group(s) in terms of preferred drinks do
##    we have to sample more?
# tea has the smallest total number of samples (13), while water has the smallest sample for "no sleep problems".
# Thus water should be sampled first, so that the threshold of five can be superceeded in each category.

## f. Assume we don't have the possibility to sample more students. Which test do
##    you have to run instead? How does it work roughly? Perform a suitable test
# we should run Fisher's exact test, because the table we have is 3x2 (too big for X^2) and the variables are independent.
# We create all tables that can be produced by changing the values in rows and columns, with the constraint that all totals must stay the same
# Then we determine the sum of probabilities of the tables which were more extreme than the original table
# if the sum is less than 0.05, we reject the null-hypothesis
fisher.test(table(clean$drink, clean$sleepProblem, dnn= c("drinks", "sleepProblems")))

## g. Lastly, what is the conclusion of your test? What have you learned and what
##    have you not learned?
# Because the result (0.028) is smaller than 0.05, we reject the null-hypothesis. 
# This means that we expect that the preferred drink influences sleep problems.
# We have yet to find out whether this holds for all drinks and how likely a certain drink is to influence sleep problems. 

#########################################
## Exercise 3. Binomial distribution
#########################################
##  In a board game, you have to roll a fair die. You will get a point,
##  each time the number is higher than 4. You roll 20 times

## a) What is the chance in a single roll of earning a point?
# The probality is 3 * (1/6) = 0.5 (50%)

## b) Please calculate the probability of getting exactly 3 points.
##    Calculate this using the dbinom() function.
dbinom(3,size = 20,prob=0.5)

## c) Next please calculate the probability of getting less than 6 points
sum(dbinom(seq(0,5,1),size = 20,prob=0.5))

## d) What is the difference between density function and distribution function?
# The density functions shows how many tests have the same values 
#(i.e. the likeliness of the value but without explicitly calculating it)
# The distribution funcions shows how the values are distributed over the spectrum
# of possible values, but without having explicit data to back it up.

#########################################
## Exercise 4
#########################################

##  In order to better understand the relationship between sleeping problems and
##  consumed drinks, we set up a better controlled experiment:
##  For two weeks, students are asked to drink mostly coffee and are then asked
##  whether they encountered sleep problems. For another two weeks, the same students
##  are asked to switch to tea and then again asked for sleeping problems.

## a) Can you use the ChiSquare test in this situation? Explain and motivate
##  your answer
# No the ChiSquare test would not be appropriate, there are multiple reasons:
# 1) The values are not independent, because the students are testing the changes
# on their sleep back-to-back, the effects from coffee can still influence them during the time they are testing tea.
# 2) The results could either be held in a 2x2 table 
# (coffee/tea vs 0->1, 1->0 to the previous sleeping problems) or 2x4 (coffee/tea vs sleep problems 0/1 before and after), 

## b) Is there an alternative test you could use? Why would this be appropriate?
# McNemar's Test would be more appropriate, because the values are depended (see 4a-1) and the values are held in either a 2x2 or 2x4 table, which excludes ChiSquare.
