### Stats with R Exercise sheet 4

##########################
#Week 5: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and
## submit by 23:55 on Sunday, December 4. Write the code below the questions.
## If you need to provide a written answer, comment this out using a hashtag (#).
## Submit your homework via cms

## Please write below your (and your teammates') name, matriculation number.
## Name: Chaahat Jain
## Matriculation number: 7025099

## Name: Denis Krieger
## Matriculation Number: 7021772

## Name: Eric Minas
## Matriculation Number: 2568884

###########################################################################################
###########################################################################################

#####################################################
### 1. Restructuring, plotting, and first t test
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
##
## Variables we will be interested in include
## Subject (code for a participant)
## Complex (whether the word is a compound (e.g.blackberry) or not (e.g.cherry))
## RT (log reaction time)
## Sex (of the participant)
## Frequency (log-transformed frequency of the word in the CELEX corpus per million words)


## a. Create the dataset lex, which is a copy of lexdec, but only includes the columns
##  indicated above
lex <- lexdec %>% select(Subject, Complex, RT, Sex, Frequency)

## Say you are interested in the influence of the frequency of a word on lexical decision time.
## In particular, you want to compare high frequency words to low frequency words using a t-test.

## b. Why is this not possible with the data as it is?
# The structure is not separated into high or low frequency words.

## Run the following line to prepare the dataset for later steps:
lex = lex %>% mutate(Freq = as.factor(ifelse(Frequency > 4.75, "high", "low")))

## c. Look at the new variable. Describe how freque was transformed and why.

# The frequency was transformed from a number into a factor with two levels: "low" and "high".
# The transformation was necessary, because we want to test only the characteristic captured by the levels, not the exact value.

## Before we start testing, we want to get an impression of the data and create a barplot of
## the mean by Freq, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI:
## (just execute the next line, as you will need the function in 2.)
se = function(x) {
  sd(x) / sqrt(length(x))
}

## d. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by Freq and get the mean as well as the
##  se of RT. Store the result to summaryByFreq
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function
summaryByFreq <-
  lex %>% group_by(Freq) %>%
  summarise(mean = mean(RT), se = se(RT))

## e. Describe the resulting data set (summaryByPrevType) in your own words
# The dataset shows the mean and standard-error of the two levels of frequency.

## f. Now use summaryByFreq to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)
ggplot(summaryByFreq, aes(x = Freq, y = mean)) +
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)),
                width = 0.2,
                position = position_dodge(0.9))

## g. The barplot always starts at zero, which makes the portion of the graph, we are most
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes
ggplot(summaryByFreq, aes(x = Freq, y = mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)),
                width = 0.2,
                position = position_dodge(0.9))

## h. Gauging from the plot, does it look like there's an important difference in mean RT
##  for low and high frequency words?
# yes, low frequency words are on average reacted to 0.1 seconds slower.

## i. Let's go back to the original data frame "lex".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT for low vs high frequency nouns.
##  Why can't you compute a t-test on the data as they are now?
##  Hint: Which assumption is violated?
# We need the data to come from a normal distribution, but we don't know whether thats the case.

## j. We need to restructure the data to only one observation (average RT) per subject
##  and low/high condition (Freq). We will again use group_by and summarize, but
##  this time we have to group by Subject and Freq, while we only need the mean to be
##  stored, not the se. Assign the result to bySubj
bySubj <-
  lex %>% group_by(Subject, Freq) %>% summarise(mean = mean(RT))

## k. Create histograms of the RT data in bySubj depending on the frequency category
##  and display them side by side. Set the binwidth to 0.08
ggplot(bySubj, aes(x = mean, fill = Freq)) +
  geom_histogram(position = position_dodge(), binwidth = 0.08)

## l. Display the same data in density plots.
ggplot(bySubj, aes(x = mean, fill = Freq)) +
  geom_density()

## m. Based on the histograms and the density plots - are these data likely coming
## from a normal distribution?
# No, the density plot shows that the values are concentrated on the left side of the graph (right skewed).

## n. Create boxplots of the mean RT in bySubj by Freq
ggplot(bySubj, aes(x = Freq, y = mean)) +
  geom_boxplot()

## o. We want to compute a t-test to compare the mean RT between lexical decisions on low
##  frequency words vs high frequency words using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?

## p. Compute the t-test you specified above

## q. What does the output tell you? What conclusions do you draw?

## r. Compute the effect size using Cohen's D.

## s.  Which effect size do we get? How do you interpret this result?

## t. Why would you report the effect size in addition to the p-value?

#####################################################
### 2. T-tests on wide format
#####################################################


##  In exercise 1, we have worked on long-format data, where each line represents one observation.
##  In the context of t-tests, we may also encounter data sets in a wide format
##  (this is the format we have been using in class examples.)
##  Let's look at the same dataset as in 1., but at a different variable, namely the morphological
##  complexity (Complex) of the target word.

## a. Again, summarize the dataset to obtain the mean RT by "Subject" and "Complex" and transform
##  the dataset to a wide format.
##  In addition to group_by() and summarize(), you will need the function spread().
##  Assign the result to wide
wide <-
  lex %>% group_by(Subject, Complex) %>% summarise(mean = mean(RT))

## b. Compute a t-test on the wide format data - note that for wide-format
##  data you need to use a different syntax inside t.test()
t.test(wide$mean, wide$Complex)

## c. What do you conclude from this?


#####################################################
### 3. Another T-test
#####################################################


## a. Now let's look at yet another question, namely whether the Sex of the participant
##  influences their reaction time. Check out the variable Sex. Can you use a t-test to pursue this
##  question and which type of t-test would you use?

# - Sex is independent
# - sd is unknown
# - sd for either group unknown
# => Welch Test should be used

## b. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to Sex and assign it to bySubjSex
## Perform the t-test you decided for.
bySubjSex <- lex %>% group_by(Sex) %>% summarise(mean = mean(RT))

independentSamplesTTest(formula = mean ~ Sex,
                        data = bySubjSex,
                        var.equal = TRUE)
## c. What do you conclude?
# don't know why it doesnt work, it should

## d. Choose an appropriate plot to visualize the result
ggplot(bySubjSex, aes(x = Sex, y = mean, group = 1)) +
  geom_line() +
  geom_point()

#############################################
### 4. T-Tests for different sample sizes
#############################################

## In this exercise we will again use simulation to explore the independent samples t-test
## with different samples.
## We will take a similar example as discussed in the lecture. A class has two tutors, and we want
## to find out which tutor is better by comparing the performance of the students in the final
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(9273)
## a. Generate 10 random samples from a normal distribution with mean 20 and sd 8 and save it in a variable
##  called "tutor1_grades"
tutor1_grades <- rnorm(n = 10, mean = 20, sd = 8)

## b. Now we generate our second sample of size 10 ("tutor2_grades), this time for tutor 2
##  and with mean 28 and sd 10
tutor2_grades <- rnorm(n = 10, mean = 28, sd = 10)

## c. Combine the two samples and store the result into one vector called "score" (it should
##    first show all scores from tutor1 followed by the scores of tutor2)
score <- c(tutor1_grades, tutor2_grades)

## d. Create a vector called tutor indicating which tutor the score belongs to: it should show
##   "tutor1" 10 times followed by "tutor2" 10 times
tutor <- rep(c("tutor1", "tutor2"), each = 10)

## e. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.
data_frame <- data.frame(tutor = as.factor(tutor), score = score)

## f. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed
###  in the lecture.
independentSamplesTTest(formula = score ~ tutor,
                        data = data_frame,
                        var.equal = TRUE)

# The mean score for tutor-1's class is 15.3 (std. dev. = 11.3), whereas the mean in tutor-2's class
# is 28.3 (std. dev. = 11.4). A Student's independent samples t-test shows that this 13 points difference was not significant
# (t(18) = -2.6, p < 0.05, CI_95 = [-23,7, -2,3], d = 1.141),
# suggesting that no genuine difference in scored points has occurred.

## Time to play around!

## g. Repeat the whole experiment you performed above with different sample size
##  (the number of samples drawn from each tutor group). How big does your sample need to be in order
##  for the t test to be significant when keeping mean and sd constant?
## make sure to set the seed again before you run your code to be able to reproduce results
size <- 30
set.seed(9273)
tutor1_grades <- rnorm(n = size, mean = 20, sd = 8)
tutor2_grades <- rnorm(n = size, mean = 28, sd = 10)
tutor <- rep(c("tutor1", "tutor2"), each = size)
data_frame <- data.frame(tutor = as.factor(tutor), score = score)
independentSamplesTTest(formula = score ~ tutor,
                        data = data_frame,
                        var.equal = TRUE)
## h.	repeat the whole experiment you performed in a-f with different means.
##   What do you find? When is the test more likely to come out significant?

## i.	Now, vary the standard deviation, keeping means and sample size constant!
##   What do you find? When is the test more likely to come out significant?