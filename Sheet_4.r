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
## Name: Eric Minas
## Matriculation number: 2568884
## Name: Dennis Krieger
## Matriculation number: 7021772

###########################################################################################
###########################################################################################

#####################################################
### 1. Restructuring, plotting, and first t test
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
# install.packages("ggplot2")
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
lex <- subset(lexdec, TRUE, c("Subject", "Complex", "RT", "Sex", "Frequency"))
## Say you are interested in the influence of the frequency of a word on lexical decision time.
## In particular, you want to compare high frequency words to low frequency words using a t-test.

## b. Why is this not possible with the data as it is?
# At the moment, Frequency is a number (continous variable).
# As such, we do not know what exactly corresponds to a high frequency and which to a low one. 
# t-tests work best when you are comparing two groups.

## Run the following line to prepare the dataset for later steps:
lex = lex %>% mutate(Freq = as.factor(ifelse(Frequency > 4.75, "high", "low")))

## c. Look at the new variable. Describe how frequency was transformed and why.
# We have manually specified a frequency threshold of 4.75. 
# Now, Freq consists of two groups "high" and "low" which enables comparisons.

## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by Freq, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI:
## (just execute the next line, as you will need the function in 2.)
se = function(x){sd(x)/sqrt(length(x))}

## d. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by Freq and get the mean as well as the
##  se of RT. Store the result to summaryByFreq
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function
summaryByFreq <- lex %>% group_by(Freq) %>% summarize(m = mean(RT), serr = se(RT))

## e. Describe the resulting data set (summaryByPrevType) in your own words
# summaryByFreq gives the mean and standard error of RT for the two groups of words 
# (separated by high and low frequency).

## f. Now use summaryByFreq to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)
ggplot(summaryByFreq, aes(x = Freq)) + geom_bar() + 
  geom_errorbar(data = summaryByFreq, aes(ymin = m - 1.96*serr, ymax = m + 1.96*serr))

## g. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes
ggplot(summaryByFreq, aes(x = Freq, y = m)) + geom_line(aes(group = 1)) + geom_point() + 
  geom_errorbar(data = summaryByFreq, aes(ymin = m - 1.96*serr, ymax = m + 1.96*serr))
## h. Gauging from the plot, does it look like there's an important difference in mean RT 
##  for low and high frequency words?
# Yes, there is a difference of almost 0.1 RT which would be significant since this is log calculations

## i. Let's go back to the original data frame "lex".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT for low vs high frequency nouns.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?

# We violate the assumption of independence due to multiple reaction time measures for each Subject.

## j. We need to restructure the data to only one observation (average RT) per subject 
##  and low/high condition (Freq). We will again use group_by and summarize, but
##  this time we have to group by Subject and Freq, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj
bySubj <- lex %>% group_by(Subject, Freq) %>% summarize(m = mean(RT))
## k. Create histograms of the RT data in bySubj depending on the frequency category 
##  and display them side by side. Set the binwidth to 0.08
ggplot(bySubj, aes(x = m, fill = Freq)) + geom_histogram(binwidth = 0.08, alpha = 0.5, position = "dodge")

## l. Display the same data in density plots. 
ggplot(bySubj, aes(x = m, fill = Freq)) + geom_density(alpha = 0.5)

## m. Based on the histograms and the density plots - are these data likely coming
## from a normal distribution?
# No, the data is likely coming from data with a right skew

## n. Create boxplots of the mean RT in bySubj by Freq
ggplot(bySubj, aes(x = Freq, y = m)) + geom_boxplot()

## o. We want to compute a t-test to compare the mean RT between lexical decisions on low
##  frequency words vs high frequency words using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?
# We use independent sample t-test since we have one value per participant.

## p. Compute the t-test you specified above
hd <- subset(bySubj, Freq == "high")
ld <- subset(bySubj, Freq == "low")
t.test(ld$m, hd$m)
## q. What does the output tell you? What conclusions do you draw?
# Since our p-value is 0.02 (less than 0.05), our results are statistically significant. 
# So, we can reject the null hypothesis. 
# Thus, we can say that the difference between the mean reaction time of low frequency compared to 
# high frequency is statistically significant. (t (37.6) = 2.4, p = 0.02)

## r. Compute the effect size using Cohen's D. 
cohensD(ld$m, hd$m)
## s.  Which effect size do we get? How do you interpret this result?
# The effect size is 0.76. So, there is a strong relationship between frequency and mean RT.

## t. Why would you report the effect size in addition to the p-value?
# The effect size provides the magnitude of the relationship. 
# The p-value establishes there is a relationship.
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
wide <- lex %>% group_by(Subject, Complex) %>% summarize(m = mean(RT)) %>% spread(key = Complex, value = m)
## b. Compute a t-test on the wide format data - note that for wide-format 
##  data you need to use a different syntax inside t.test()
t.test(wide$complex, wide$simplex)
## c. What do you conclude from this?
# Since p is higher than 0.05, we cannot reject the null hypothesis.
# So, it is likely there is no significant difference in RT mean between complex and simplex words.

#####################################################
### 3. Another T-test
#####################################################


## a. Now let's look at yet another question, namely whether the Sex of the participant 
##  influences their reaction time. Check out the variable Sex. Can you use a t-test to pursue this 
##  question and which type of t-test would you use? 
# To check if Sex influences reaction times, we will use independent t-test.  

## b. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to Sex and assign it to bySubjSex
## Perform the t-test you decided for.
bySubjSex <- lex %>% group_by(Subject, Sex) %>% summarize(m = mean(RT))
t.test(m ~ Sex, bySubjSex)
## c. What do you conclude?
# Null hypothesis cannot be rejected. Difference in Mean reaction times of males and females is not statistically significant.

## d. Choose an appropriate plot to visualize the result
ggplot(bySubjSex, aes(x = Sex, y = m)) + geom_boxplot()
#############################################
### 4. T-Tests for different sample sizes
#############################################

## In this exercise we will again use simulation to explore the independent samples t-test 
## with different samples. 
## We will take a similar example as discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

expe = function(sample, mean1, sdev1, mean2, sdev2){
  set.seed(9273)
  tutor1_grades <- rnorm(sample, mean1, sdev1)
  
  ## b. Now we generate our second sample of size 10 ("tutor2_grades), this time for tutor 2 
  ##  and with mean 28 and sd 10
  tutor2_grades <- rnorm(sample, mean2, sdev2)
  ## c. Combine the two samples and store the result into one vector called "score" (it should 
  ##    first show all scores from tutor1 followed by the scores of tutor2)
  score <- append(tutor1_grades, tutor2_grades)
  ## d. Create a vector called tutor indicating which tutor the score belongs to: it should show 
  ##   "tutor1" 10 times followed by "tutor2" 10 times
  tutor = c(rep("tutor1",sample),rep("tutor2",sample))
  ## e. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.
  data_frame <- as.data.frame(cbind(tutor, score))
  data_frame$score <- as.numeric(data_frame$score)
  str(data_frame)
  ## f. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed 
  ###  in the lecture. 
  independentSamplesTTest(score ~ tutor, data = data_frame)
  }
set.seed(9273)
## a. Generate 10 random samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"
tutor1_grades <- rnorm(10, 20, 8)

## b. Now we generate our second sample of size 10 ("tutor2_grades), this time for tutor 2 
##  and with mean 28 and sd 10
tutor2_grades <- rnorm(10, 28, 10)
## c. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)
score <- append(tutor1_grades, tutor2_grades)
## d. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 10 times followed by "tutor2" 10 times
tutor = c(rep("tutor1",10),rep("tutor2",10))
## e. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.
data_frame <- as.data.frame(cbind(tutor, score))
data_frame$score <- as.numeric(data_frame$score)
str(data_frame)
## f. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed 
###  in the lecture. 
independentSamplesTTest(score ~ tutor, data = data_frame)

expe(10, 20, 8, 28, 10)
# p-value of 0.5 means our results are not statistically significant. Null hypothesis cannot be rejected.
# There is no significant difference in mean scores between the tutors.
## Time to play around!

## g. Repeat the whole experiment you performed above with different sample size 
##  (the number of samples drawn from each tutor group). How big does your sample need to be in order
##  for the t test to be significant when keeping mean and sd constant?
## make sure to set the seed again before you run your code to be able to reproduce results
expe(25, 20, 8, 28, 10)
expe(30, 20, 8, 28, 10) # Need 30 samples to get a p-value of 0.011 which is statistically significant.

## h.	repeat the whole experiment you performed in a-f with different means.
##   What do you find? When is the test more likely to come out significant?
expe(25, 20, 8, 30, 10)
expe(25, 20, 8, 40, 10)
# The larger the difference in means is, the smaller the p-value gets.

## i.	Now, vary the standard deviation, keeping means and sample size constant!
##   What do you find? When is the test more likely to come out significant?
expe(25, 20, 5, 20, 10)
expe(25, 20, 5, 20, 50)
expe(25, 20, 2, 20, 30)
# The t-test compares the difference in means and not the spread of the data. 
# As such, despite varying the standard deviation, as long as the mean around which the data is generated is constant, 
# the p-value does not change by much. 