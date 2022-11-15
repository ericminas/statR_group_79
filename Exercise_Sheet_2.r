### Stats with R Exercise sheet 2

###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 20th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Remember to answer each subquestion and test your code, before you submit.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Chaahat Jain
## Matriculation number: 7025099

## Only 1 member needs to submit! 

###############################################################
### Exercise 1: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with 
## different sizes.

## a) We will not use data from a normal distribution, but work with the poisson distribution, which is 
## often used for count data. We will use the dataset discoveries, please find out what it is about

## please run the following line to convert the data into numeric
discoveries = as.numeric(discoveries)

## b) Take a look at the dataset using the table() function and histogram and boxplot. 
table(discoveries)
hist(discoveries)
boxplot(discoveries)
## c) Compare mean and median of the dataset
avg <- mean(discoveries)
med <- median(discoveries)
avg > med
## d) Create a sample of a normal distribution of equal length and with the same mean and 
##   standard deviation as the observed sample, assign it to norm_disc

norm_disc <- rnorm(discoveries, avg, sd(discoveries))
## e) Make a histogram of norm_disc and compare this to the histogram of the original dataset from b
hist(norm_disc)

## f) Describe the differences observed in e)
# The histogram in the original dataset is right skewed. The histogram created for the new dataset is normal. 

## g) Now, we are going to draw a smaller sample from discoveries.
### Use the function sample() to create a sample of four instances from discoveries
### assign it to sample4
sample4 <- sample(discoveries, 4)

## h) draw another 2 samples of 4 called sample4b and sample4c
sample4b <- sample(discoveries, 4)
sample4c <- sample(discoveries, 4)
## i) calculate the mean for each of the three samples and store them in the vector means4
means4 <- c(mean(sample4), mean(sample4b), mean(sample4c))

## j) Are the values different? Why?
# Yes they are different.
# Out of 100 values, picking 4 values at random does not guarantee these values will be close to each other.
# This is a result of the Central Limit Theorem. 

## k)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 4. However, we don't want to repeat 
###   question h and i 1000 times. Use a for loop to draw 1000 samples of size 4
###   and store the mean of each sample in the vector means4.
means4 <- numeric(1000)
for (i in 1:1000) {
  samp <- sample(discoveries, 4)
  means4[i] <- mean(samp)
}
## l) Repeat the for-loop in question k, but use a sample size of 40. 
##    Assign this to 'means40' instead of 'means4'.
means40 <- numeric(1000)
for (i in 1:1000) {
  samp <- sample(discoveries, 40)
  means40[i] <- mean(samp)
}
## m) Explain in your own words what 'means4' and 'means40' now contain. 
##    How do they differ?
# We randomly take 4 values out of our dataset and take their mean.
# means4 contains the means of these samples if the sampling is repeated 1000 times.
# means40 contains the means of similar samples (but with size 40 rather than 4) repeated 1000 times.
max(means4) - min(means4) > max(means40) - min(means40)
var(means4) > var(means40)
# It can be observed that means4 has a higher range and variance in values compared to means40.


## n) Draw histograms of means4 and means40. Describe in what way they differ
hist(means4)
hist(means40)
# The x axis is smaller for means40 (ranging from 2-4) whereas the x axis for means4 ranges from (0 - 8)
# There is a slight skew in means4 but not in means40

## o) Why do you observe a skew for means4, but not for means40?
# Since our sample size is not sufficiently large, the central limit theorem does not apply. 
# Thus the sample means4 has a slight skew. 
# A sample size of 40 is sufficiently large, thus not facing the same problem.

###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.


## a) What does a confidence interval mean from the perspective of experiment replication?
# If a replicated experiment finds the population parameter value to be outside the confidence interval range,
# then these results are sgnificant as this should be extremely unlikely. 

## b) please install and load packages sciplot and lsr
# install.packages("sciplot")
# install.packages("lsr")
library(sciplot)
library(lsr)

## c) calculate 95% Confidence Intervals for discoveries, sample4 and sample4c. You can
##    use the function ciMean()
ciMean(discoveries)
ciMean(sample4)
ciMean(sample4c)
## d) Why are these intervals so different?
# Since the sample size is so small, the values picked do not accurately reflect the entire population. 

## e) Is the true mean contained in each interval?
# Theoretically, the intervals claim the true mean is contained in each of them with a probability of 95%
# Practically, the below code verifies this
ciMean(discoveries)[1] < mean(discoveries) && mean(discoveries) < ciMean(discoveries)[2]
ciMean(sample4)[1] < mean(discoveries) && mean(discoveries) < ciMean(sample4)[2]
ciMean(sample4c)[1] < mean(discoveries) && mean(discoveries) < ciMean(sample4c)[2]
# Yes, the true mean is contained in each interval 

## f) In the following, we will check whether the CI behaves as expected.
##   What is the true mean in our example?
trueMean <- mean(discoveries)
## g) Change your for loop from above (subquestion 1l) to calculate the confidence interval 
##  instead of the mean for 1000 samples of size 15. Then check whether the confidence 
##  interval contains the true mean and save the result in a vector called TrueMeanContained.
##  Hint: You will need to compare the mean to the lower and the upper bound of the 
##  confidence interval
## ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound

TrueMeanContained <- numeric(1000)
for (i in 1:1000) {
  samp <- sample(discoveries, 40)
  ciSamp <- ciMean(samp)
  TrueMeanContained[i] <- ciSamp[1] < trueMean && trueMean < ciSamp[2]
}

## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
##   the mean 95% of the time. Does it?
sum(TrueMeanContained)/length(TrueMeanContained)
# Yes, it contains the true mean 98% of the time

## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
## the dataset chickwts, which contains weight of chickens after being administered different kinds of 
## food for 6 weeks.
## Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor
bargraph.CI(x.factor = chickwts$feed, response = chickwts$weight, xlab = "Feed", ylab = "Weight")

## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
##  Hint: Look into the documentation of bargraph.CI.
bargraph.CI(x.factor = chickwts$feed, response = chickwts$weight, xlab = "Feed", ylab = "Weight", ci.fun = ciMean)
# The default valuefor ci.fun calculates displays the range of values within 1 std dev of the mean
# By specifying ciMean, we are changing this range to within 2 std dev of the mean