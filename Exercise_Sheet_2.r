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

## Name: Eric Minas
## Matriculation number: 2568884

###############################################################
### Exercise 1: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with
## different sizes.

## a) We will not use data from a normal distribution, but work with the poisson distribution, which is
## often used for count data. We will use the dataset discoveries, please find out what it is about

## please run the following line to convert the data into numeric
discoveries = as.numeric(discoveries)
# discoveries contains the discoveries made per year, starting in year 1860.

## b) Take a look at the dataset using the table() function and histogram and boxplot.
table(discoveries)
hist(discoveries)
boxplot(discoveries)

## c) Compare mean and median of the dataset
mean(discoveries) # = 3.1
median(discoveries) # = 3

# The mean and median are almost the same value.

## d) Create a sample of a normal distribution of equal length and with the same mean and
##   standard deviation as the observed sample, assign it to norm_disc
norm_disc <-
  rnorm(length(discoveries),
        mean = mean(discoveries),
        sd = sd(discoveries))

## e) Make a histogram of norm_disc and compare this to the histogram of the original dataset from b
# to compare: uncomment the line below
# hist(discoveries)
hist(norm_disc)

## f) Describe the differences observed in e)
# The histogram of discoveries is positively skewed with a concentration of values in the smallest "bucket"
#, which holds values from 0 to 2.  norm_disc's histogram also has a concentration of values in the 0-2 "bucket",
# but it contains values < 0. However, it does also contain values greater than 10,
# which were not present in the first histogram.

## g) Now, we are going to draw a smaller sample from discoveries.
### Use the function sample() to create a sample of four instances from discoveries
### assign it to sample4
sample4 <- sample(discoveries, size = 4)

## h) draw another 2 samples of 4 called sample4b and sample4c
sample4b <- sample(discoveries, size = 4)
sample4c <- sample(discoveries, size = 4)

## i) calculate the mean for each of the three samples and store them in the vector means4
means4 <- c(mean(sample4), mean(sample4b), mean(sample4c))

## j) Are the values different? Why?
means4
# Yes, the means are different. This happens because different values are picked
# and the sample size is so small (4% of the available data).

## k)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 4. However, we don't want to repeat
###   question h and i 1000 times. Use a for loop to draw 1000 samples of size 4
###   and store the mean of each sample in the vector means4.

# reset means4
means4 <- c()
for (i in 1:1000) {
  means4 <- append(means4, mean(sample(discoveries, size = 4)))
}

## l) Repeat the for-loop in question k, but use a sample size of 40.
##    Assign this to 'means40' instead of 'means4'.

means40 <- c()
for (i in 1:1000) {
  means40 <- append(means40, mean(sample(discoveries, size = 40)))
}

## m) Explain in your own words what 'means4' and 'means40' now contain.
##    How do they differ?
# create comparison boxplot
comp <- data.frame(means4 = means4, means40 = means40)
boxplot(comp)
# Both vectors contain 1000 mean values of samples from the data set.
# The difference it that the means within the vectors stem from different sample sizes.
# Due to the law of big numbers, the values of means40 are grouped closer together and there are less outliers.
# Check the boxplot form above to see it directly.

## n) Draw histograms of means4 and means40. Describe in what way they differ
hist(means4)
hist(means40)
# The histogram of means 40 looks closer to a bell-curve, while the histogram of means4 is more right-skewed.

## o) Why do you observe a skew for means4, but not for means40?
# This skew is due to the samples: The histogram of discoveries showed that
# most values were between 0 and 2 with the second most values within 2 and 4.
# This means that the smaller the sample the more likely it is that the sample contains mostly values that are smaller than or equal to 4.
# Looking at the histogram, we can see that this holds true, as seen in the drop-off at 4 with most data points being located to the left of that drop-off.

###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an
## unknown population parameter.
## The population parameter is what we're trying to find out.
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?

# The confidence interval is the range that you expect the values from your replication to be within.

## b) please install and load packages sciplot and lsr
install.packages("sciplot")
library(sciplot)

install.packages("lsr")
library(lsr)

install.packages("plyr")
library(plyr)

## c) calculate 95% Confidence Intervals for discoveries, sample4 and sample4c. You can
##    use the function ciMean()

# I calculated the samples again, in order to not have to select the whole doc all the time
sample4 <- sample(discoveries, size = 4)
sample4c <- sample(discoveries, size = 4)

ciMean(discoveries, conf = 0.95)
ciMean(sample4)
ciMean(sample4c)
## d) Why are these intervals so different?
# This is again due to the sample size.

## e) Is the true mean contained in each interval?
mean(discoveries)
# Yes, each confidence interval of my samples included the mean of discoveries,
# although it never was the mean of the confidence interval.

## f) In the following, we will check whether the CI behaves as expected.
##   What is the true mean in our example?
# The mean of discoveries (i.e. 3.1)

## g) Change your for loop from above (subquestion 1l) to calculate the confidence interval
##  instead of the mean for 1000 samples of size 15. Then check whether the confidence
##  interval contains the true mean and save the result in a vector called TrueMeanContained.
##  Hint: You will need to compare the mean to the lower and the upper bound of the
##  confidence interval
## ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound

TrueMeanContained <- c()
ci <- 0
m <- mean(discoveries)

for (i in 1:1000) {
  ci <- ciMean(sample(discoveries, size = 15))
 
  TrueMeanContained <-
    append(TrueMeanContained, (ci[1] <= m && ci[2] >= m))
}


TrueMeanContained
## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
##   the mean 95% of the time. Does it?
NumTMC <- as.numeric(TrueMeanContained)

if (sum(NumTMC) >= 950) {
  print("The mean is included 95% of the time")
} else {
  print(paste("The mean is not included 95% of the time. Actual value: ", sum(NumTMC)/1000))
}

## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
## the dataset chickwts, which contains weight of chickens after being administered different kinds of
## food for 6 weeks.
## Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor
bargraph.CI(x.factor = chickwts$feed, response = chickwts$weight)

## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
##  Hint: Look into the documentation of bargraph.CI.
bargraph.CI(x.factor = chickwts$feed, response = chickwts$weight, ci.fun = ciMean)

# The bars in the graph with ci.fun are smaller, they only go up to ~300, while the other graph goes up to ~350.
# The reason for this is the different function for the CI: the default function always chooses +/- the SD,
# which is not necessarily correct. 