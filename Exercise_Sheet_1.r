### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete, please do not leave out subquestions!

## Please write below your (and your teammates') name and matriculation number. 
## Name: Eric Minas
## Matriculation number: 2568884
## Name: Chaahat Jain
## Matriculation number: 7025099
## Name:
## Matriculation number:


## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()
## b) Get help with this function.
?getwd
## c) Change your working directory to another directory.
# setwd("statR")
###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the range for which you want to plot the 
##    normal distribution (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.75. Assign this to the 
##    variable x.
x <- seq(from = -5, to = 5, by = 0.75)
## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution. Use the defaults for mean and sd (standard normal distribution)
y <- dnorm(x)
## c) Now use plot() to plot the normal distribution for z values of "x". Specify
## the type to be line using the suitable argument of plot()
plot(x, y, type = "l")
## d) This plot does not look like a smooth normal distribution. Change the vector
##  x to have smaller increments and plot again (you also need to update y)
x <- seq(from = -5, to = 5, by = 0.25)
y <- dnorm(x)
plot(x, y, type = "l")
## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dotted line, set the argument 'lty' to 3.
abline(v = mean(x), lty = 3)
## f) Take a look at the trees dataset (You can see it by typing "trees"), which 
##    has height, diameter and volume.
##    Then select only the Height part and store it in a variable "treesHeight".
treesHeight <- trees$Height
## g) Calculate the mean and standard deviation of treesHeight and plot a normal
##    distribution with these parameters (NB:you should not use the same x range 
##    as above!)
avg <- mean(treesHeight)
std <- sd(treesHeight)

x <- seq(-4,4, 0.25) * std + avg
y <- dnorm(x, mean = avg, sd = std)
plot(x, y, type = "l")
## h) We observe two additional tree height values (62 and 86). What's the 
##    likelihood that these heights (or more extreme ones) respectively 
##    come from the normal distribution from g)?
pnorm(62, avg, std)                     # from z-table, 62 or more extreme values would occur <= 1.4% of the time
pnorm(86, avg, std, lower.tail = FALSE) # from z-table, 86 or more extreme values would occur <= 5.9% of the time

## i) What do you conclude from the p-values? (informal)
# That either values are not very likely to happen if the results are composed 
# as a normal distribution, but also that 86 is more likely to happen than 62.
# Getting a 62 or more extreme value is very unlikely and this would be statistically significant. 
# Getting an 86 or more extreme value is unlikely but would not be statistically significant.


## j) Use the random sampling function in R to generate 25 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 58 to 94 using xlim. 
##    Fix the number of breaks to 11 using breaks

# I did the repetition of rnorm and the hist-generation five times,
# in case I misread the exercise. 

# create empty vector
res <- c()
# do the random selection 5 times
for( i in 0:4)
  # add the selected values at the end of the vector
  res <- append(res, rnorm(25, mean = m, sd = s), after = (i * 25))

# plot the histogram
hist(res, breaks = 11, xlim = c(58, 94), main = "Histogram of randomly selected values in trees$Heights")

## k) What do you observe in j?
# That in my case the histogram did rarely follow the normal distribution.

###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)

install.packages("dplyr")
library(dplyr)

## b) Specifically, we will deal with the dataset 'lexdec'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from

# Description = 
# Lexical decision latencies elicited from 21 subjects for 79 English concrete nouns, with variables
# linked to subject or word.

## c) Inspect 'lexdec'. Look at the head, tail, 
##    and summary. 
head(lexdec)
tail(lexdec)
summary(lexdec)
## d) What do head and tail show you?
# head -> The first 6 entries, as well as the names of the tables cols
# tail -> the last 6 entries, as well as the names of the tables cols

## e) Look at the first 15 rows of the data.frame
head(lexdec, 15)

## f) The file contains multiple observations for each participant. Create a 
##   subset only including subject number M2 and assign it to M2
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?
M2 <- subset(lexdec, lexdec$Subject == "M2")
# There are 79 entries to the Subject M2 (see: length(M2))

## g) looking at the summary of M2, what can you find out about the demographic 
##    parameters of this participant?
summary(M2)
# M2 is classified as a female
# M2 is not a native English speaker

## h) Create a histogram (using hist()) of "RT" (logarithm of reaction time) 
##    for M2

hist(M2$RT, main = "histogram for M2 - RT", breaks = 11)
## i) Create a kernel density plot for this data using density()
# With the histogram
m2_d <- density(M2$RT)
lines(m2_d, lwd = 2, col = "red")

# Separate plot
plot(density(M2$RT))

## j) What is the difference between the two?
# The density plot is a smoother curve than the histogram. 
# The histogram provides more accurate information regarding frequency of values. 
# The density plot is easier to compare to a normal distribution. 

## k) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)
# Looking at the density, it seems not to stem from a normal distribution,
# because the values are right-skewed (positively skewed). 
# The standard error can be used to see how reliably the data could stem from a 
# normal distribution. 
# We could also check by comparing the mean and median values.

## l) Looking at the graph, do you think the data is skewed? In which direction?
# I think it is positively skewed (i.e. to the left)

#############################################
### Exercise 4: Dataframes and boxplots
#############################################
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 26 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18	15	18	19	23	17	18	24	17	14	16	16	17	21	22	18	20	21	20	20	
# 16	17	17	18	20	26


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why.
# This is a ratio scale. 
# This is discrete data as it is being counted. There is no value between 20 and 21.

## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
lib = c(rep("Y",13),rep("N",13))
# This is a ratio scale. Since it is numeric. 
# If these are considered to be strings, then it is a nominal scale. 

## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pid', and your 
##    participants should be labeled from 1 to 26
pid <- c(seq(from = 1, to = 26, by = 1))

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18,	15,	18,	19,	23,	17,	18,	24,	17,	14,	16,	16,	17,	21,	22,	18,	
         20,	21,	20,	20, 16,	17,	17,	18,	20,	26)

## e) Create a dataframe including pid, obs and lib. Assign this to 'stories'. 
stories <- data.frame(pid, obs, lib)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pid' and 'lib'?
summary(stories)
class(stories$pid) # -> pid = numeric
class(stories$lib) # -> lib = character

## g) Change the class of 'pid' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)

# Factor is better for pid since we do not want to perform mathematical operations over pid (ex: taking the mean) 
stories$pid <- as.factor(stories$pid)

# lib should be a factor, because there are only two values and we want 
# to be able to count occurrences more easily
stories$lib <- as.factor(stories$lib)

## h) Create a boxplot of obs for the two lib groups
boxplot(obs ~ lib, data = stories, xlab = "Has a library card", ylab = "# of 'and then's")

## i) Are there outliers in one of the lib groups?
# "No" has one outlier (26) and "Yes" has two (23,24)

## j) Which group shows the larger interquartile range? 
# "No" has the larger quantile range

## k) Which one has the greater overall range?
stories %>% group_by(lib) %>% summarize(max = max(obs), min = min(obs), med = median(obs), avg = mean(obs))
# Both of them have the same range (10)

## l) What is a whisker? Why is the upper whisker of group "Y" so short?
# The lines extending from the box. This line shows the largest/smallest 
# value in the data set that is within the interquartile range.
# The upper whisker is short because the maximum value within 1.5*IQR is closer to the third quartile compared to 
# the IQR.

## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?
# The box plot plots the median (50% quartile). 
# The mean is sensitive to outliers which is why the median and mean are different.