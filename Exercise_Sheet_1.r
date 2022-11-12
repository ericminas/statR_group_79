### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete, please do not leave out subquestions!

## Please write below your (and your teammates') name and matriculation number. 
## Name: Chaahat Jain
## Matriculation number: 7025099
## Name:
## Matriculation number:
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
?getwd()
## c) Change your working directory to another directory.
setwd("C:/Users/Chaahat/Desktop/StatsticsR")
###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the range for which you want to plot the 
##    normal distribution (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.75. Assign this to the 
##    variable x.
x <-  seq(-5,5,0.75)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution. Use the defaults for mean and sd (standard normal distribution)
help(dnorm)
y <- dnorm(x)
## c) Now use plot() to plot the normal distribution for z values of "x". Specify
## the type to be line using the suitable argument of plot()
plot(x, y, type = "l")
## d) This plot does not look like a smooth normal distribution. Change the vector
##  x to have smaller increments and plot again (you also need to update y)

x <-  seq(-5,5,0.25)
y <- dnorm(x)
plot(x, y, type = "l")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dotted line, set the argument 'lty' to 3.
?abline 
abline(v = mean(x), lty = 3)
## f) Take a look at the trees dataset (You can see it by typing "trees"), which 
##    has height, diameter and volume.
##    Then select only the Height part and store it in a variable "treesHeight".
trees 
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
# Getting a 62 or more extreme value is very unlikely and this would be statistically significant. 
# Getting an 86 or more extreme value is unlikely but would not be statistically significant.

## j) Use the random sampling function in R to generate 25 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 58 to 94 using xlim. 
##    Fix the number of breaks to 11 using breaks

for (i in 1:5){
  randomY <- sample(y, 25)
  hist(randomY, breaks = 11)
}

## k) What do you observe in j?
# None of them have a negative value. Data surrounding 0.00 occurs most frequently.


###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
# install.packages("languageR")
library(languageR)
## b) Specifically, we will deal with the dataset 'lexdec'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from
?lexdec
# It comes from lexical decision latencies elicited from 21 subjects for 79 English concrete nouns with variables linked to subject or word. 
# This data was collected by Jen Hay, University of Cantrerbury, Christchurch, New Zealand. 

## c) Inspect 'lexdec'. Look at the head, tail, 
##    and summary. 
head(lexdec)
tail(lexdec)
summary(lexdec)
## d) What do head and tail show you?
# head and tail gives us the first and last 5 rows of the dataframe respectively 

## e) Look at the first 15 rows of the data.frame
lexdec[1:15,]
## f) The file contains multiple observations for each participant. Create a 
##   subset only including subject number M2 and assign it to M2
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?
# install.packages("dplyr")
library(dplyr)
M2 <- lexdec %>% filter(Subject == "M2")
count(M2) # there are 79 observations for M2 

## g) looking at the summary of M2, what can you find out about the demographic 
##    parameters of this participant?
# M2 is not English speaking and is a female. 

## h) Create a histogram (using hist()) of "RT" (logarithm of reaction time) 
##    for M2
hist(M2$RT, breaks = 11)

## i) Create a kernel density plot for this data using density()
plot(density(M2$RT))
## j) What is the difference between the two?
# The density plot is a smoother curve than the histogram. 
# The histogram provides more accurate information regarding frequency of values. 
# The density plot is easier to compare to a normal distribution. 


## k) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)
# This data is slightly right-skewed. We would check by comparing the mean and median values. 

## l) Looking at the graph, do you think the data is skewed? In which direction?
# This data is slightly right-skewed.

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
# This is a ratio scale. 

## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pid', and your 
##    participants should be labeled from 1 to 26
pid <- c(1:26)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18,	15,	18,	19,	23,	17,	18,	24,	17,	14,	16,	16,	17,	21,	22,	18,	20,	21,	20,	20,	16,	17,	17,	18,	20,	26)
##,e) Create a dataframe including pid, obs and lib. Assign this to 'stories'. 
stories <- data.frame(pid, obs, lib)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pid' and 'lib'?
summary(stories)
# pid is class int 
# lib is class charachter 

## g) Change the class of 'pid' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)
stories$pid <- as.factor(stories$pid)
stories$lib <- as.factor(stories$lib)

# Factor is better for pid since we do not want to perform mathematical operations over pid (ex: taking the mean)
# Factor is better lib as charachters are hard to work with, and in this case, they clearly define 2 separate types(Y or N)

## h) Create a boxplot of obs for the two lib groups
boxplot(obs ~ lib)
## i) Are there outliers in one of the lib groups?
# Yes, there are outliers in both of the lib groups. 

## j) Which group shows the larger interquartile range? 
# Group N shows the larger interquartile range

## k) Which one has the greater overall range?
stories %>% group_by(lib) %>% summarize(max = max(obs), min = min(obs), med = median(obs), avg = mean(obs))
# Both of them have the same range (10)

## l) What is a whisker? Why is the upper whisker of group "Y" so short?
# The whiskers represent the minimum and maximum values within 1.5*IQR of the median.
# The upper whisker is short because the maximum value is closer to the third quartile compared to the IQR.
# 
## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?
# The box plot plots the median (50% quartile). 
# The mean is sensitive to outliers which is why the median and mean are different.
