# Wrangling and Analysis of the data of the third week. Source data is from UCI Machine Learning Repository, Student Performance Data.
# Jussi Jaatinen
# 14.11.2018

# Load used libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(tidyverse)

# Read data from CSV-files (mat = Mathematics, por = Portugese)

math <- read.csv("~/Documents/GitHub/IODS-project/data/student-mat.csv", header = TRUE, sep = ";")
por <- read.csv("~/Documents/GitHub/IODS-project/data/student-por.csv", header = TRUE, sep = ";")

str(math)
str(por)

# Show column names

colnames(math)
colnames(por)

# Combine data, including pupils present in both subjects and remove duplicates

# Create a common columns vector

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# Join the two datasets by the common columns vector and add suffixes

types <- c(".math", ".por")
math_por <- inner_join(math, por, by = join_by, suffix = types)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# for every column name not used for joining, select first character value or take mean of numeric values
for(column_name in notjoined_columns) {
  
  two_columns <- select(math_por, starts_with(column_name))
  first_column <- select(two_columns, 1)[[1]]
  
  if(is.numeric(first_column)) {
    alc[column_name] <- round(rowMeans(two_columns))
  } else { 
    alc[column_name] <- first_column
  }
}

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# Check structure, variables and observations
glimpse(alc)

# Write table "alc" to .csv and .txt files

write.csv(alc, file = "~/Documents/GitHub/IODS-project/data/alc.csv")
write.table(alc, file = "~/Documents/GitHub/IODS-project/data/alc.txt")

# Analysis

glimpse(alc)

gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

# produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences

g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))


# define the plot as a boxplot and draw it

g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")

# find the model with glm()
m <- glm(high_use ~ famres + absences + sex, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model

coef(m)

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)
alc
# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = (probability > 0.5))

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, famres, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point() + aes(col = prediction)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)


# compute the average number of wrong predictions in the (training) data


# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = nrow(alc))

# 10-fold cross-validation

cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]




