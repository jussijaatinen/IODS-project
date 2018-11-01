# 29.10.2018
# Author: Jussi Jaatinen
# Data input and wrangling exercise

# Set working directory 
setwd("~/Documents/GitHub/IODS-project") 

# Part 1: Data wrangling

# Download file from a web source and check its structure

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header = TRUE) 
str(lrn14)

# Filter only needed variables, summing and normalizing point values

library(dplyr)
library(ggplot2)
library(GGally)

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep, surface and strategic learnings and create columns 'deep', 'surf' and 'stra' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

# Scale summed variable 'attitude' (total points of 50 to the scale of Likert 1-5) by dividing each number in the column vector by 10
Scaled_Attitude <- lrn14$Attitude / 10

# Overwrite column 'attitude' with scaled values
lrn14$Attitude <- Scaled_Attitude

# choose a handful of columns to keep
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))

# Exclude participants with zero points
learning2014_no_zeros <- filter(learning2014, Points != 0)

str(learning2014_no_zeros)

# Write modified data to a CSV-file

write.csv(learning2014_no_zeros, file = "~/Documents/GitHub/IODS-project/data/learning2014.csv")
write.table(learning2014_no_zeros, file = "~/Documents/GitHub/IODS-project/data/learning2014.txt")

# learning2014_read <- read.csv("~/Documents/GitHub/IODS-project/data/learning2014.csv")
learning2014 <- read.table("~/Documents/GitHub/IODS-project/data/learning2014.txt")

# Part two: Analysis

# Summary of data: Variables and their statistics, dimensions of the table and structure of the data table.

summary(learning2014)
dim(learning2014)
str(learning2014)

# Distributions and correlations

ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

# Linear models with different variable combinations

# Dependent variable: Points - Explanatory variables: Attitude, Strategic learning, Surface learning

My_model1 <- lm(learning2014$Points ~ learning2014$Attitude + learning2014$stra + learning2014$surf, data = learning2014)
summary(My_model1)

# Dependent variable: Points - Explanatory variables: Attitude, Strategic learning

My_model2 <- lm(learning2014$Points ~ learning2014$Attitude + learning2014$stra, data = learning2014)
summary(My_model2)

# Dependent variable: Points - Explanatory variables: Attitude, Surface learning

My_model3 <- lm(learning2014$Points ~ learning2014$Attitude + learning2014$surf, data = learning2014)
summary(My_model3)

# Dependent variable: Points - Explanatory variable: Attitude

My_model4 <- lm(learning2014$Points ~ learning2014$Attitude, data = learning2014)
summary(My_model4)

# Dependent variable: Points - Explanatory variables: Attitude, Strategic learning, Gender

My_model5 <- lm(learning2014$Points ~ learning2014$Attitude + learning2014$stra + learning2014$gender, data = learning2014)
summary(My_model5)

# Produce the following diagnostic plots: Residuals vs Fitted values, Normal QQ-plot and Residuals vs Leverage. 
# Explain the assumptions of the model and interpret the validity of those assumptions based on the diagnostic plots.

par(mfrow = c(2,2))

a <- c(1, 2, 5)
plot(My_model4, which = a)
