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

