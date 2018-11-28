# Author: Jussi Jaatinen
# Data wrangling exercise week 4

library(dplyr)
library(stringr)
library(ggplot2)
library(GGally)

# Read dataset from web

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
# Metadata http://hdr.undp.org/en/content/human-development-index-hdi

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")
# Metadata http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf

# Exploring data hd
str(hd)
dim(hd)
summary(hd)

# Changing names of variables

colnames(hd) <- c("HDI_Rank", "Country", "HDI_Index", "Life_Exp", "Edu_Exp", "Edu_Mean", "GNI", "GNI-HDI" )

# Exploring data gii

str(gii)
dim(gii)
summary(gii)

# Changing names of variables

# colnames(gii) <- c("Gii_Rank","Country", "Gii_Ind","Mat_Mortal","Teen_Preg","F_Rep_Parl","Scnd_edu_F", "Scnd_edu_M", "Work_F", "Work_M", "Edu_Rto_FM", "Work_Rto_FM")
colnames(gii) <- c("Gii_Rank","Country", "Gii_Ind","Mat_Mortal","Teen_Preg","F_Rep_Parl","Scnd_edu_F", "Scnd_edu_M", "Work_F", "Work_M")

ERG <- (gii$Scnd_edu_F / gii$Scnd_edu_M)
WRG <- (gii$Work_F / gii$Work_M)

gii <- mutate(gii, Edu_Rto_FM = ERG)
gii <- mutate(gii, Work_Rto_FM = WRG)

str(gii)

# Join datasets

join_by <- c("Country")

human <- inner_join(hd, gii, by = join_by)

str(human)

# Write table "alc" to .csv and .txt files

write.csv(human, file = "~/Documents/GitHub/IODS-project/data/human.csv")
write.table(human, file = "~/Documents/GitHub/IODS-project/data/human.txt")

# Week 5

# Load the ‘human’ data into R. Explore the structure and the dimensions of the data and describe the 
# dataset briefly, assuming the reader has no previous knowledge of it (this is now close to the reality, 
# since you have named the variables yourself). (0-1 point)
# 

# 
human <- read.table(file = "~/Documents/GitHub/IODS-project/data/human.txt", header = TRUE, sep = "")

str(human)

# Mutate the data: transform the Gross National Income (GNI) variable to numeric 

# remove the commas from GNI and print out a numeric version of it
GNI_numeric <- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric() 

# Mutate the data: transform the Gross National Income (GNI) variable to numeric 
human <- mutate(human, GNI = GNI_numeric)

# Exclude unneeded variables: keep only the columns matching the following variable names 
# (described in the meta file above):  "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor",
#"Ado.Birth", "Parli.F" (1 point)

included_variables <- c("Country",  "Life_Exp", "Edu_Exp",  "GNI",  "Mat_Mortal", "Teen_Preg", "F_Rep_Parl",  "Edu_Rto_FM", "Work_Rto_FM")
human <- select(human, one_of(included_variables))

# Remove all rows with missing values (1 point).

# print out a completeness indicator of the 'human' data
complete.cases(human)
# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))
# filter out all rows with NA values
human <- filter(human, complete.cases(human) == TRUE)

# Remove the observations which relate to regions instead of countries. (Last 7 obs.)

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human <- human[1:last, ]

# add countries as rownames
rownames(human) <- human$Country

# Remove original variable Country

human <- dplyr::select(human, -Country)

# The data should now have 155 observations and 8 variables. Save the human data in your data folder 
# including the row names. You can overwrite your old ‘human’ data. (1 point)

write.csv(human, file = "~/Documents/GitHub/IODS-project/data/human.csv", row.names = TRUE)
write.table(human, file = "~/Documents/GitHub/IODS-project/data/human.txt", row.names = TRUE, quote = TRUE)

human <- read.table(file = "~/Documents/GitHub/IODS-project/data/human.txt", sep = "")

# add countries as rownames
# rownames(human_) <- human_$Country
# human <- dplyr::select(human_, -Country)

human
str(human)

# Analysis

pairs(human)

