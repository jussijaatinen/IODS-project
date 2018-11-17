# Author: Jussi Jaatinen
# Data wrangling exercise week 4

library(dplyr)

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

colnames(hd) <- c("HDI_Rank", "Country", "HDI_Index", "Life_Exp_Years", "Edu_Exp_Years", "Edu_Mean_Years", "GNI", "GNI-HDI_Rank" )

# Exploring data gii

str(gii)
dim(gii)
summary(gii)

# Changing names of variables

# colnames(gii) <- c("Gii_Rank","Country", "Gii_Index","Mat_Mortal_Ratio","Teen_Pregnancy_Rate","Repres_Parliament","Second_grade_edu_F", "Second_grade_edu_M", "Working_F", "Working_M", "Edu_Ratio_Gender", "Working_Ratio_Gender")
colnames(gii) <- c("Gii_Rank","Country", "Gii_Index","Mat_Mortal_Ratio","Teen_Pregnancy_Rate","Repres_Parliament","Second_grade_edu_F", "Second_grade_edu_M", "Working_F", "Working_M")
# Adding two new variables: Edu_Ratio_Gender and Working_Ratio_Gender

ERG <- (gii$Second_grade_edu_F / gii$Second_grade_edu_M)
WRG <- (gii$Working_F / gii$Working_M)

gii <- mutate(gii, Edu_Ratio_Gender = ERG)
gii <- mutate(gii, Working_Ratio_Gender = WRG)

str(gii)

# Join datasets

join_by <- c("Country")

human <- inner_join(hd, gii, by = join_by)

str(human)

# Write table "alc" to .csv and .txt files

write.csv(human, file = "~/Documents/GitHub/IODS-project/data/human.csv")
write.table(human, file = "~/Documents/GitHub/IODS-project/data/human.txt")




