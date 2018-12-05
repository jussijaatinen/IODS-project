
# Author: Jussi Jaatinen
# Data wrangling exercise week 6

# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)

# Read the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
# Look at the (column) names of BPRS
names(BPRS)
# Look at the structure of BPRS
str(BPRS)
# Print out summaries of the variables
summary(BPRS)
# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(BPRSL$weeks, 5, 5)))
# Take a glimpse at the BPRSL data
glimpse(BPRSL)



# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')
# Factor variables ID and Group
RATS$ID <- as.factor(RATS$ID)
RATS$Group <-as.factor(RATS$Group)
# Glimpse the data
glimpse(RATS)
# Convert data to long form
RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group)
RATSL <- RATSL %>% mutate(Time = as.integer(substr(RATSL$WD, 3, 4)))  
# Glimpse the data
glimpse(RATSL)

