

library(dplyr)

data=read.csv(file.choose(), header = T, na.strings = " ")

str(data)
summary(data)
View(data)

## REMOVE UNWANTED VARIABLE and MISSING DATA

data[,24]<-NULL

data <- filter(data, !(is.na(P_EDU)))
data <- filter(data, !(is.na(P_JBT)))
data <- filter(data, !(is.na(JBT)))
data <- filter(data, !(is.na(OCC)))
data <- filter(data, !(is.na(B_SIZE)))

## DROP DK, SINGLE and MULTIPLE

data <- droplevels(data %>% filter(B_SIZE != "DK"))
data <- droplevels(data %>% filter(B_SIZE != "Single"))
data <- droplevels(data %>% filter(B_SIZE != "Multiple"))
data <- droplevels(data %>% filter(B_SIZE != ""))

nrow(data)

str(data)
summary(data)

T=table(data$B_SIZE)
T
T1=round(prop.table(T), digits = 2)
round(100*T1, digits = 0)

# Splitting the dataset into the Training set and Test set 
library(caTools) 

set.seed(123) 
split = sample.split(data$B_SIZE, SplitRatio = 0.7) 

training_set = subset(data, split == TRUE) 
test_set = subset(data, split == FALSE) 

table(training_set$B_SIZE)
str(training_set)
