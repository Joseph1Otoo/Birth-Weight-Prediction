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

## DROP 8 and 9 from B_SIZE

data <- droplevels(data %>% filter(B_SIZE != "8"))
data <- droplevels(data %>% filter(B_SIZE != "9"))

nrow(data)

#############CHANGING NUMERIC INTO FACTORS WITH THEIR LABELS#######

data$B_SIZE <- factor(data$B_SIZE,levels = c(1:5),labels = c("Very Large", "Larger than average","Average", "Smaller than average", "Very Small"))
data$PAR <- factor(data$PAR, levels = c(1:2), labels = c("Single", "Multiple"))
data$D_COMP <- factor(data$D_COMP, levels = c(1:3), labels = c("Born alive", "Born dead", "Lost pregnancy"))
data$AGE <- factor(data$AGE, levels = c(1:7), labels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"))
data$RES <- factor(data$RES, levels = c(1:2), labels = c("Urban","Rural"))
data$C_RES <- factor(data$C_RES, levels = c(0:4), labels = c("Large city","City","Town","Countryside","Abroad"))
data$EDU <- factor(data$EDU, levels = c(0:3), labels = c("No education","Primary","Secondary","Higher"))
data$M_ST <-factor(data$M_ST, levels = c(0:5), labels = c("Never married","Married","Living together","Widowed","Divorced","Not living together"))

str(data)
summary(data)

## Categories of birth weight in percentages

T=table(data$B_SIZE)
T
T1=round(prop.table(T), digits = 2)
round(100*T1, digits = 0)

## Categories of delivery outcomes in percentages

T2=table(data$D_COMP)
T2
T3=round(prop.table(T2), digits = 2)
round(100*T3, digits = 0)

# Splitting the dataset into the Training set and Test set 
library(caTools) 

set.seed(123) 
split = sample.split(data$B_SIZE, SplitRatio = 0.7) 

training_set = subset(data, split == TRUE) 
test_set = subset(data, split == FALSE) 

table(training_set$B_SIZE)
str(training_set)

### NEW SVM APPROACH

library(e1071)
mymodel= svm(B_SIZE~ AGE+RES+EDU+PAR+C_RES+M_ST, data = training_set)
summary(mymodel)

attributes(mymodel)

##### MISCLASSIFICATION ERROR FOR TRAINING SET

pred1 = predict(mymodel, training_set)

tab1 = table(Predicted = pred1, Actual = training_set[,27])
tab1

mr1=1 - sum(diag(tab1))/sum(tab1)
round(mr1*100, digits = 2)

### In the plot when there more variables we use 
## slice = list(v1=3,v2=4)
##It is worth noting that the plot is for continuos variables

# plot(mymodel, data, PAR~EDU, slice = list(AGE=3, RES=4))

## Confusion matrix and misclassification error

pred = predict(mymodel, test_set)

tab = table(Predicted = pred, Actual = test_set[,27])
tab

### MISCLASSIFICATION RATE

mr=1 - sum(diag(tab))/sum(tab)
round(mr*100, digits = 2)


### RANDOM FOREST

library(randomForest)

set.seed(222)

RMmodel = randomForest(B_SIZE ~ AGE+RES+EDU+PAR+C_RES+M_ST,data = training_set)

print(RMmodel)


#### MODEL AFTER TUNING PARAMETERS

RMmodel = randomForest(B_SIZE ~ AGE+RES+EDU+PAR+C_RES+M_ST,data = training_set,
                       ntree = 300, mtry = 10,
                       importance = TRUE,
                       proximity = TRUE)
print(RMmodel)


attributes(RMmodel)

RMmodel$terms

library(caret)

p1 = predict(RMmodel, training_set)
confusionMatrix(p1, training_set$B_SIZE)


p2 = predict(RMmodel, test_set)
confusionMatrix(p2, test_set$B_SIZE)

## ERROR RATE

plot(RMmodel)


## TUNE RF

t = tuneRF(training_set[,-27], training_set[,27],
           stepFactor = 0.5, plot = TRUE,
           ntreeTry = 300, trace = TRUE,
           improve = 0.05)

### NUMBER OF NODES FOR THE TREES

hist(treesize(RMmodel), main = "No. of nodes for trees",
     col = "blue")

## VARIABLE IMPORTANCE

varImpPlot(RMmodel, sort = TRUE,
           n.var = 6,
           main = "Top 6 variables of importance")
importance(RMmodel)
varUsed(RMmodel)


### PARTIAL DEPENDENCY PLOT

partialPlot(RMmodel, training_set, RES, "Average")


## EXTRACT SINGLE TREE

getTree(RMmodel, 1, labelVar = TRUE)


### MULTI- DIMENSIONAL  SCALING PLOT OF THE PROXIMITY MATRIX

MDSplot(RMmodel, training_set$B_SIZE)
