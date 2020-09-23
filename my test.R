library(dplyr)
data=read.csv(file.choose(), skip = 0, nrows = 13188, na.strings = " ")

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
View(data)


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
library(caret)

set.seed(123) 
split = sample.split(data$B_SIZE, SplitRatio = 0.7) 

training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE) 

table(training_set$B_SIZE)
str(training_set)

control <- trainControl(method="cv", number=100)
metric <- "Accuracy"


library(e1071)
library(randomForest)
library(caret)
#set.seed(123)         lda=fit.lda, 
#fit.lda <- train(B_SIZE~., data=data, method="lda", metric=metric, trControl=control)
set.seed(123)
fit.cart <- train(B_SIZE~., data=data, method="rpart", metric=metric, trControl=control)
set.seed(123)
fit.knn <- train(B_SIZE~., data=data, method="knn", metric=metric, trControl=control)
set.seed(123)
fit.svm <- train(B_SIZE~., data=data, method="svmRadial", metric=metric, trControl=control)
set.seed(123)
fit.rf <- train(B_SIZE~., data=data, method="rf", metric=metric, trControl=control)
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)



#dotploprint(fit.lda)
#predictions <- predict(fit.lda, validation)
#confusionMatrix(predictions, validation$B_SIZE)