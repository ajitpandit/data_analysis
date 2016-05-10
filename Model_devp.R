#loading the data
data_train <- read.table("datatraining.txt", header = TRUE, sep = ",")
data_test <- read.table("datatest.txt", header = TRUE, sep = ",")
data_test2 <- read.table("datatest2.txt", header = TRUE, sep = ",")
#loading libraries
#RWeka for J48 (aka C4.5 algorithm) and caret for loading ggplot2 and 
#lattice libraries, rpart and rattle for plotting functions,
#randomforest for variable importance plotting
library(RWeka)
library(caret)
library(rpart.plot)
library(rattle)
library(randomForest)

#checking for missing values
table(is.na(data_train))
table(is.na(data_test))
table(is.na(data_test2))

#converting occupancy from numeric to factor for C4.5 algorithm
data_train$Occupancy <-as.factor(data_train$Occupancy)
data_test$Occupancy <-as.factor(data_test$Occupancy)
data_test2$Occupancy <-as.factor(data_test2$Occupancy)

#formatting of date/time stamp to vector
data_train$date <-as.POSIXct(data_train$date, tz = "UTC")
data_test$date <-as.POSIXct(data_test$date, tz = "UTC")
data_test2$date <-as.POSIXct(data_test2$date, tz = "UTC")

#reviewing data types for algo
str(data_train)
str(data_test)
str(data_test2)

#randomising data_train table
set.seed(9850)
g <-runif(nrow(data_train))
data_train <- data_train[order(g),]

#applying C4.5 to data_train
m1 <- J48(Occupancy~., data = data_train)
#summarizing the model m1
summary(m1)
#plotting the model
png("dtree.png", res = 100, height = 8000, width = 6000)
plot(m1)
dev.off()

#making predictions for data_test
predictions <- predict(m1,data_test[,1:6])
table(predictions,data_test$Occupancy)
#     0           1
# 0   1641        15
# 1   52          957
# accuracy = 97.48%

#plot cp 
form <- J48(Occupancy ~ ., data = data_train)
m1 <- rpart(form,data=data_train,control=rpart.control(minsplit=20,cp=0))
summary(m1)
#prp(m1, varlen = 3)
plotcp(m1)

#rattle() GUI is used for co-relation plot and distribution plot
rattle()

#plotting variable importance chart
fit2 <- randomForest(Occupancy~.,data = data_train)
varImpPlot(fit2, type = 2)
