rm(list=ls())

#Set working directory
setwd("C:/Users/jatin/Documents/Rcode/ml/New folder")

#loading csv files
train <- read.csv("train.csv")
test <- read.csv("test.csv")
feature <- read.csv("features.csv")
stores <- read.csv("stores.csv")

str(train)

#Changing the Date to date format
train$Date <- as.Date(train$Date ,"%d-%b-%y")
feature$Date <- as.Date(feature$Date ,"%d-%b-%y")

#mergering data set # store and feature
store_feature <- merge(stores, feature, by="Store")
#mergering data set # store_feature and train
train_store_feature <- merge(store_feature,train,by=c("Store","Date","IsHoliday"))

#converting variables to factor
train_store_feature$Store <- as.factor(train_store_feature$Store)
train_store_feature$IsHoliday <- as.factor(train_store_feature$IsHoliday)


str(train_store_feature)
#scaling the variable in training data frame
train_store_feature$Size <- scale(train_store_feature$Size)
train_store_feature$Temperature<- scale(train_store_feature$Temperature)
train_store_feature$Fuel_Price <- scale(train_store_feature$Fuel_Price)
train_store_feature$CPI <- scale(train_store_feature$CPI)
train_store_feature$Unemployment <- scale(train_store_feature$Unemployment)


#spilting to data frame to training and testing data frames in 70%-30% ratio
set.seed(12)
library(caTools)
split <- sample.split(train_store_feature$Sales,SplitRatio = 0.7)
training_set <- subset(train_store_feature,split == T)
test_set <- subset(train_store_feature,split == F)



library(randomForest)

#building regrestion model using random forest
rm_model <- randomForest(Sales ~ .,data = training_set[,-c(2)],ntree=5)
save(rm_model,file = "rm_model.rda") # to save the model
load(file = "rm_model.rda") # to load the model
#predicting sales value for test data set
rm_pred <- predict(rm_model,test_set[,-c(2,11)])

#ploting actual value vs predicting values to see the how acturate our model is
plot(test_set[,11],rm_pred)


#Changing the Date to date format
test$Date <- as.Date(test$Date ,"%d-%b-%y")

#mergering data set # store_feature and test
test_store_feature <- merge(store_feature,test,by=c("Store","Date","IsHoliday"))

#converting variables to factor
test_store_feature$Store <- as.factor(test_store_feature$Store)
test_store_feature$IsHoliday <- as.factor(test_store_feature$IsHoliday)


#scaling the variable in test data frame
test_store_feature$Size <- scale(test_store_feature$Size)
test_store_feature$Temperature<- scale(test_store_feature$Temperature)
test_store_feature$Fuel_Price <- scale(test_store_feature$Fuel_Price)
test_store_feature$CPI <- scale(test_store_feature$CPI)
test_store_feature$Unemployment <- scale(test_store_feature$Unemployment)

#using our random forest model to predict the sales forecast values.
test_pred <- predict(rm_model,test_store_feature[,-c(2,11)])
#storing back predicting vales to test data set
test$SalesForecast <-test_pred 

write.csv(test,"test_predicted.csv",row.names = F)
