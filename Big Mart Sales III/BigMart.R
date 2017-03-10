setwd("C:/Users/shashwat2014/Documents/Analytics Vidhya/Practice/Big Mart Sales")
train<-read.csv("train.csv",  na.strings = "")
test<-read.csv("test.csv", na.strings = "")
library(ggplot2)
ggplot(train,aes(Item_Fat_Content, Item_Outlet_Sales))+geom_boxplot()
levels(train$Item_Fat_Content)
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)=="LF"]<-"Low Fat"
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)=="low fat"]<-"Low Fat"
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)=="reg"]<-"Regular"
summary(train$Item_Fat_Content)


train$Item_Weight[which(is.na(train$Item_Weight))]<-c$mean_weight[train$Item_Type[which(is.na(train$Item_Weight))]==c$Item_Type]
library(dplyr)
b<-group_by(train,Item_Type)
c<-summarise(b, mean(Item_Weight,na.rm = TRUE))


names(c)[2]<-paste("mean_weight")
  

which(c$Item_Type[1]==train$Item_Type )

table(is.na(train$Item_Weight))


missing<-which(is.na(test$Item_Weight))

for(i in 1:16){
  test$Item_Weight[missing[missing%in%which(c$Item_Type[i]==test$Item_Type)]]=c$mean_weight[i]
}
table(is.na(train$Item_Weight))
colSums(is.na(train))

train$Outlet_Size[train$Outlet_Type=="Grocery Store"]<-"Small"
library(gmodels)
CrossTable(train$Outlet_Type,train$Outlet_Size)

test$Outlet_Duration<-(2016-test$Outlet_Establishment_Year)

test$Item_Code<-substr(as.character(test$Item_Identifier),1,3)
test$Item_Code<-as.factor(test$Item_Code)

library(dummies)
test<-dummy.data.frame(test,names = c("Item_Fat_Content"),sep="_")
test<-dummy.data.frame(test,names = c("Item_Type"),sep="_")
test<-dummy.data.frame(test,names = c("Outlet_Size"),sep="_")
test<-dummy.data.frame(test,names = c("Outlet_Location_Type"),sep="_")
test<-dummy.data.frame(test,names = c("Outlet_Type"),sep="_")

x<-c(Item_Identifier,Outlet_Identifier,Outlet_Establishment_Year,Item_Code)
linear_model<-lm(Item_Outlet_Sales~.-Item_Identifier-Outlet_Identifier-Outlet_Establishment_Year-Item_Code-LowSaleDuration, data=train)









#Cross validation
k<-5
train$id <- sample(1:k, nrow(train), replace = TRUE)
list <- 1:k
# prediction and train set data frames that we add to with each iteration over
# the folds
prediction <- data.frame()
trainsetCopy <- data.frame()
#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
#function for k fold
for(i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create train set
  trainingset <- subset(train, id %in% list[-i])
  trainset <- subset(train, id %in% c(i))
  
  #run a regression linear model
  linear_model<-lm(Item_Outlet_Sales~.-Item_Identifier-Outlet_Identifier-Outlet_Establishment_Year, data=trainingset)
  
  #remove response column 35, Item_Outlet_Sales
  temp <- as.data.frame(predict(linear_model, trainset[,-35]))
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's train set to the train set copy data frame
  
  trainsetCopy <- rbind(trainsetCopy, as.data.frame(trainset[,35]))
  
  progress.bar$step()
}
# add predictions and actual Sepal Length values
result <- cbind(prediction, trainsetCopy[,1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
# As an example use Mean Absolute Error as Evalution 
summary(result$Difference)
tencrosserrormean<-mean(result$Difference)
result$Squared_diff<-result$Difference^2
sqrt(sum(result$Squared_diff)/nrow(result))

test$Outlet_Size[is.na(test$Outlet_Size)]<-"Small"
test$LowSaleDuration<-ifelse(test$Outlet_Duration==18,0,1)

prediction<-predict(linear_model, newdata=test)
train<-train[-38]
write.csv(prediction, "submit1.csv")
#1203.169
#1127.522

#Normalise
train$Outlet_Duration<-(train$Outlet_Duration-min(train$Outlet_Duration))/(max(train$Outlet_Duration)-min(train$Outlet_Duration))

test$Item_Weight<-(test$Item_Weight-min(test$Item_Weight))/(max(test$Item_Weight)-min(test$Item_Weight))
test$Item_Visibility<-(test$Item_Visibility-min(test$Item_Visibility))/(max(test$Item_Visibility)-min(test$Item_Visibility))
test$Item_MRP<-(test$Item_MRP-min(test$Item_MRP))/(max(test$Item_MRP)-min(test$Item_MRP))
test$Outlet_Duration<-(test$Outlet_Duration-min(test$Outlet_Duration))/(max(test$Outlet_Duration)-min(test$Outlet_Duration))

linear_model<-lm(Item_Outlet_Sales~.-Item_Identifier-Outlet_Identifier-Outlet_Establishment_Year, data=train)
prediction<-predict(linear_model, newdata=test)
for(i in 1:5681){
  if(prediction[i]<0){
  prediction[i]=0
}
}
write.csv(prediction, "submit2.csv")

#Ridge
x <- as.matrix(train[,c(2:22,25:34,36)])
y <- as.matrix(train[,35])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.1, lambda=0.001)
# summarize the fit
summary(fit)

x1<-as.matrix(test[,c(2:22,25:35)])
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
for(i in 1:5681){
  if(predictions[i]<0){
    predictions[i]=0
  }
}
write.csv(predictions, "submit3.csv")
#Poorer performance with ridge

#Lasso
library(lars)
x <- as.matrix(train[,c(2:22,25:34,36)])
y <- as.matrix(train[,35])
# fit model
fit <- lars(x, y, type="lasso")
# summarize the fit
summary(fit)
best_step <- fit$df[which.min(fit$RSS)]
predictions <- predict(fit, x1, s=best_step, type="fit")$fit
rmse <- sqrt(mean((y - predictions)^2))
write.csv(predictions,"submit_lasso.csv")

fit <- glmnet(x, y, family="gaussian", alpha=0.01, lambda=0.001)
predictions <- predict(fit, x, type="link")
for(i in 1:5681){
  if(predictions[i]<0){
    predictions[i]=0
  }
}
sqrt(mean((y^2 - predictions^2)^2))

#Normalise target variable
train$Item_Outlet_Sales<-sqrt(train$Item_Outlet_Sales)
fit <- glmnet(x, y, family="gaussian", alpha=0.01, lambda=0.001)
plot(fit)
linear_model<-lm(log(Item_Outlet_Sales)~.-Item_Identifier-Outlet_Identifier-Outlet_Establishment_Year, data=train)
plot(linear_model)

summary(fit)
summary(linear_model)
plot(linear_model)

train<-train[-c(1522,1493,8076),]
linear_model<-lm(log(Item_Outlet_Sales)~.-Item_Identifier-Outlet_Identifier-Outlet_Establishment_Year, data=train)
summary(linear_model)
plot(linear_model)
train<-train[-c(2854,1807,7404),]
linear_model<-lm(log(Item_Outlet_Sales)~.-Item_Identifier-Outlet_Identifier-Outlet_Establishment_Year, data=train)
summary(linear_model)
plot(linear_model)

library(Metrics)
rmse(train$Item_Outlet_Sales, exp(linear_model$fitted.values))

library(rpart)
library(e1071)
library(caret)
library(rpart.plot)
kaamkatrain<-train[,-c(1,23,24,37)]
names(kaamkatrain)<-gsub(" ", "_",colnames(kaamkatrain))
fitControl<-trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
tree_model <- train(Item_Outlet_Sales ~ ., data = kaamkatrain, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
print(tree_model)

main_tree <- rpart(Item_Outlet_Sales ~ ., data = kaamkatrain, control = rpart.control(cp=0.01))
prp(main_tree)
pre_score<-predict(main_tree, type = "vector")
rmse(kaamkatrain$Item_Outlet_Sales, pre_score)
names(test)<-gsub(" ","_",fixed=TRUE,x=colnames(test))

pre_test<-predict(main_tree, test[,-c(1,23,24,37)], type="vector")
for(i in 1:5681){
  if(pre_test[i]<0){
    pre_test[i]=0
  }
}
write.csv(pre_test, "submittree.csv")

#randomforest
library(randomForest)
control <- trainControl(method = "cv", number = 5)
rf_model <- train(Item_Outlet_Sales ~ ., data = kaamkatrain, method = "parRF", trControl =control, prox = TRUE, allowParallel = TRUE)

rfmodel<-randomForest(Item_Outlet_Sales~.,data = kaamkatrain, mtry=6, ntree=100)
plot(rfmodel)
summary(rfmodel)
print(rfmodel)
varImpPlot(rfmodel)
x<-c(5:20)
rfmodel2<-randomForest(Item_Outlet_Sales~.-c(5:20),data = kaamkatrain, mtry=6, ntree=100)
fit<-cforest(Item_Outlet_Sales~., data=kaamkatrain, controls=cforest_unbiased(ntree=200, mtry=6))
predictions<-predict(fit,test[,-c(1,23,24,37)], OOB=TRUE, type = "response")
write.csv(predictions, "submitcf.csv")

#gbm
library(gbm)
gbmmodel<-gbm(Item_Outlet_Sales~., data=kaamkatrain, distribution = "gaussian", n.trees = 100,interaction.depth = 6, shrinkage = 0.1,bag.fraction = 0.6, cv.folds=5, n.minobsinnode=10, verbose = TRUE)
best.iter <- gbm.perf(gbmmodel,method="cv")
summary(gbmmodel, n.trees=best.iter)

predictions<-predict(gbmmodel, test[,-c(1,23,24,37)], type = "response", n.trees = 100)
predictions<-predict(gbmmodel, kaamkatrain, best.iter)
sqrt(mean((predictions-kaamkatrain$Item_Outlet_Sales)^2))
write.csv(predictions, "submitgbm1.csv")

#xgboost
library(xgboost)
xgb<-xgboost(data = data.matrix(kaamkatrain[,c(1:31,33)]), 
             label=data.matrix(kaamkatrain[,c(32)]),
             eta=0.1,
             max_depth=6,
             nround=50,
             subsample=0.5,
             seed=1,
             eval_metric="rmse",
             objective="reg:linear",
             colsample_bytree=0.4,
             min_child_weight=0.16,
             verbose=2)
y_pred <- predict(xgb, data.matrix(test))
write.csv(y_pred, "submitxgb.csv")







