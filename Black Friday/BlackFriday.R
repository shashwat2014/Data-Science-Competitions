setwd("C:/Users/shashwat2014/Documents/Analytics Vidhya/Practice/Black Friday")
train<-read.csv("train.csv", na.strings = "")
test<-read.csv("test.csv", na.strings = "",row.names = 1)
source("C:/Users/shashwat2014/Documents/R/files/Functions.R")
source("C:/Users/shashwat2014/Documents/R/files/XGBoost.R")

library(dplyr)
group<-group_by(train, Gender)
a<-summarise(group, median_purchase=median(Purchase))
ggplot(a,aes(Gender, median_purchase))+geom_boxplot()

group<-group_by(train, Stay_In_Current_City_Years)
a<-summarise(group, mean_purchase=mean(Purchase))
ggplot(a,aes(Stay_In_Current_City_Years, mean_purchase))+geom_bar(stat="identity")

train$Marital_Status<-as.factor(train$Marital_Status)
#gender distribution

boxplot(train,train$Gender, train$Purchase)
#age wise
boxplot(train,train$Age, train$Purchase)

#marital status =0 only in test
#remove marital status from train

train$Product_Category_2[which(is.na(train$Product_Category_2))]<-0
train$Product_Category_3[which(is.na(train$Product_Category_3))]<-0
test$Product_Category_2[which(is.na(test$Product_Category_2))]<-0
test$Product_Category_3[which(is.na(test$Product_Category_3))]<-0
str(train)
train$Product_Category_1<-as.factor(train$Product_Category_1)
train$Product_Category_2<-as.factor(train$Product_Category_2)
train$Product_Category_3<-as.factor(train$Product_Category_3)
names(train)
train<-train[,-c(8)]
str(train)
train$Occupation<-as.factor(train$Occupation)

str(test)
test<-test[-c(1,2,12)]
test<-test[-c(6)]
test$Product_Category_1<-as.factor(test$Product_Category_1)
test$Product_Category_2<-as.factor(test$Product_Category_2)
test$Product_Category_3<-as.factor(test$Product_Category_3)
test$Occupation<-as.factor(test$Occupation)
str(test)
str(train)




library(h2o)
localh2o<-h2o.init(nthreads = -1)
train.h2o<-as.h2o(train)
y<-c(9)
x<-c(1:8,10,11)

gbmmodel<-h2o.gbm(x,y,training_frame = train.h2o, distribution = "gaussian", ntrees=500, max_depth = 6, learn_rate = 0.1, seed=1122)
test.h2o<-as.h2o(test)
h2o.performance(gbmmodel)
predict.gbm <- as.data.frame(h2o.predict(gbmmodel, test.h2o))
sub_gbm <- data.frame(User_ID = test1$User_ID, Product_ID = test1$Product_ID, Purchase = predict.gbm$predict)
write.csv(sub_gbm, file = "sub_gbm2.csv", row.names = F)

linear_model<-lm(Purchase~., data=train)
summary(linear_model)

train$Product_2_NA<-ifelse(train$Product_Category_2==0,0,1)
train$Product_3_NA<-ifelse(train$Product_Category_3==0,0,1)
test$Product_2_NA<-ifelse(test$Product_Category_2==0,0,1)
test$Product_3_NA<-ifelse(test$Product_Category_3==0,0,1)

linear_model<-lm(Purchase~., data=train)

train_new<-train[-c(1:7,11)]
wbcd_test_pred <- knn(train = train_new, test = test_new, cl = train2$Product_ID, k = 3) 

library(tidyr)
train_ID<-extract_numeric(train2$Product_ID)
#Will take hell lot of time doing the loop. Avoid until u get a GPU!
test_new$similar=0
for(i in seq(1:233599)){
  test_new$similar[i]=train_ID[which(test_new$Product_Category_1[i]==train_new$Product_Category_1 &test_new$Product_Category_2[i]==train_new$Product_Category_2 & test_new$Product_Category_3[i]==train_new$Product_Category_3)][1]
}

#Lets use K-means clustering after one-hot encoding
str(train_new)
names<-c(1,2,3)
train_new[,names]<-lapply(train_new[,names], factor)
library(dummies)
train_new<-dummy.data.frame(train_new,names = c("Product_Category_1"),sep="_")
train_new<-dummy.data.frame(train_new,names = c("Product_Category_2"),sep="_")
train_new<-dummy.data.frame(train_new,names = c("Product_Category_3"),sep="_")
train_new<-train_new[-c(19,20)]
product_clusters<-kmeans(train_new, 50)
cluster<-as.data.frame(product_clusters$cluster)
names(cluster)[1]<-"Target"


train_new<-cbind(train_new,cluster$Target)

test_new<-test[-c(1:5)]
str(test_new)
names(train_new)[55]<-"target"
test_new<-test_new[-c(4)]
names<-c(1,2,3)
test_new[,names]<-lapply(test_new[,names], factor)
str(test_new)
test_new<-dummy.data.frame(test_new,names = c("Product_Category_1"),sep="_")
test_new<-dummy.data.frame(test_new,names = c("Product_Category_2"),sep="_")
test_new<-dummy.data.frame(test_new,names = c("Product_Category_3"),sep="_")
train_new$`cluster$Target`<-as.factor(train_new$`cluster$Target`)
names(train_new)[53]<-"Target"
library(rpart)
tree<-rpart(Target~., data=train_new)
predicted_cluster<-as.data.frame(predict(tree, newdata = test_new))
head(predicted_cluster)
tr<-as.data.frame(t(predicted_cluster))
library(dplyr)
#Number of users
g<-group_by(train, User_ID)
s<-summarise(g,n())
a<-merge(train, s, by="User_ID")

#Number of products
g1<-group_by(train, Product_ID)
s1<-summarise(g1, n())
a<-merge(a, s1, by="Product_ID")

#average spending of user
mean_user<-summarise(g, mean=mean(Purchase))
head(mean_user)
a<-merge(a, mean_user, by="User_ID")
names(a)[12]<-"User_Count"
names(a)[13]<-"Product_Count"
names(a)[14]<-"User_Avg_Spending"

#Average purchase of product
mean_product<-summarise(g1, mean=mean(Purchase))
a<-merge(a, mean_product, by="Product_ID")
names(a)[15]<-"Product_Avg_Purchase"

b<-a[-c(1,2)]

b.h2o<-as.h2o(b)
y<-c(9)
x<-c(1:8,10:13)

gbmmodel<-h2o.gbm(x,y,training_frame = b.h2o, distribution = "gaussian", ntrees=200, max_depth = 6, stopping_metric = "MSE", learn_rate = 0.1, seed=1122)
test.h2o<-as.h2o(test)
h2o.performance(gbmmodel)
predict.gbm <- as.data.frame(h2o.predict(gbmmodel, test.h2o))

linear<-lm(Purchase~., data=b)
y<-b$Purchase


test2<-test2[-c(1,2,10,14)]
#Number of users
g_t<-group_by(test2, User_ID)
s_t<-summarise(g_t,n())
a_t<-merge(test2, s_t, by="User_ID")
names(a_t)[11]<-"User_Count"
#Number of products
g1_t<-group_by(test2, Product_ID)
s1_t<-summarise(g1_t, n())
a_t<-merge(a_t, s1_t, by="Product_ID")

#average spending of user
a_t<-merge(a_t, mean_user, by="User_ID")
a_t<-merge(x=a_t, y=mean_product, by="Product_ID", all.x = TRUE)
names(a_t)[12]<-"Product_Count"
names(a_t)[13]<-"User_Avg_Spending"
names(a_t)[14]<-"Product_Avg_Purchase"

a_t$Product_Category_2[which(is.na(a_t$Product_Category_2))]<-0
a_t$Product_Category_3[which(is.na(a_t$Product_Category_3))]<-0
b_t<-a_t[-c(1,2)]
b_t$Occupation<-as.factor(b_t$Occupation)
names<-c(6,7,8)
b_t[,names]<-sapply(b_t[,names], factor)
b_t.h2o<-as.h2o(b_t)
predict.gbm <- as.data.frame(h2o.predict(gbmmodel, b_t.h2o))
sub_gbm <- data.frame(User_ID = test2$User_ID, Product_ID = test2$Product_ID, Purchase = predict.gbm$predict)
write.csv(sub_gbm, file = "sub_gbm2.csv", row.names = F)

b<-b[-c(9)]
model_xgb_1 <- XGBoost(b,y,b_t,cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,colsample_bytree=0.5,seed=235,metric="rmse",importance=1)
