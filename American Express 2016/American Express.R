setwd("C:/Users/shashwat2014/Documents/Data Analytics and Consulting/American Express")
train<-read.csv("training_Dataset.csv", na.strings = "")
colSums(is.na(train))
train<-read.csv("Leaderboard_Dataset.csv", na.strings = "")
str(train)
library(ggplot2)
library(gmodels)
CrossTable(train$mvar27, train$actual_vote)
ggplot(train, aes(train$actual_vote, train$Centaur))+geom_boxplot()
mean(train$mvar5[which(train$actual_vote=="Tokugawa")])

library(nnet)
reg<-multinom(actual_vote~., data=train)
norm<-function(x){
  y <- x ^ 0.1
  return(y)
}
train$mvar1<-train$mvar1^0.1
names<-c(4:27)
train[,names]<-lapply(train[,names], norm)

order<-function(x){
  y<-(x-min(x))/(max(x)-min(x))
  return(y)
}

names<-c(3:27)
train[,names]<-lapply(train[,names], order)

train$Centaur<-(train$mvar1+train$mvar6+train$mvar11+train$mvar16+train$mvar21)/5
train$Ebony<-(train$mvar2+train$mvar7+train$mvar12+train$mvar17+train$mvar22)/5
train$Tokugawa<-(train$mvar3+train$mvar8+train$mvar13+train$mvar18+train$mvar23)/5
train$Odyssey<-(train$mvar4+train$mvar9+train$mvar14+train$mvar19+train$mvar24)/5
train$Cosmos<-(train$mvar5+train$mvar10+train$mvar15+train$mvar20+train$mvar25)/5

hist(train$Centaur)
mean(train$Centaur[which(train$party_voted_past=="Centaur" & train$actual_vote!="Centaur")])
mean(train$Centaur[which(train$party_voted_past=="Centaur")])

table(train$mvar28[which(train$party_voted_past!=train$actual_vote)])
#married people tend to change their vote mvar28

table(train$mvar29[which(train$party_voted_past!=train$actual_vote)])
#those who dont have a home change their vote mvar29

table(train$mvar30[which(train$party_voted_past!=train$actual_vote)])
#more educated ones change more

hist(train$mvar31[which(train$party_voted_past!=train$actual_vote)])
#more newspapers imply change

train$mvar32<-as.character(train$mvar32)
train$mvar33<-as.character(train$mvar33)

train$changeplace<-ifelse(train$mvar32!=train$mvar33, 1, 0)
train$changevote<-ifelse(train$party_voted_past==train$actual_vote, 0, 1)

train_new<-train[,c(2,28:43)]
summary(train_new)
class(train_new$mvar26)
str(train_new)


names<-c(3:27)
test[,names]<-lapply(test[,names], norm)

order<-function(x){
  y<-(x-min(x))/(max(x)-min(x))
  return(y)
}

names<-c(3:27)
test[,names]<-lapply(test[,names], order)

test$Centaur<-(test$mvar1+test$mvar6+test$mvar11+test$mvar16+test$mvar21)/5
test$Ebony<-(test$mvar2+test$mvar7+test$mvar12+test$mvar17+test$mvar22)/5
test$Tokugawa<-(test$mvar3+test$mvar8+test$mvar13+test$mvar18+test$mvar23)/5
test$Odyssey<-(test$mvar4+test$mvar9+test$mvar14+test$mvar19+test$mvar24)/5
test$Cosmos<-(test$mvar5+test$mvar10+test$mvar15+test$mvar20+test$mvar25)/5

test$mvar32<-as.character(test$mvar32)
test$mvar33<-as.character(test$mvar33)

test$changeplace<-ifelse(test$mvar32!=test$mvar33, 1, 0)
train_2<-train_new[, c(1:16)]

library(plyr)
train_new$mvar27<-mapvalues(train_new$mvar27, from = c("18-24", "25-35", "36-45", "46-55", "55+" ), to = c("0", "1", "2", "3", "4"))

train_new$mvar28[which(is.na(train_new$mvar28))]<-1
train_new$mvar29[which(is.na(train_new$mvar29))]<-0
train_new$mvar30[which(is.na(train_new$mvar30))]<-"Primary"
train_new$mvar28<-as.factor(train_new$mvar28)
train_new$mvar29<-as.factor(train_new$mvar29)

train_new$mvar26<-ifelse(train_new$mvar26 %in% c(1,2), "small", train_new$mvar26)
train_new$mvar26<-ifelse(train_new$mvar26 %in% c(3,4), "med", train_new$mvar26)
train_new$mvar26<-ifelse(train_new$mvar26 %in% c(5,6,7,8), "large", train_new$mvar26)
train_new$mvar26<-as.factor(train_new$mvar26)

train_new$mvar31<-ifelse(train_new$mvar31 %in% c(0,1,2), "low", train_new$mvar31)
train_new$mvar31<-ifelse(train_new$mvar31 %in% c(3,4), "lmed", train_new$mvar31)
train_new$mvar31<-ifelse(train_new$mvar31 %in% c(5,6,7), "hmed", train_new$mvar31)
train_new$mvar31<-ifelse(train_new$mvar31 %in% c(8,9,10,11), "high", train_new$mvar31)
train_new$mvar31<-as.factor(train_new$mvar31)
train_new$changeplace<-as.factor(train_new$changeplace)
train_new$changevote<-as.factor(train_new$changevote)


train_new<-train_new[,-c(8,9)]
train_2<-train_new[,-c(15)]
library(randomForest)
rf<-randomForest(actual_vote~., data=train_2, mtry=3, ntree=200)
test_new<-test[,c(2,28:41)]
names(test_new)
names(train_new)
test_new<-test_new[,-c(8,9)]
test_new$mvar28[which(is.na(test_new$mvar28))]<-1
test_new$mvar29[which(is.na(test_new$mvar29))]<-0
test_new$mvar30[which(is.na(test_new$mvar30))]<-"Primary"
test_new$mvar27<-mapvalues(test_new$mvar27, from = c("18-24", "25-35", "36-45", "46-55", "55+" ), to = c("0", "1", "2", "3", "4"))

pred<-predict(rf, test_new)
write.csv(data.frame(test$citizen_id, pred), "Focus_IITGuwahati_2.csv")

change<-train_new[,c(1:7,14,15)]
str(change)
rf_change<-randomForest(changevote~., data=change, mtry=3, ntree=200)

test_new$mvar28<-as.factor(test_new$mvar28)
test_new$mvar29<-as.factor(test_new$mvar29)

test_new$mvar26<-ifelse(test_new$mvar26 %in% c(1,2), "small", test_new$mvar26)
test_new$mvar26<-ifelse(test_new$mvar26 %in% c(3,4), "med", test_new$mvar26)
test_new$mvar26<-ifelse(test_new$mvar26 %in% c(5,6,7,8), "large", test_new$mvar26)
test_new$mvar26<-as.factor(test_new$mvar26)

test_new$mvar31<-ifelse(test_new$mvar31 %in% c(0,1,2), "low", test_new$mvar31)
test_new$mvar31<-ifelse(test_new$mvar31 %in% c(3,4), "lmed", test_new$mvar31)
test_new$mvar31<-ifelse(test_new$mvar31 %in% c(5,6,7), "hmed", test_new$mvar31)
test_new$mvar31<-ifelse(test_new$mvar31 %in% c(8,9,10,11), "high", test_new$mvar31)
test_new$mvar31<-as.factor(test_new$mvar31)
test_new$changeplace<-as.factor(test_new$changeplace)


pred<-predict(rf_change, test_new)
pred_train<-predict(rf_change, change)
table(change$changevote,pred_train)

#poor result by rf. Go by glm
log<-glm(changevote~., data = change, family = "binomial")
train_change<-predict(log, change, type = "response")
outcome<-ifelse(train_change>0.43, 1, 0)
table(change$changevote,outcome)
#poorer result by glm

library(gbm)
library(xgboost)
sparse_matrix <- sparse.model.matrix(changevote ~ .-1, data = change)

xgb<-xgboost(data = sparse_matrix, 
             label=data.matrix(change[,c(9)]),
             eta=0.1,
             max_depth=6,
             nround=100,
             subsample=0.5,
             seed=1,
             eval_metric="logloss",
             objective="binary:logistic",
             colsample_bytree=0.4,
             min_child_weight=0.16,
             verbose=2)
test_change<-test_new[,c(1:7,13)]
sparse_test<-sparse.model.matrix( ~ .-1, data = test_change)
y_pred <- predict(xgb, sparse_test)
summary(y_pred)
outcome<-ifelse(y_pred>0.4, 1, 0)
table(change$changevote,outcome)
test_change$changevote<-outcome
names(train_new)
str(train_new)
rf1<-randomForest(actual_vote~., data=train_new, mtry=4, ntree=300)
test_new$changevote<-outcome
test_new$changevote<-as.factor(test_new$changevote)
final_pred<-predict(rf1, test_new)
write.csv(data.frame(test$citizen_id, final_pred), "Focus_IITGuwahati_3.csv")


train_xg<-train_new[-15]
names(train_xg)
xg_train_sparse<-sparse.model.matrix( actual_vote~ .-1, data = train_xg)
train_xg$actual_vote<-revalue(train_xg$actual_vote, c("Centaur"=0, "Cosmos"=1, "Ebony"=2, "Odyssey"=3, "Tokugawa"=4))
xgb<-xgboost(data = xg_train_sparse, 
             label=data.matrix(train_xg[,c(8)]),
             eta=0.1,
             max_depth=6,
             nround=100,
             subsample=0.5,
             seed=1,
             eval_metric="mlogloss",
             objective="multi:softmax", 
             num_class=5,
             colsample_bytree=0.4,
             min_child_weight=0.16,
             verbose=2)
test_new<-test_new[-14]
sparse_xg_test<-sparse.model.matrix( ~ .-1, data = test_new)
y_pred <- predict(xgb, xg_train_sparse)
y_pred<-as.factor(y_pred)
y_pred<-revalue(y_pred, c("0"="Centaur", "1"="Cosmos", "2"="Ebony", "3"="Odyssey", "4"="Tokugawa"))
table(y_pred,train$actual_vote)

write.csv(y_pred, "Focus_IITGuwahati_4.csv")

test_2<-test[,-c(3:27)]
test_2<-test_2[-1]
test_2<-test_2[-c(8,9)]
test_2$mvar27<-mapvalues(test_2$mvar27, from = c("18-24", "25-35", "36-45", "46-55", "55+" ), to = c("0", "1", "2", "3", "4"))

names<-c(6,18)
train_2[,names]<-lapply(train_2[,names], order)

test_2[,names]<-lapply(test_2[,names], order)


#convert train_2 and test_2 into sparse matrices
library(dummies)
train_2<-dummy.data.frame(train_2,names = c("party_voted_past"),sep="_")
train_2<-dummy.data.frame(train_2,names = c("mvar27"),sep="_")
train_2<-dummy.data.frame(train_2,names = c("mvar30"),sep="_")
train_3<-train_2[-19]

test_2<-dummy.data.frame(test_2,names = c("party_voted_past"),sep="_")
test_2<-dummy.data.frame(test_2,names = c("mvar27"),sep="_")
test_2<-dummy.data.frame(test_2,names = c("mvar30"),sep="_")


label<-train_2$actual_vote
label<-revalue(label, c("Centaur"=0, "Cosmos"=1, "Ebony"=2, "Odyssey"=3, "Tokugawa"=4))
xgb<-xgboost(data = data.matrix(train_3), 
             label=data.matrix(label),
             eta=0.1,
             max_depth=6,
             nround=100,
             subsample=0.5,
             seed=1,
             eval_metric="mlogloss",
             objective="multi:softmax", 
             num_class=5,
             colsample_bytree=0.4,
             min_child_weight=0.16,
             verbose=2)
y_pred <- predict(xgb, mat_test)
y_pred<-as.factor(y_pred)
y_pred<-revalue(y_pred, c("0"="Centaur", "1"="Cosmos", "2"="Ebony", "3"="Odyssey", "4"="Tokugawa"))
write.csv(data.frame(test$citizen_id, y_pred), "Focus_IITGuwahati_5.csv", row.names=FALSE, col.names = FALSE)
