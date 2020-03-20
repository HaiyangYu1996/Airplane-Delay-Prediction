data1 <- read.csv("FlightDelays.csv")
head(data1)
str(data1)
options(scipen = 999)
library(gains)
library(pROC)
#data processing
data1$weather <- factor(data1$weather,levels=c(0,1),labels=c("no","yes"))
data1$dayweek <- factor(data1$dayweek,levels=c(1:7),labels = c("Mon","Tue","Wed","Thur",
                                                               "Fri","Sat","Sun"))
data1$daymonth <- factor(data1$daymonth,levels=c(1:31))
#data1$didf_schedtime <- abs(data1$deptime-data1$schedtime)
#data1$dif_within15 <- (data1$didf_schedtime > 15,0,1)
#data1$deptime <- factor(round(data1$deptime/100))
data1$schedtime <- factor(round(data1$schedtime/100))

#barplot - data visualization
par(mfcol = c(2,4))
barplot(aggregate(data1$delay == "delayed",by = list(data1$dayweek),mean,rm.na = T)[,2],
        xlab = "Day of Week",ylab = "Average Delay",names.arg = c("Mon","Tue","Wed","Thur",
                                                                  "Fri","Sat","Sun"))
barplot(aggregate(data1$delay == "delayed",by = list(data1$dest),mean,rm.na = T)[,2],
        xlab = "Destination",ylab = "Average Delay",names.arg = c("EWR","JFK","LGA"))
barplot(aggregate(data1$delay == "delayed",by = list(data1$carrier),mean,rm.na = T)[,2],
        xlab = "Carrier",ylab = "Average Delay",names.arg = c("CO","DH","DL","MQ","OH","RU",
                                                              "UA","US"))
barplot(aggregate(data1$delay == "delayed",by = list(data1$weather),mean,rm.na = T)[,2],
        xlab = "Weather",ylab = "Average Delay",names.arg = c(0,1))
barplot(aggregate(data1$delay == "delayed",by = list(data1$distance),mean,rm.na = T)[,2],
        xlab = "Distance",ylab = "Average Delay",names.arg = c("169","184","199","213","214","228","229"))
barplot(aggregate(data1$delay == "delayed",by = list(data1$daymonth),mean,rm.na = T)[,2],
        xlab = "Day of Month",ylab = "Average Delay",names.arg = c(1:31))
barplot(aggregate(data1$delay == "delayed",by = list(data1$origin),mean,rm.na = T)[,2],
        xlab = "Origin",ylab = "Average Delay",names.arg = c("BWI","DCA","IAD"))
barplot(aggregate(data1$delay == "delayed",by = list(data1$schedtime),mean,rm.na = T)[,2],
        xlab = "Schedtime",ylab = "Average Delay",names.arg = c(6:21))

#remove "distance","deptime","dest","date","flight_number","origin","tailnu"
data1 <- data1[,-c(3,4,5,6,7,8,12)]
#create 1/0 variable for delay
data1$isDelay <- 1*(data1$delay == "delayed")
data1 <- data1[,-c(6)]

#split dataset
set.seed(6)
train.index <- sample(c(1:dim(data1)[1]),dim(data1)[1]*0.6)
train.df <- data1[train.index,]
valid.df <- data1[-train.index,]

#model1
glmmodel_1 <- glm(isDelay~.,binomial,train.df)
summary(glmmodel_1)

#delete daymonth
train.df2 <- train.df[,-c(5)]
valid.df2 <- valid.df[,-c(5)]

#model2_without daymonth
glmmodel_2 <- glm(isDelay~.,binomial,train.df2)
summary(glmmodel_2)

#Confusion matrix
pred2 <- predict(glmmodel_2,valid.df2)
y_pred2 <- ifelse(pred>0.5,1,0)
#gain <- gains(valid.df2$delay,glmmodel_2$fitted.values,groups = 100)
CM_model_2 <- table(y_pred2,valid.df2$isDelay)
CM_model_2

#grouping variables
data2 <- data1
data1$Morning <- data1$schedtime %in% c(6,7,8,9)
data1$Noon <- data1$schedtime %in% c(10,11,12,13)
data1$Afternoon <- data1$schedtime %in% c(14,15,16,17,18)
#data1$Evening <- data1$schedtime %in% c(19,20,21) (be careful the dummmy variable trap)
data1$Mon_Sun <- data1$dayweek %in% c("Mon","Sun")
data1$carrier_co_mq_dh_ru <- data1$carrier %in% c("CO","MQ","DH","RU")

#split dataset
set.seed(6)
train.index <- sample(c(1:dim(data1)[1]),dim(data1)[1]*0.6)
train.df3 <- data1[train.index,]
train.df3 <- train.df3[,-c(1,2,4,5)]
valid.df3 <- data1[-train.index,]
valid.df3 <- valid.df3[,-c(1,2,4,5)]

#model3
glmmodel_3 <- glm(isDelay~.,binomial,train.df3)
summary(glmmodel_3)

#deviance testing
Null_deviance = glmmodel_3$null.deviance
Null_df = glmmodel_3$df.null
Residual_deviance = glmmodel_3$deviance
Residual_df = glmmodel_3$df.residual
1-pchisq(Null_deviance,Null_df)
1-pchisq(Residual_deviance,Residual_df)

#confusion matrix
pred3 <- predict(glmmodel_3,valid.df3)
y_pred3 <- ifelse(pred3>0.5,1,0)
#gain <- gains(valid.df2$delay,glmmodel_2$fitted.values,groups = 100)
CM_model_3 <- table(ifelse(pred3>0.5,1,0),valid.df3$isDelay)
CM_model_3

#ROC
par(mfcol = c(1,1))
modelroc <- roc(valid.df3$isDelay,y_pred3)
plot(modelroc,print.auc = TRUE,auc.polygon = TRUE,
     grid = c(0.1,0.2),grid.col = c("green","red"),
     max.auc.polygon = TRUE,auc.polygon.col = "skyblue",
     print.thres = TRUE)

#futher grouping - "peaktime"
data2$peaktime <- data1$schedtime %in% c(15,16,17,18,19,20,21)
data2$Mon_Sun <- data1$dayweek %in% c("Mon","Sun")
data2$carrier_co_mq_dh_ru <- data1$carrier %in% c("CO","MQ","DH","RU")
data2 <- data2[,-c(1,2,4,5)]
set.seed(6)
train.index <- sample(c(1:dim(data2)[1]),dim(data2)[1]*0.6)
train.df4 <- data2[train.index,]
valid.df4 <- data2[-train.index,]

#model4-peaktime and prediction
glmmodel_4 <- glm(isDelay~.,binomial,train.df4)
summary(glmmodel_4)
pred4 <- predict(glmmodel_4,valid.df4,type = "response")
predtrain <- predict(glmmodel_4,train.df4,type = "response")
y_pred4 <- ifelse(pred4>0.3,1,0)


#Null_deviance
Null_deviance = glmmodel_4$null.deviance
Null_df = glmmodel_4$df.null
1-pchisq(Null_deviance,Null_df) 

#Residual_deviance
Residual_deviance = glmmodel_4$deviance
Residual_df = glmmodel_4$df.residual
1-pchisq(Residual_deviance,Residual_df)

#the drop
1-pchisq(Null_deviance-Residual_deviance,Null_df-Residual_df)

#confusion matrix
y_pred4 <- factor(y_pred4)
valid.df4$isDelay <- factor(valid.df4$isDelay)
confusionMatrix(y_pred4,valid.df4$isDelay)

#ROC
par(mfcol = c(1,1))
library(pROC)
modelroc <- roc(valid.df4$isDelay,pred4)
plot(modelroc,print.auc = TRUE,auc.polygon = TRUE,
     grid = c(0.1,0.2),grid.col = c("green","red"),
     max.auc.polygon = TRUE,auc.polygon.col = "skyblue",
     print.thres = TRUE)

#residuals plot
model.residuals <- residuals(glmmodel_4)
summary(model.residuals)
# [sign] positive underpredicted/ negative overpredicted (yi/ni - p^i)
plot(model.residuals)

#lift chart
#library(gains)
gain <- gains(as.numeric(as.character(valid.df4$isDelay)),pred4)
gaintrain <- gains(as.numeric(as.character(train.df4$isDelay)),predtrain)
#plot(c(0,gain$cume.pct.of.total*sum(valid.df4$isDelay))~
#             c(0,gain$cume.obs),
#     xlab="# cases",ylab="Cumulative",main="",type = "l")
#lines(c(0,sum(valid.df4$isDelay))~c(0,dim(valid.df4)[1]),lty=2)

#decile-wise lift chart-valid
heights <- gain$mean.resp/mean(as.numeric(as.character(valid.df4$isDelay)))
midpoints <- barplot(heights,names.arg = gain$depth,ylim = c(0,9),
                     xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart-valid")
#add labels to colunms
text(midpoints,heights+0.5,labels = round(heights,1),cex = 0.8)

#decile-wise lift chart-train-train
heightstrain <- gaintrain$mean.resp/mean(as.numeric(as.character(train.df4$isDelay)))
midpointstrain <- barplot(heightstrain,names.arg = gaintrain$depth,ylim = c(0,9),
                     xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart-train")
#add labels to colunms
text(midpointstrain,heightstrain+0.5,labels = round(heightstrain,1),cex = 0.8)

#neural net
#dummy variables for training set
train.df5 <- train.df4
dummyweather <- model.matrix(~weather,train.df5)
train.df5 <- cbind(dummyweather,train.df5)

train.df5$peaktime <- factor(train.df5$peaktime)
dummypeaktime <- model.matrix(~peaktime,train.df5)
train.df5 <- cbind(dummypeaktime,train.df5)

train.df5$Mon_Sun <- factor(train.df5$Mon_Sun)
dummyMon_Sun <- model.matrix(~Mon_Sun,train.df5)
train.df5 <- cbind(dummyMon_Sun,train.df5)

train.df5$carrier_co_mq_dh_ru <- factor(train.df5$carrier_co_mq_dh_ru)
dummycarrier_co_mq_dh_ru <- model.matrix(~carrier_co_mq_dh_ru,train.df5)
train.df5 <- cbind(dummycarrier_co_mq_dh_ru,train.df5)

train.df5 <- train.df5[,-c(1,3,5,7,9,11,12,13)]

train.df5$isDelay <- factor(train.df5$isDelay)
train.df5 <- cbind(train.df5,class.ind(train.df5$isDelay))
train.df5 <- train.df5[,-c(5)]

#dummy variables for valid set
valid.df5 <- valid.df4
dummyweather <- model.matrix(~weather,valid.df5)
valid.df5 <- cbind(dummyweather,valid.df5)

valid.df5$peaktime <- factor(valid.df5$peaktime)
dummypeaktime <- model.matrix(~peaktime,valid.df5)
valid.df5 <- cbind(dummypeaktime,valid.df5)

valid.df5$Mon_Sun <- factor(valid.df5$Mon_Sun)
dummyMon_Sun <- model.matrix(~Mon_Sun,valid.df5)
valid.df5 <- cbind(dummyMon_Sun,valid.df5)

valid.df5$carrier_co_mq_dh_ru <- factor(valid.df5$carrier_co_mq_dh_ru)
dummycarrier_co_mq_dh_ru <- model.matrix(~carrier_co_mq_dh_ru,valid.df5)
valid.df5 <- cbind(dummycarrier_co_mq_dh_ru,valid.df5)

valid.df5 <- valid.df5[,-c(1,3,5,7,9,11,12,13)]

valid.df5$isDelay <- factor(valid.df5$isDelay)
valid.df5 <- cbind(valid.df5,class.ind(valid.df5$isDelay))
valid.df5 <- valid.df5[,-c(5)]

#nn with nodes = 3
nn <- neuralnet(`0` + `1` ~., data = train.df5,hidden = 3,threshold = 0.1)
plot(nn)

#confusionMatrix
train.pred = compute(nn,train.df5)
train.class = apply(train.pred$net.result,1,which.max)-1
train.class <- factor(train.class)
train.df4$isDelay <- factor(train.df4$isDelay)
confusionMatrix(train.class,train.df4$isDelay)

valid.pred = compute(nn,valid.df5)
valid.class = apply(valid.pred$net.result,1,which.max)-1
valid.class <- factor(valid.class)
confusionMatrix(valid.class,valid.df4$isDelay)

valid.pred = compute(nn,valid.df5)

#decile-wise lift chart
library(gains)
gain_train <- gains(as.numeric(as.character(train.df4$isDelay)),train.pred$net.result[,2])
gain_valid <- gains(as.numeric(as.character(valid.df4$isDelay)),valid.pred$net.result[,2])
heights_train <- gain_train$mean.resp/mean(as.numeric(as.character(train.df4$isDelay)))
heights_valid <- gain_valid$mean.resp/mean(as.numeric(as.character(valid.df4$isDelay)))
midpoints1 <- barplot(heights_train,names.arg = gain_train$depth,ylim = c(0,9),
                      xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart_train_node3")
#add labels to colunms
text(midpoints1,heights_train+0.5,labels = round(heights_train,1),cex = 0.8)

midpoints2 <- barplot(heights_valid,names.arg = gain_valid$depth,ylim = c(0,9),
                      xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart_valid_node3")
#add labels to colunms
text(midpoints2,heights_valid+0.5,labels = round(heights_valid,1),cex = 0.8)

#pROC
library(pROC)
modelroc <- roc(valid.df4$isDelay,valid.pred$net.result[,2])
plot(modelroc,print.auc = TRUE,auc.polygon = TRUE,
     grid = c(0.1,0.2),grid.col = c("green","red"),
     max.auc.polygon = TRUE,auc.polygon.col = "skyblue",
     print.thres = TRUE)

#naive bayes
#data processing
library(e1071)
train.df4$peaktime <- factor(train.df4$peaktime)
train.df4$Mon_Sun <- factor(train.df4$Mon_Sun)
train.df4$carrier_co_mq_dh_ru <- factor(train.df4$carrier_co_mq_dh_ru)

valid.df4$peaktime <- factor(valid.df4$peaktime)
valid.df4$Mon_Sun <- factor(valid.df4$Mon_Sun)
valid.df4$carrier_co_mq_dh_ru <- factor(valid.df4$carrier_co_mq_dh_ru)

#naiveBayes model
delays.nb <- naiveBayes(isDelay~.,data = train.df4)
delays.nb
#predict probabilities
pred.prob_train <- predict(delays.nb,newdata = train.df4,type = "raw")
pred.prob_valid <- predict(delays.nb,newdata = valid.df4,type = "raw")

#predict class membership
pred.class_train <- predict(delays.nb,newdata = train.df4)
pred.class_valid <- predict(delays.nb,newdata = valid.df4)

#confusion matrix
#training
confusionMatrix(pred.class_train,train.df4$isDelay)
#valid
confusionMatrix(pred.class_valid,valid.df4$isDelay)

#decile-wise lift chart
gain_train_nb <- gains(as.numeric(as.character(train.df4$isDelay)),pred.prob_train[,2])
gain_valid_nb <- gains(as.numeric(as.character(valid.df4$isDelay)),pred.prob_valid[,2])
heights_train_nb <- gain_train_nb$mean.resp/mean(as.numeric(as.character(train.df4$isDelay)))
heights_valid_nb <- gain_valid_nb$mean.resp/mean(as.numeric(as.character(valid.df4$isDelay)))
midpoints_nb1 <- barplot(heights_train_nb,names.arg = gain_train_nb$depth,ylim = c(0,9),
                      xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart_train_nb")
text(midpoints_nb1,heights_train_nb+0.5,labels = round(heights_train_nb,1),cex = 0.8)
midpoints_nb2 <- barplot(heights_valid_nb,names.arg = gain_valid_nb$depth,ylim = c(0,9),
                      xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart_valid_nb")
text(midpoints_nb2,heights_valid_nb+0.5,labels = round(heights_valid_nb,1),cex = 0.8)

#ROC
modelroc <- roc(valid.df4$isDelay,pred.prob_valid[,2])
plot(modelroc,print.auc = TRUE,auc.polygon = TRUE,
     grid = c(0.1,0.2),grid.col = c("green","red"),
     max.auc.polygon = TRUE,auc.polygon.col = "skyblue",
     print.thres = TRUE)
