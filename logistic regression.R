##Logistic Regression
#I hope to know what factors have impact on whether customers will dedeem the membership card.
#First log in the data
hotel.df<-read.csv("hotelloyaltydata.csv")
View(hotel.df)
select.var <-c(6,7,8,9,10,11,12)
train.index <- sample(1:nrow(hotel.df), 0.7*nrow(hotel.df))
train.df <- hotel.df[train.index, select.var]
valid.df <- hotel.df[-train.index,select.var]

View(train.df)
#Regression
model <- glm(Reedemer~.,family=binomial,data=train.df)
summary(model)

#see the confusionmatrix
library(caret)
predict <- predict(model,newdata=train.df,type='response')
table(train.df$Reedemer,predict>0.5)
confusionMatrix(data = as.numeric(predict>0.5), reference = train.df$Reedemer)

#see the R square value of the model
install.packages('rms')
library(rms)
require(rms)
model <- lrm(Reedemer~., data=train.df)
print(model)

#valid the model
library (forecast)
hotel.pred <-predict(model,valid.df)
accuracy(hotel.pred,valid.df$Reedemer)

#Plot the regression with variable "Spend"
library(ggplot2)
ggplot(train.df, aes(x=Spend , y=Reedemer)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

##Antother example of Logistic Regreassion:delay of flight
flight.df<-read.csv("FlightDelays.csv")

#check out what kinds of variables I have
str(flight.df) #we see that day_week is integer, we need to transfer that into categorical variable
            #And  
View(flight.df)


#transform
flight.df$DAY_WEEK <- factor(flight.df$DAY_WEEK,levels = c(1:7), labels=c("Mon",'Tue',"Wed","Thu","Fri","Sat","Sun"))
flight.df$CRS_DEP_TIME <- factor(round(flight.df$CRS_DEP_TIME/100))
View(flight.df$CRS_DEP_TIME )
#for here, weather's category only has 2 choices, so it doesn't influence our regression here.

#set the base level for categorical variables
flight.df$ORIGIN <- relevel(flight.df$ORIGIN, ref = "IAD")
flight.df$DEST <- relevel(flight.df$DEST, ref = "LGA")
flight.df$CARRIER <- relevel(flight.df$CARRIER, ref = "US")
flight.df$DAY_WEEK <- relevel(flight.df$DAY_WEEK, ref = "Mon")

#transform the factor as numeric for prediction
flight.df$isDelay <- as.numeric(flight.df$Flight.Status == "delayed")


#partition the data
set.seed(5)
View(flight.df)
selected.var <- c(10, 1, 8, 4, 2, 9, 14)
train.index <- sample(1:nrow(flight.df), 0.7*nrow(flight.df))
train.df <- flight.df[train.index, selected.var]
valid.df <- flight.df[-train.index,selected.var]

#regress
View(train.df)
logit.reg <- glm(isDelay ~., data = train.df, family = "binomial")
summary(logit.reg)
#INTERPRETION:compare to base level, if the flight departs on Tuesday, the probabilty of delay is decreased.

#predict
logit.reg.pred <-predict(logit.reg,valid.df,type="response")
#Generate the outcome by comparing predicted probability with the cutoff probability
#cutoff value 0.5
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
View(pred)

library(caret)
confusionMatrix(factor(pred), factor(valid.df$isDelay))

#this only prints out the accuracy
confusionMatrix(factor(pred), factor(valid.df$isDelay))$overall[1]


#we use factor(valid.df$isDelay) instead of valid.df$isDelay here, because valid.df$isDelay is numeric

#let's see the predictive power of this algorithm by comparing the first 5 actual and predicted records
data.frame(actual = valid.df$isDelay[1:5], predicted = pred[1:5])

# find out training accuracy
train.pred <- ifelse(logit.reg$fitted.values > 0.5, 1, 0)
confusionMatrix(factor(train.pred), factor(train.df$isDelay))

#if the difference between training accuracy and valid accuracy is too much, then we should consider that the partition is uneven


View(airbnb)

##Decision Tree
default.hotel <-rpart(Reedemer~., data=train.df,method = "class")
prp(default.hotel,type=1,extra=1,varlen=6)
default.hotel.pred <-predict(default.hotel,valid.df,type="class")
confusionMatrix(default.hotel.pred,factor(valid.df$Reedemer))

