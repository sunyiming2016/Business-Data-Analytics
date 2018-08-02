##This project tends to evaluate whether the new 500 customers are credit trustworthy or not. 
##In order to find the answer, I ran a logistic regression model by using data from the credit training dataset for prediction.
## Before I built the model, I tested the correlation of numeric variables of the data set, and found no data were highly correlated.
##I cleaned the dataset by drop variables Occupation& Concurent Occupancy for lacking variability. And dropped variable duration in current address for lacking values.
# Then I run the data with partitioned 70% data from credit training dataset, and used the other 30% for validation.

credit.df<-read.csv("credit-data-training.csv")
View(credit.df)
str(credit.df)

#test correlation
selected.var <- c(3,6,9,11,12,13,15,17,18,19,20)
test.df<-credit.df[,selected.var]
library(gplots)
heatmap.2(cor(test.df), Rowv = FALSE, Colv = FALSE,dendrogram = "none",
          cellnote = round(cor(test.df),2))

#identify the missing values
heatmap(1*is.na(test.df),Rowv = NA,Colv = NA)# So we drop the duration in current address column

#Guarantor also has too little variability
plot(credit.df$Guarantors)
plot(credit.df$Foreign.Worker)
credit.df$Telephone <- as.numeric(credit.df$Telephone,1,2)
str(credit.df$Telephone)
plot(credit.df$Telephone)

#I realized the variable Occupation& Concurent Occupancy are short of variablility, so I drop them
install.packages("dplyr")
library(dplyr)
credit1.df=select(credit.df,-11,-17,-14,-10,-20)
View(credit1.df)
str(credit.df$Age.years)
credit.df$Age.years[is.na(credit.df$Age.years)] =median(credit.df$Age.years, na.rm=TRUE)
str(credit.df$Age.years)

#ready to regress
set.seed(5)
train.index <- sample(1:nrow(credit1.df), 0.7*nrow(credit1.df))
train.df <- credit1.df[train.index, ]
valid.df <- credit1.df[-train.index,]

logit.reg <- glm(Credit.Application.Result ~., data = train.df, family = "binomial")
summary(logit.reg) # 6 variables are statiscally significant

#see the R square value of the model
install.packages('rms')
library(rms)
require(rms)
model <- lrm(Credit.Application.Result ~., data=train.df)
print(model)# we see that the regression result is not satisfactory

#validation
logit.reg.pred <-predict(logit.reg,valid.df,type="response")
pred<-ifelse(logit.reg.pred>0.5,"Non-Creditworthy","Creditworthy")
library(caret)
confusionMatrix(factor(pred), factor(valid.df$Credit.Application.Result)) #this shows that this model is not valid here

#prediction
prediction.df<-read.csv("Copy of customers-to-score.csv")
logit.reg.pred <-predict(logit.reg,prediction.df,type="response")
pred1<-ifelse(logit.reg.pred>0.5,"Creditworthy","Non-Creditworthy")
View(pred1)

