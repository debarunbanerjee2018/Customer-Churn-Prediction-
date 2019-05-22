
###Required Libraries
library(knitr)
library(readr)
install.packages('DataExplorer')
library(DataExplorer)
library(corrplot)

##Setting the directory
setwd("Z:/Edwisor Projects(24-02-2018)") #setting our working directory
Churn = read.csv("Test_data.csv", header = T, stringsAsFactors = FALSE)# reading data from csv file
names(Churn)

str(Churn)

##Doing the exploratory analysis
text(barplot(table(Churn$Churn),col=c('green','red'),main='Bar plot of Churn')
     ,0,table(Churn$Churn),cex=2,pos=3)

anyNA(Churn)

##Mapping the correlation
corrplot(cor(Churn[sapply(Churn, is.numeric)]))

Churn$state <- NULL
Churn$phone.number <- NULL
Churn$area.code <- NULL
Churn$account.length <- NULL
Churn$number.vmail.messages <- NULL
Churn$number.customer.service.calls <- NULL
Churn$international.plan <- NULL

###Splitting the Traa=ining and Test Set
library(caTools)
set.seed(101)
split<-sample.split(Churn,SplitRatio = 0.75)
split

training<-subset(Churn,split=="TRUE")
testing<-subset(Churn,split=="FALSE")

##Training the Logistic Regression 
model<-glm(Churn~.,training,family ="binomial")
##Getting the summary details of Logistic Regression
summary(model)

##Predicting the logistic regression
res<-predict(model,testing,type = "response")
res


##Plotting the Region Under the Curve
library(ROCR)
res<-predict(model,training,type = "response")
ROCRpred=prediction(res,training$Churn)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

###Checking the confusion matrix and accuracy
res<-predict(model,testing,type = "response")
table(Actualvalue=testing$Churn,Predictedvalue=res>0.3)

((392 + 18)/(25 + 41 + 392 + 18))


exp(confint(model))


library(randomForest)

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(Churn ~ ., data = training, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, training, type = "class")
  a[i-2] = mean(predValid == training$Churn)
}

# Create a Random Forest model with default parameters
model1 <- randomForest(Churn ~ ., data = training, importance = TRUE)
model2 <- randomForest(Churn ~ ., data = training, importance = TRUE, mtry = 6, ntree = 500)

library(dplyr)

training$voice.mail.plan = NULL
training_test = training %>% select()
training_test = as.factor(training$Churn)
training$Churn = as.factor(training$Churn)

predict0 = predict(model2, training, type = "class" )

table(predict0, training$Churn)

# To check important variables
importance(model2)        
varImpPlot(model2)        


##Importing the required libraries for library for Decision Tree 
library(rpart)
library(caret)
library(e1071)

##Implementing the Decision Tree
model_dt = train(Churn ~ ., data = training, method = "rpart")
model_dt_1 = predict(model_dt, data = training)
table(model_dt_1, training$Churn)

##Plotting the Decison Tree
library(caret)
library(rpart.plot)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

###Doing the Decision Tree and Plotting the plot
dtree_fit <- train(Churn ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)





prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


dtree_fit_gini <- train(Churn ~., data = training, method = "rpart",
                          parms = list(split = "gini"),
                          trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini


prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

library(rpart)
library(rpart.plot)

tree <- rpart(Churn~., data=training, cp=.02)

##Plotting the most optimum decision tree
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)


