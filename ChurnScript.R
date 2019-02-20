#Churn rate project
setwd("C:/Users/Subbu/Documents/Churn")
#Getting working directory
getwd()
#clean the r environment
rm(list=ls())
# loading Libraries
install.packages("dplyr")
install.packages("DataCombine")
library(DataCombine)
install.packages('libcoin', dependencies = T)
install.packages('C50', dependencies = T)
install.packages('caret')
install.packages('e1071', dependencies=TRUE)
library(caret)
library(e1071)
install.packages("randomForest")
install.packages("RRF")
library(randomForest)
library(RRF)
install.packages('inTrees')
library(inTrees)
library("class")
install.packages('corrgram')
install.packages("fpc", dependencies = TRUE)
library("knitr")
library("corrgram")


#Reading test data from csv
dfTest= read.csv("Test_data.csv", header=TRUE)
#Reading train data from csv
dfTrain= read.csv("Train_data.csv", header=TRUE)
#getting attribute classes
sapply(dfTrain,class)
#getting dimension of data set
dim(dfTrain)

###Data Manupulation 
dfTrain$state = as.factor(as.character(dfTrain$state))
dfTrain$international.plan = as.factor(as.character(dfTrain$international.plan))
dfTrain$voice.mail.plan = as.factor(as.character(dfTrain$voice.mail.plan))
dfTrain$phone.number = as.factor(as.character(dfTrain$phone.number))


dfTest$state = as.factor(as.character(dfTest$state))
dfTest$international.plan = as.factor(as.character(dfTest$international.plan))
dfTest$voice.mail.plan = as.factor(as.character(dfTest$voice.mail.plan))
dfTest$phone.number = as.factor(as.character(dfTest$phone.number))

#Missing value analysis for train data

missing_train = data.frame(apply(dfTrain,2,function(x){sum(is.na(x))}))

missing_train$Columns = row.names(missing_train)

names(missing_train)[1] =  "Missing_percentage"
missing_train$Missing_percentage = (missing_train$Missing_percentage/nrow(dfTrain)) * 100
missing_train = missing_train[order(-missing_train$Missing_percentage),]
row.names(missing_train) = NULL
missing_train = missing_train[,c(2,1)]
#Missing value analysis ----No missing values in train


#Missing value analysis for test data

missing_test = data.frame(apply(dfTest,2,function(x){sum(is.na(x))}))

missing_test$Columns = row.names(missing_test)

names(missing_test)[1] =  "Missing_percentage"
missing_test$Missing_percentage = (missing_test$Missing_percentage/nrow(dfTrain)) * 100
missing_test = missing_test[order(-missing_test$Missing_percentage),]
row.names(missing_test) = NULL
missing_test = missing_test[,c(2,1)]
missing_test
#Missing value analysis ----No missing values in test


##Data Manupulation; convert string categories into factor numeric
for(i in 1:ncol(dfTrain)){
  
  if(class(dfTrain[,i]) == 'factor'){
    
    dfTrain[,i] = factor(dfTrain[,i], labels=(1:length(levels(factor(dfTrain[,i])))))
    
  }
}

for(i in 1:ncol(dfTest)){

  if(class(dfTest[,i]) == 'factor'){
    
    dfTest[,i] = factor(dfTest[,i], labels=(1:length(levels(factor(dfTest[,i])))))
    
  }
}

##Data Manupulation; convert string categories into factor numeric


#outlier analysis
numeric_index = sapply(dfTrain,is.numeric) #selecting only numeric

numeric_data = dfTrain[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(dfTrain))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Churn")+
            ggtitle(paste("Box plot of churn for",cnames[i])))
}



##Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,gn16,ncol=4)

for (i in 1:length(cnames))
{
  assign(paste0("tgn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(dfTest))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of churn for",cnames[i])))
}

gridExtra::grid.arrange(tgn1,tgn2,tgn3,ncol=3)
gridExtra::grid.arrange(tgn4,tgn5,tgn6,ncol=3)
gridExtra::grid.arrange(tgn7,tgn8,tgn9,ncol=3)
gridExtra::grid.arrange(tgn10,tgn11,tgn12,ncol=3)
gridExtra::grid.arrange(tgn13,tgn14,tgn15,gn16,ncol=4)


for(i in cnames){
  print(i)
  val = dfTrain[,i][dfTrain[,i] %in% boxplot.stats(dfTrain[,i])$out]

  dfTrain = dfTrain[which(!dfTrain[,i] %in% val),]
}
for(i in cnames){
  print(i)
  val = dfTest[,i][dfTest[,i] %in% boxplot.stats(dfTest[,i])$out]
  
  dfTest = dfTest[which(!dfTest[,i] %in% val),]
}


#outlier analysis ---done removing out liers

#####Feature selection###
corrgram(dfTrain[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(dfTrain,is.factor)
factor_data = dfTrain[,factor_index]
factor_data
for (i in 1:4)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i]), correct = FALSE))
  
}

## Dimension Reduction
dfTrain = subset(dfTrain, 
                         select = -c(total.day.minutes,total.eve.minutes,total.night.minutes, total.intl.minutes, phone.number))

dfTest = subset(dfTest, 
                 select = -c(total.day.minutes,total.eve.minutes,total.night.minutes, total.intl.minutes, phone.number))


#####Feature selection###

#Normalisation

#numeric_index = sapply(dfTrain,is.numeric) #selecting only numeric

#numeric_data = dfTrain[,numeric_index]

#cnames = colnames(numeric_data)
#cnames
#for(i in cnames){
 
#  dfTrain[,i] = (dfTrain[,i] - min(dfTrain[,i]))/
#   (max(dfTrain[,i] - min(dfTrain[,i])))
#}
#for(i in cnames){
  
 # dfTest[,i] = (dfTest[,i] - min(dfTest[,i]))/
  #  (max(dfTest[,i] - min(dfTest[,i])))
#}

sapply(dfTest,class)
dim(dfTest)
#Build decision tree  model
C50_churn_Model = C50::C5.0(Churn ~., dfTrain, trials = 100, rules = TRUE)

summary(C50_churn_Model)
#predict for test cases
C50_churn_Predictions = predict(C50_churn_Model, dfTest[,-16], type = "class")

#Evaluate the performance of classification model
ConfMatrix_churn_C50 = table(dfTest$Churn , C50_churn_Predictions)
confusionMatrix(ConfMatrix_churn_C50)

#False Negative rate
TP= 667
FN=1
FP=27
TN=64

FNR = FN/(FN+TP) 

FNR

FPR = FP/(FP+TN)
FPR

#Accuracy: 96.3%
#FNR: 0.14%
#FPR: 29.67% 

#Build Logistic regression model
Log_churn_model = glm(Churn~., data = dfTrain, family = "binomial")
summary(Log_churn_model)
#predict for test cases
logic_churn_Predict = predict(Log_churn_model, newdata = dfTest, type = "response")
logic_churn_Predict
#convert prob

logic_churn_Predict= ifelse(logic_churn_Predict > 0.5, 2, 1)
logic_churn_Predict
#Evaluate the performance of classification model
ConfMatrix_churn_logic = table(dfTest$Churn , logic_churn_Predict)

ConfMatrix_churn_logic

TP=650
FN=18
FP=58
TN=33

accuracy = (650+33)/(650+18+58+33)
accuracy

FNR = FN/(FN+TP) 

FNR

FPR = FP/(FP+TN)
FPR
#accuracy 89.98
#FNR 2.69%
#FPR 63.7%

#Build Random Forest Model
RF_churn_model = randomForest(Churn ~ ., dfTrain, importance = TRUE, ntree = 500)

#predict for test cases
RF_churn_Predictions = predict(RF_churn_model, dfTest[,-16])

##Evaluate the performance of classification model
ConfMatrix_churn_RF = table(dfTest$Churn, RF_churn_Predictions)
confusionMatrix(ConfMatrix_churn_RF)


TP = 664
FN=4
FP=34
TN=57

FNR = FN/(FN+TP) 

FNR

FPR = FP/(FP+TN)
FPR

#Accuracy 95.62%
#FNR 0.623%
#FPR 28%

#Build KNN model and predictions
knn_Churn_Predictions = knn(dfTrain[,-16], dfTest[,-16],dfTrain$Churn,k=3)

#Evaluate the performance of classification model
ConfMatrix_churn_knn = table(knn_Churn_Predictions, dfTest$Churn)
confusionMatrix(ConfMatrix_churn_knn)

knnAccuracy=sum(diag(ConfMatrix_churn_knn))/ nrow(dfTest)
knnAccuracy

TP= 657
FN=78
FP=11
TN=13

FNR = FN/(FN+TP) 

FNR

FPR = FP/(FP+TN)
FPR


#Accuracy 86.74%
#FNR 7.13%
#FPR 3.41

#Build the naive bayes model
NB_churn_Model = naiveBayes(Churn~., data=dfTrain)

#Predict the test cases
NB_churn_Predictions= predict(NB_churn_Model,dfTest[,-16], type = 'class')

#Evaluate the performance of classification model
ConfMatrix_churn_NB = table(observed=dfTest[,16],predicted = NB_churn_Predictions)

confusionMatrix(ConfMatrix_churn_NB)

TP = 658
FN =10
FP = 63
TN = 28


FNR = FN/(FN+TP) 

FNR

FPR = FP/(FP+TN)
FPR
#Accuracy 87.22%
#FNR 1.49
#FPR 69%
