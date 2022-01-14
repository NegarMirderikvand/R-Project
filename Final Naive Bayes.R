

#Reading data into R
data = read.csv('/Volumes/Leila LaCie/Stat Data Course/Naive Bayes/diabetes.csv')
head(data)
str(data)

#Setting outcome variables as categorical
data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))
levels(data$Outcome)
summary(data)


#--------------------------------------------------Data Imputation------------------------------------------------------------------
#Convert '0' values into NA
data[, 2:7][data[, 2:7] == 0] <- NA


#Use mice package to predict missing values
library(mice)
library(randomForest)
mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values into the main data set
data$Glucose <- mice_complete$Glucose
data$BloodPressure <- mice_complete$BloodPressure
data$SkinThickness <- mice_complete$SkinThickness
data$Insulin<- mice_complete$Insulin
data$BMI <- mice_complete$BMI
#------------------------------------------------------------------------------------------------------------

#Data Visualization
#Visual 1
library(ggplot2)
ggplot(data, aes(Age, colour = Outcome)) + geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

#visual 2
ggplot(data, aes(x=Pregnancies, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Pregnancy Distribution by Outcome")

#visual 3
ggplot(data, aes(x=BMI, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="BMI Distribution by Outcome")

#visual 4
ggplot(data, aes(x=Glucose, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Glucose Distribution by Outcome")

#visual 5
ggplot(data, aes(x=Insulin, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Insulin Distribution by Outcome")


#____________________________Data Modeling
#This stage begins with a process called Data Splicing, wherein the data set is split into two parts:

#Training set: This part of the data set is used to build and train the Machine Learning model.
#Testing set: This part of the data set is used to evaluate the efficiency of the model.

#split data into training and test data sets
library(caret)
indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,] 

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-9]
y = training$Outcome

#Fit the model
library(e1071)
model = train(x,y,'nb')
model

#Plot Variable performance
X <- varImp(model)
plot(X)

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing ) #Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, testing$Outcome )

newcases = read.csv('/Volumes/Leila LaCie/Stat Data Course/Naive Bayes/new cases.csv')
Predictnewcases <- predict(model,newdata = newcases )
Predictnewcases



