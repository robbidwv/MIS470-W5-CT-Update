#Clear out the environment from my previous work
rm(list = ls()) 

#Show current run date/time
date()

getwd()

#Load the downloaded data table into telco and display a brief summary of the data
telco <- read.csv("/Users/robertdawson/Downloads/MIS470Telcocustomer (1).csv", stringsAsFactors = TRUE)
str(telco)

# uncomment install.packages("caret") if already installed
# install.packages("caret")
library(caret)

# Prep data. Partition data into training and testing partitions
intrain<- createDataPartition(telco$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- telco[intrain,]
testing<- telco[-intrain,]

# Create a Logistic Model for Churn with training data
LogModel <-glm(Churn ~ .,family=binomial(link="logit"),data=training)
summary(LogModel)

# Evaluate model with testing data
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)

# TO DO
# 1. calculate and print out logistic model accuracy using misClasificError variable
# 2. Display a confusion matric using the table() function
# 3. Create and evaluate the performance of 3 logistic models 
#    using the three (3) most significant predictors 
#    (either individual or collectively)
# 

#Calculate and print out logistic model accuracy using misClasificError variable
accuracy <- 1 - misClasificError
print(paste('Accuracy', accuracy))

# Display a confusion matrix using the table() function
confusion_matrix <- table(Predicted = fitted.results, Actual = testing$Churn)
print(confusion_matrix)

# Model 1: Using Contract alone
#Create model using glm and store in LogModel1
LogModel1 <- glm(Churn ~ Contract, family=binomial(link="logit"), data=training)
#Display a summary of the model that was created
summary(LogModel1)

#Store the predicted values 
fitted.results1 <- predict(LogModel1, newdata=testing, type='response')

#If the values are greater than 0.5 then record them as a 1, else a zero
fitted.results1 <- ifelse(fitted.results1 > 0.5, 1, 0)

#Calculate 

accuracy1 <- mean(fitted.results1 == testing$Churn)
print(paste('Model 1 Accuracy', accuracy1))

# Model 2: Using Contract & Paperless Billing
#Create model using glm and store in LogModel2
LogModel2 <- glm(Churn ~ Contract + PaperlessBilling, family=binomial(link="logit"), data=training)
#Display a summary of the model that was created
summary(LogModel2)
#Store the predicted values 
fitted.results2 <- predict(LogModel2, newdata=testing, type='response')
#If the values are greater than 0.5 then record them as a 1, else a zero
fitted.results2 <- ifelse(fitted.results2 > 0.5, 1, 0)
accuracy2 <- mean(fitted.results2 == testing$Churn)
print(paste('Model 2 Accuracy', accuracy2))

# Model 3: Usng Contract & Paperless Billing & Tenure Group
#Create model using glm and store in LogModel3
LogModel3 <- glm(Churn ~ Contract + PaperlessBilling + tenure_group, family=binomial(link="logit"), data=training)
#Display a summary of the model that was created
summary(LogModel3)
#Store the predicted values 
fitted.results3 <- predict(LogModel3, newdata=testing, type='response')
#If the values are greater than 0.5 then record them as a 1, else a zero
fitted.results3 <- ifelse(fitted.results3 > 0.5, 1, 0)
accuracy3 <- mean(fitted.results3 == testing$Churn)
print(paste('Model 3 Accuracy', accuracy3))


