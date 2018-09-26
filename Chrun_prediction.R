#setwd("C:/Users/Install/Desktop/marketing-analytics-in-r-statistical-modeling")
library(dplyr)
################################################################################
#Churn Prediction
################################################################################
#Application churn prevention
#QUESTION : What is the main incentive for an online shop to do churn prevention?
#RESPONSE : Convincing existing customers to buy again and stay loyal to the online shop
#COMMENT : Churn prevention is a measure to ensure that customers visit the online shop again.

################################################################################
#Data Discovery
################################################################################
#This dataset is about bank customers and will be used 
#to predict if customers will default on their loan payments.
defaultData <- read.csv("defaultData.csv", stringsAsFactors = TRUE, sep =";")
# Summary of data
summary(defaultData)
# Look at data structure
str(defaultData)
# Analyze the balancedness of dependent variable
ggplot(defaultData,aes(x = PaymentDefault)) +
  geom_histogram(stat = "count")
table(defaultData$PaymentDefault)
################################################################################
#Peculiarities of the dependent variable
################################################################################
#I want to know how you can transform the outcome variable (taking only values of 0 and 1) 
#in order to examine a linear influence of the explanatory variables?
#RESPONSE : By using the logarithmic odds to transform it to a range from - ∞ to + ∞
#COMMENT : Unfortunately this makes interpretability more difficult.

################################################################################
#Model specification and estimation
################################################################################
# Build logistic regression model
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                        age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                        billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                        payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                      family = binomial, data = defaultData)
# Take a look at the model
summary(logitModelFull)
# Take a look at the odds
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp
################################################################################
#Statistical significance
################################################################################
#Everyone is talking about statistical significance, but do you know the exact meaning of it?
#What is the correct interpretation of a p value equal to 0.05 for a variable's coefficient ?
#H0: The influence of this variable on the payment default of a customer is equal to zero.
#RESPONSE ;The probability of finding this coefficient's value is only 5%, given that our null hypothesis (the respective coefficient is equal to zero) is true.

################################################################################
#Model specification
################################################################################
#stepAIC() function gives back a reduced model
library(MASS)
#Make use of the stepAIC()
#Set trace = 0, as you do not want to get an output for the whole model selection process
#Build the new model
logitModelNew <- stepAIC(logitModelFull,trace = 0) 

#Look at the model
summary(logitModelNew) 

# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit
#Remember the 'stepAIC()' function, you might need it!

#Pseuf R2 stats
library(descr)
LogRegR2(logitModelNew)
# x > 0.2 Reasonalble ; > 0.4 Good ; > 0.5 Very Good
################################################################################
#In-sample fit full model
################################################################################
#You now want to know how your model performs by calculating the accuracy
#you first need a confusion matrix
# Make predictions using the full Model
defaultData$predFull <- predict(logitModelFull, type = "response", na.action = na.exclude)

library(SDMTools)
# Construct the in-sample confusion matrix
confMatrixModelFull <- confusion.matrix(defaultData$PaymentDefault, defaultData$predFull, threshold = 0.5)
confMatrixModelFull

# Calculate the accuracy for the full Model
accuracyFull <- sum(diag(confMatrixModelFull)) / sum(confMatrixModelFull)
accuracyFull
#Acc = 0.80 
#In the next step, do it for the other one as well!
################################################################################
#In-sample fit restricted model
################################################################################
#When comparing the values of the different models with each other
#always choose the model with less explanatory variables
# Calculate the accuracy for 'logitModelNew'
# Make prediction
defaultData$predNew <- predict(logitModelNew, type = "response", na.action = na.exclude)

# Construct the in-sample confusion matrix
confMatrixModelNew <- confusion.matrix(defaultData$PaymentDefault,defaultData$predNew, threshold = 0.5)
confMatrixModelNew

# Calculate the accuracy...
accuracyNew <- sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew)
accuracyNew

# and compare it to the full model's accuracy
accuracyFull

#RESULT : As the accuracy values are approximately the same, let's continue with the smaller model logitModelNew

################################################################################
#Finding the optimal threshold
################################################################################
library(SDMTools)
# Prepare data frame with threshold values and empty payoff column
payoffMatrix <- data.frame(threshold = seq(from = 0.1, to = 0.5, by = 0.1),
                           payoff = NA) 
payoffMatrix 

for(i in 1:length(payoffMatrix$threshold)) {
  # Calculate confusion matrix with varying threshold
  confMatrix <- confusion.matrix(defaultData$PaymentDefault,
                                 defaultData$predNew, 
                                 threshold = payoffMatrix$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffMatrix$payoff[i] <- confMatrix[1,1]*250 + confMatrix[1,2]*(-1000)
}
payoffMatrix
# the choice of the threshold is essential for your results
#payoffMatrix that contains a column with the thresholds 0.1,0.2,...,0.5 for you
#calculate the optimal threshold for ModelNew
#You could see that the optimal threshold is 0.3.

################################################################################
#Danger of overfitting
################################################################################
#QUESTION :some insights about in-sample fitting and the problem of overfitting
#RESPONSE : The model is highly tailored to the given data and not suited for explaining new data.
#COMMENT : That is a problem that you should be aware of.

################################################################################
#Assessing out-of-sample model fit
###############################################################################
#The in-sample accuracy - using the optimal threshold of 0.3 - is 0.7922901. 
#Make sure you understand if there is overfitting.

# Split data in train and test set
set.seed(534381) 
defaultData$isTrain <- rbinom(nrow(defaultData), 1, 0.66)
train <- subset(defaultData, defaultData$isTrain == 1)
test <- subset(defaultData, defaultData$isTrain  == 0)

logitTrainNew <- glm(formulaLogit, family = binomial, data = train) # Modeling
test$predNew <- predict(logitTrainNew, type = "response", newdata = test) # Predictions

# Out-of-sample confusion matrix and accuracy
confMatrixModelNew <- confusion.matrix(test$PaymentDefault, test$predNew, threshold = 0.3) 
sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew) # Compare this value to the in-sample accuracy
#Accuracy = 0.7797764
#In case you experience overfitting in the future you would have to go back to modeling and build smaller models.

################################################################################
#Cross validation
###############################################################################
#Cross validation is a clever method to avoid overfitting
library(boot)
# Accuracy function
costAcc <- function(r, pi = 0) {
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# Cross validated accuracy for logitModelNew
set.seed(534381)
cv.glm(defaultData, logitModelNew, cost = costAcc, K = 6)$delta[1]
#Accuracy 0.78
